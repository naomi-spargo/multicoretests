open QCheck
open STM


(*Test of the Bytes module. Expected (and does) fail some parallel tests*)

module CConf =
struct
  type cmd =
    | Length
    | Get of int
    | Set of int * char
    | Fill of int * int * char (*pos, len, contents*)
    | Sub_string of int * int (*pos, len*)
    | Blit of int * int * int (*src_pos, dst_pos, len*)
    | Blit_string of string * int * int * int
    | Index_from of int * char  [@@deriving show {with_path = false}]
  (*Blit (src_pos, dst_pos, len) reuses the sut for the src and the dest.
  ie, if b is the sut, it represents Bytes.blit b src_pos b dst_pos len *)

type state = char List.t

type sut = bytes

let arb_cmd (s : state) : cmd arbitrary =
  let bind_keep_input (g : 'a Gen.t) (f: 'a -> 'b Gen.t): ('a * 'b) Gen.t =
    Gen.(g >>= (fun a -> map (fun b -> (a, b)) (f a))) in
  let make_index _ : int Gen.t = (*skews towards picking a nat <= len*)
    (* Gen.(frequency [(len, int_bound len); (1, nat)])*) Gen.int in
  let index = make_index (List.length s) in
  let contents : char Gen.t = (*skews towards picking a char in s*)
    Gen.(if (Int.equal (List.length s) 0) then char
         else frequency [(List.length s, oneofl s); (1, char)]) in
  QCheck.make ~print:show_cmd Gen.(oneof [
      return Length;
      map (fun n -> Get n) index;
      map2 (fun n -> fun ch -> Set (n, ch)) index char;
      map3 (fun pos -> fun len -> fun ch ->
          Fill (pos, len, ch)) index small_nat char;
      map2 (fun pos -> fun len -> Sub_string (pos, len)) index small_nat;
      map3 (fun src_pos -> fun dst_pos -> fun len ->
          Blit (src_pos, dst_pos, len)) index index small_nat;
      map3 (fun (src, src_pos) -> fun dst_pos -> fun len ->
          Blit_string (src, src_pos, dst_pos, len))
        (bind_keep_input small_string (fun s -> make_index (String.length s)))
        index small_nat;
      (*skews src_pos to be a valid index of src *)
      map2 (fun i-> fun ch -> Index_from (i, ch)) index contents
    ])

let init_state  = List.init 42 (fun _ -> '0')
let init_sut () = Stdlib.Bytes.make 42 '0'
let cleanup _ = ()

let is_range (limit: int) (start : int) (len: int) =
  start >= 0 && len >= 0 && start + len <= limit

let next_state c s = match c with
  | Length -> s
  | Get _ -> s
  | Set (n, ch) ->
    if (n >= List.length s) then s
    else List.mapi (fun i -> fun v -> if (Int.equal i n) then ch
                     else v) s
  | Fill (pos, len, ch) ->
    if (is_range (List.length s) pos len)
    then List.mapi (fun i -> fun v -> if (pos <= i && i < pos + len) then ch
                     else v) s
    else s
  | Sub_string _ -> s 
  | Blit (src_pos, dst_pos, len) ->
    if (is_range (List.length s) src_pos len &&
        is_range (List.length s) dst_pos len)
    then List.mapi (fun i v ->
        if (dst_pos <= i && i < dst_pos + len)
        then let offset = i - dst_pos in List.nth s (src_pos + offset)
        else v) s
    else s
  | Blit_string (src, src_pos, dst_pos, len) ->
    if (is_range (String.length src) src_pos len &&
        is_range (List.length s) dst_pos len)
    then List.mapi (fun i  -> fun v ->
        if (dst_pos<= i && i < dst_pos + len)
        then let offset = i - dst_pos in String.get src (src_pos + offset)
        else v) s
    else s
  | Index_from (_, _) -> s 

let precond _ _ = true

let run c bytes : res = match c with 
  | Length -> Res(int, Bytes.length bytes)
  | Get n -> Res(result char exn, protect (Bytes.get bytes) n)
  | Set (n, ch) -> Res(result unit exn, protect (Bytes.set bytes n) ch)
  | Fill (pos, len, ch) ->
    Res(result unit exn, protect (Bytes.fill bytes pos len) ch)
  | Sub_string (pos, len) ->
    Res(result string exn, protect (Bytes.sub_string bytes pos) len)
  | Blit (src_pos, dst_pos, len) ->
    Res(result unit exn, protect (Bytes.blit bytes src_pos bytes dst_pos) len)
  | Blit_string (src, src_pos, dst_pos, len) ->
    Res(result unit exn,
        protect (Bytes.blit_string src src_pos bytes dst_pos) len)
  | Index_from (i, ch) ->
    Res (result int exn, protect (Bytes.index_from bytes i) ch)

let postcond c (s : state) r =
  let index_from (s: state) (i : int) (ch : char) =
    List.find (fun (j, v) -> j>= i && Char.equal v ch)
     (List.mapi (fun j -> fun v -> (j, v)) s) |> fst
  in
  match c, r with
  | Length, Res((Int, _), r) -> r = List.length s
  | Get n, Res((Result (Char, Exn), _), r) ->
    r = (try Ok (List.nth s n)
         with _ -> Error (Invalid_argument "index out of bounds"))
  | Set (n, _), Res((Result (Unit, Exn), _), r) ->
    r = if (n >= List.length s)
    then Error (Invalid_argument "index out of bounds")
    else Ok ()
  | Fill (pos, len, _), Res((Result (Unit, Exn), _), r) ->
    r = if (is_range (List.length s) pos len) then Ok ()
    else Error (Invalid_argument "String.fill / Bytes.fill")
  | Sub_string (pos, len), Res ((Result (String, Exn), _), r) ->
    r = if (is_range (List.length s) pos len)
    then Ok (String.init len (fun i -> List.nth s (i + pos)))
    else Error (Invalid_argument "String.sub / Bytes.sub")
  | Blit (src_pos, dst_pos, len), Res((Result (Unit, Exn), _), r) ->
    r = if (is_range (List.length s) src_pos len &&
            is_range (List.length s) dst_pos len)
    then Ok ()
    else Error (Invalid_argument "Bytes.blit")
  | Blit_string (src, src_pos, dst_pos, len), Res ((Result (Unit, Exn), _), r) ->  
    r = if (is_range (String.length src) src_pos len &&
            is_range (List.length s) dst_pos len) then Ok ()
    else Error (Invalid_argument "String.blit / Bytes.blit_string")
  | Index_from (i, ch), Res ((Result (Int, Exn), _), r) ->
    r = if (i > List.length s)
    then Error (Invalid_argument "String.index_from / Bytes.index_from")
    else protect (index_from s i) ch
  | _ -> false
end

module BT = STM.Make(CConf);;
QCheck_runner.run_tests_main
  (let count,name = 1000,"bytes test" in
   [BT.agree_test     ~count ~name;
   BT.agree_test_par ~count ~name
   ])


