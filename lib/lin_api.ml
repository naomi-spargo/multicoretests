(* Type-representing values *)

type constructible = |
type deconstructible = |

type combinable
type noncombinable

type (_,_,_,_) ty =
  | Gen : 'a QCheck.arbitrary -> ('a, constructible, 's, combinable) ty
  | Deconstr : ('a -> string) * ('a -> 'a -> bool) -> ('a, deconstructible, 's, combinable) ty
  | GenDeconstr : 'a QCheck.arbitrary * ('a -> string) * ('a -> 'a -> bool) -> ('a, 'c, 's, combinable) ty
  | State : ('s, constructible, 's, noncombinable) ty

let wrap_print : type a. (a -> string) option -> (a -> string) = function
  | None -> fun _ -> "???"
  | Some f -> f

let print_char c   = Printf.sprintf "%C" c
let print_string s = Printf.sprintf "%S" s
let unit =           GenDeconstr (QCheck.unit,           QCheck.Print.unit, (=))
let bool =           GenDeconstr (QCheck.bool,           QCheck.Print.bool, (=))
let nat_small =      GenDeconstr (QCheck.small_nat,      QCheck.Print.int,  (=))
let int =            GenDeconstr (QCheck.int,            QCheck.Print.int,  (=))
let int_small =      GenDeconstr (QCheck.small_int,      QCheck.Print.int,  (=))
let char =           GenDeconstr (QCheck.char,           print_char,        (=))
let char_printable = GenDeconstr (QCheck.printable_char, print_char,        (=))
let string =         GenDeconstr (QCheck.string,         print_string,      String.equal)
let pos_int =        GenDeconstr (QCheck.pos_int,        QCheck.Print.int,  (=))
let small_nat =      GenDeconstr (QCheck.small_nat,      QCheck.Print.int,  (=))
let int32 =          GenDeconstr (QCheck.int32,          Int32.to_string,   Int32.equal)
let int64 =          GenDeconstr (QCheck.int64,          Int64.to_string,   Int64.equal)
let float =          GenDeconstr (QCheck.float,          Float.to_string,   Float.equal)

let option : type a c s. ?ratio:float -> (a, c, s, combinable) ty -> (a option, c, s, combinable) ty =
  fun ?ratio ty ->
  match ty with
  | Gen arb -> Gen (QCheck.option ?ratio arb)
  | GenDeconstr (arb, print, eq) -> GenDeconstr (QCheck.option ?ratio arb, QCheck.Print.option print, Option.equal eq)
  | Deconstr (print, eq) -> Deconstr (QCheck.Print.option print, Option.equal eq)
let opt = option

let list : type a c s. (a, c, s, combinable) ty -> (a list, c, s, combinable) ty =
  fun ty -> match ty with
  | Gen arb -> Gen (QCheck.list arb)
  | GenDeconstr (arb, print, eq) -> GenDeconstr (QCheck.list arb, QCheck.Print.list print, List.equal eq)
  | Deconstr (print, eq) -> Deconstr (QCheck.Print.list print, List.equal eq)


let state = State
let t = state

let int_bound bound =
  let arb = QCheck.int_bound bound in
  GenDeconstr (arb, QCheck.Print.int, (=))

let print_result print_ok print_err = function
  | Ok x    -> Printf.sprintf "Ok (%s)" (print_ok x)
  | Error y -> Printf.sprintf "Error (%s)" (print_err y)

let or_exn ty = match ty with
  | GenDeconstr (_, print, eq) ->
      Deconstr (print_result print Printexc.to_string, Result.equal ~ok:eq ~error:(=))
  | Deconstr (print, eq) ->
      Deconstr (print_result print Printexc.to_string, Result.equal ~ok:eq ~error:(=))

let print : type a c s comb. (a, c, s, comb) ty -> a -> string = fun ty value ->
  match ty with
  | Gen arb -> wrap_print arb.QCheck.print value
  | Deconstr (print,_) -> print value
  | GenDeconstr (_,print,_) -> print value
  | State -> "t"

module Fun = struct
  (* Function type, number of arguments (unary encoding), state type *)
  type (_,_,_) fn =
    | Ret :        ('a, deconstructible, 's, combinable) ty -> ('a, 'a, 's) fn
    | Ret_or_exc : ('a, deconstructible, 's, combinable) ty -> ('a, ('a,exn) result, 's) fn
    | Ret_ignore : ('a, _, 's, combinable) ty -> ('a, unit, 's) fn
    | Ret_ignore_or_exc : ('a, _, 's, combinable) ty -> ('a, (unit,exn) result, 's) fn
    | Fn : ('a, constructible, 's, _) ty * ('b, 'r, 's) fn -> ('a -> 'b, 'r, 's) fn
end

let returning a = Fun.Ret a
let returning_or_exc a = Fun.Ret_or_exc a
let returning_ a = Fun.Ret_ignore a
let returning_or_exc_ a = Fun.Ret_ignore_or_exc a
let (@->) a b = Fun.Fn (a,b)

type _ elem =
  | Elem : string * ('ftyp, 'r, 's) Fun.fn * 'ftyp -> 's elem

type 's api = (int * 's elem) list

let val_ name value fntyp =
  (1, Elem (name, fntyp, value))

let val_with_freq freq name value fntyp =
  (freq, Elem (name, fntyp, value))

module type ApiSpec = sig
  type t
  val init : unit -> t
  val cleanup : t -> unit
  val api : (int * t elem) list
end

module MakeCmd (ApiSpec : ApiSpec) : Lin.CmdSpec = struct

  type t = ApiSpec.t

  let init = ApiSpec.init
  let cleanup = ApiSpec.cleanup

  (* Typed argument list and return type descriptor *)
  module Args = struct
    type (_,_) args =
    | Ret : ('a, deconstructible, t, _) ty -> ('a,'a) args
    | Ret_or_exc : ('a, deconstructible, t, _) ty -> ('a, ('a,exn) result) args
    | Ret_ignore : ('a, _, t, _) ty -> ('a, unit) args
    | Ret_ignore_or_exc : ('a, _, t, _) ty -> ('a, (unit,exn) result) args
    | Fn : 'a * ('b,'r) args -> ('a -> 'b, 'r) args
    | FnState : ('b,'r) args -> (t -> 'b, 'r) args
  end

  (* Operation name, typed argument list, return type descriptor, printer, function *)
  type cmd =
    Cmd :
      string *
      ('ftyp, 'r) Args.args *
      ('r, deconstructible, t, _) ty *
      (('ftyp, 'r) Args.args -> string) *
      'ftyp
      -> cmd

  type res =
    Res : ('a, deconstructible, t, _) ty * 'a -> res

  (* Function to generate typed list of arguments from a function description.
     The printer can be generated independently. *)
  let rec gen_args_of_desc
    : type a r. (a, r, t) Fun.fn -> ((a, r) Args.args) QCheck.Gen.t =
    fun fdesc ->
      let open QCheck.Gen in
      match fdesc with
      | Fun.Ret ty -> return @@ Args.Ret ty
      | Fun.Ret_or_exc ty -> return @@ Args.Ret_or_exc ty
      | Fun.Ret_ignore_or_exc ty -> return @@ Args.Ret_ignore_or_exc ty
      | Fun.Ret_ignore ty -> return @@ Args.Ret_ignore ty
      | Fun.(Fn (State, fdesc_rem)) ->
          let* args_rem = gen_args_of_desc fdesc_rem in
          return @@ Args.FnState args_rem
      | Fun.(Fn ((Gen arg_arb | GenDeconstr (arg_arb, _, _)), fdesc_rem)) ->
          let* arg = arg_arb.gen in
          let* args_rem = gen_args_of_desc fdesc_rem in
          return @@ Args.Fn (arg, args_rem)

  let rec ret_type
    : type a r. (a,r,t) Fun.fn -> (r, deconstructible, t, _) ty
    = fun fdesc ->
      match fdesc with
      | Fun.Ret ty -> ty
      | Fun.Ret_or_exc ty -> or_exn ty
      | Fun.Ret_ignore _ -> unit
      | Fun.Ret_ignore_or_exc _ -> or_exn unit
      | Fun.Fn (_, fdesc_rem) -> ret_type fdesc_rem

  let rec show_args : type a r. (a,r,t) Fun.fn -> (a,r) Args.args -> string = fun fdesc args ->
    match fdesc,args with
    | _, Args.(Ret _ | Ret_or_exc _ | Ret_ignore _ | Ret_ignore_or_exc _) -> ""
    | Fun.(Fn (State, fdesc_rem)), Args.(FnState args_rem) ->
        "t " ^ show_args fdesc_rem args_rem
    | Fun.(Fn ((GenDeconstr _ | Gen _ as ty), fdesc_rem)), Args.(Fn (value, args_rem)) ->
        print ty value ^ " " ^ show_args fdesc_rem args_rem
    | Fun.(Fn (State, _)), Args.(Fn _)
    | Fun.(Fn ((Gen _ | GenDeconstr _), _)), Args.(FnState _)  ->
        assert false
    | Fun.(Ret _ | Ret_or_exc _ | Ret_ignore _ | Ret_ignore_or_exc _), Args.(Fn _ | FnState _) ->
        assert false

  let gen_printer : type a r. string -> (a,r,t) Fun.fn -> (a,r) Args.args -> string = fun name fdesc args ->
    name ^ " " ^ show_args fdesc args

  let api =
    List.map (fun (wgt, Elem (name, fdesc, f)) ->
        let rty = ret_type fdesc in
        let open QCheck.Gen in
        (wgt, gen_args_of_desc fdesc >>= fun args ->
         let print = gen_printer name fdesc in
         return (Cmd (name, args, rty, print, f)))) ApiSpec.api

  let gen_cmd : cmd QCheck.Gen.t = QCheck.Gen.frequency api

  let show_cmd (Cmd (_,args,_,print,_)) = print args

  let shrink_cmd = QCheck.Shrink.nil (*FIXME*)

  (* Unsafe if called on two [res] whose internal values are of different
     types. *)
  let equal_res (Res (deconstr, v1)) (Res (_, v2)) =
    match deconstr with
    | Deconstr (_, eq) -> eq v1 (Obj.magic v2)
    | GenDeconstr (_, _, eq) -> eq v1 (Obj.magic v2)

  let show_res (Res (deconstr, value)) =
    match deconstr with
    | Deconstr (print, _) -> print value
    | GenDeconstr (_, print, _) -> print value

  let rec apply_f
    : type a r. a -> (a, r) Args.args -> t -> r = fun f args state ->
      match args with
      | Ret _ ->
          (* This happens only if there was a non-function value in the API,
             which I'm not sure makes sense *)
          raise (Invalid_argument "apply_f")
      | Ret_or_exc _ ->
          (* This happens only if there was a non-function value in the API,
             which I'm not sure makes sense *)
          raise (Invalid_argument "apply_f")
      | Ret_ignore _ ->
          (* This happens only if there was a non-function value in the API,
             which I'm not sure makes sense *)
          raise (Invalid_argument "apply_f")
      | Ret_ignore_or_exc _ ->
          (* This happens only if there was a non-function value in the API,
             which I'm not sure makes sense *)
          raise (Invalid_argument "apply_f")
      | FnState (Ret _) ->
          f state
      | FnState (Ret_or_exc _) ->
          begin
            try Ok (f state)
            with e -> Error e
          end
      | FnState (Ret_ignore _) ->
          ignore (f state)
      | FnState (Ret_ignore_or_exc _) ->
          begin
            try Ok (ignore @@ f state)
            with e -> Error e
          end
      | FnState (Fn _ as rem) ->
          apply_f (f state) rem state
      | FnState (FnState _ as rem) ->
          apply_f (f state) rem state
      | Fn (arg, Ret _) ->
          f arg
      | Fn (arg, Ret_or_exc _) ->
          begin
            try Ok (f arg)
            with e -> Error e
          end
      | Fn (arg, Ret_ignore _) ->
          ignore @@ f arg
      | Fn (arg, Ret_ignore_or_exc _) ->
          begin
            try Ok (ignore @@ f arg)
            with e -> Error e
          end
      | Fn (arg, (Fn _ as rem)) ->
          apply_f (f arg) rem state
      | Fn (arg, (FnState _ as rem)) ->
          apply_f (f arg) rem state

  let run cmd state =
    let Cmd (_, args, rty, _, f) = cmd in
    Res (rty, apply_f f args state)

end

module Make (ApiSpec : ApiSpec) = Lin.Make (MakeCmd (ApiSpec))
