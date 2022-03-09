open QCheck

(** parallel linearization tests of Lazy *)

(* a simple work item, from ocaml/testsuite/tests/misc/takc.ml *)
let rec tak x y z =
  if x > y then tak (tak (x-1) y z) (tak (y-1) z x) (tak (z-1) x y)
           else z

let work () =
  let r = ref 0 in
  for _ = 1 to 100 do
    r := !r + tak 18 12 6;
    (*assert (7 = tak 18 12 6);*)
  done;
  !r

(*
module Lazy :
  sig
    type 'a t = 'a CamlinternalLazy.t
    exception Undefined
    external force : 'a t -> 'a = "%lazy_force"
    val map : ('a -> 'b) -> 'a t -> 'b t
    val is_val : 'a t -> bool
    val from_val : 'a -> 'a t
    val map_val : ('a -> 'b) -> 'a t -> 'b t
    val from_fun : (unit -> 'a) -> 'a t
    val force_val : 'a t -> 'a
  end
*)

module LBase =
struct
  type cmd =
    | Force
    | Force_val
    | Is_val
    | Map of int_fun
    | Map_val of int_fun [@@deriving qcheck, show { with_path = false }]
  and int_fun = (int -> int) fun_ [@gen (fun1 Observable.int small_nat).gen][@printer fun fmt f -> fprintf fmt "%s" (Fn.print f)]

  type t = int Lazy.t

  let cleanup _ = ()

  type res =
    | RForce of int
    | RForce_val of int
    | RIs_val of bool
    | RMap of int
    | RMap_val of int [@@deriving show { with_path = false }]

  let run c l = match c with
    | Force               -> RForce (Lazy.force l)
    | Force_val           -> RForce_val (Lazy.force_val l)
    | Is_val              -> RIs_val (Lazy.is_val l)
    | Map (Fun (_,f))     -> RMap (Lazy.force (Lazy.map f l)) (*we force the "new lazy"*)
    | Map_val (Fun (_,f)) -> RMap_val (Lazy.force (Lazy.map_val f l)) (*we force the "new lazy"*)
end


module LTlazy    = Lin.Make(struct
    include LBase
    let init () = lazy (work ())
  end)
module LTfromval = Lin.Make(struct
    include LBase
    let init () = Lazy.from_val 42
  end)
module LTfromfun = Lin.Make(struct
    include LBase
    let init () = Lazy.from_fun work
  end)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main
  (let count = 100 in
   [LTlazy.lin_test       `Domain ~count ~name:"lazy test";
    LTfromval.lin_test    `Domain ~count ~name:"lazy test from_val";
    LTfromfun.lin_test    `Domain ~count ~name:"lazy test from_fun";
   ])