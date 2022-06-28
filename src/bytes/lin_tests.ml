
module BConfGet = struct (*this hangs*)
  type t = Bytes.t
  let init () = Stdlib.Bytes.make 42 '0'
  let cleanup _ = ()

  open Lin_api
  let api =
    [
      val_ "Bytes.get" Bytes.get (t @-> int @-> returning_or_exc char)
    ]
end


module BConfSet = struct (*this also hangs*)
  type t = Bytes.t
  let init () = Stdlib.Bytes.make 42 '0'
  let cleanup _ = ()

  open Lin_api
  let api =
    [
      val_ "Bytes.set" Bytes.set
        (t @-> int @-> char @-> returning_or_exc unit)
    ]
end


module BT = Lin_api.Make(BConfGet)
;;
Util.set_ci_printing ()
;;
QCheck_runner.run_tests_main [
  BT.lin_test `Domain ~count:1000 ~name:"Byte test with domains";
  BT.lin_test `Thread ~count:1000 ~name:"Byte test with threads";
]
