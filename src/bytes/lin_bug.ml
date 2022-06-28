open QCheck


type cmd = Bytes_test_get of  int


let bytes_test_get i = Bytes_test_get i

let run c sut = match c with (Bytes_test_get i) ->
try Ok (Bytes.get sut i) with exn -> Error exn 

let equal_res = Result.equal ~ok:Char.equal ~error:(=)

let cleanup _ = ()

let interp sut cs =
  let cs_arr = Array.of_list cs in
  let res_arr = Array.map (fun c -> Domain.cpu_relax(); run c sut) cs_arr in
  List.combine cs (Array.to_list res_arr)

let init _ = Bytes.make 42 '0'

let interp_plain sut cs = List.map (fun c -> (c, run c sut)) cs

let rec check_seq_cons pref cs1 cs2 seq_sut seq_trace = match pref with
    | (c,res)::pref' ->
        if equal_res res (run c seq_sut)
        then check_seq_cons pref' cs1 cs2 seq_sut (c::seq_trace)
        else (cleanup seq_sut; false)
    (* Invariant: call cleanup immediately after mismatch  *)
    | [] -> match cs1,cs2 with
            | [],[] -> cleanup seq_sut; true
            | [],(c2,res2)::cs2' ->
                if equal_res res2 (run c2 seq_sut)
                then check_seq_cons pref cs1 cs2' seq_sut (c2::seq_trace)
                else (cleanup seq_sut; false)
            | (c1,res1)::cs1',[] ->
                if equal_res res1 (run c1 seq_sut)
                then check_seq_cons pref cs1' cs2 seq_sut (c1::seq_trace)
                else (cleanup seq_sut; false)
            | (c1,res1)::cs1',(c2,res2)::cs2' ->
                (if equal_res res1 (run c1 seq_sut)
                 then check_seq_cons pref cs1' cs2 seq_sut (c1::seq_trace)
                 else (cleanup seq_sut; false))
                ||
                (* rerun to get seq_sut to same cmd branching point *)
                (let seq_sut' = init () in
                 let _ = interp_plain seq_sut' (List.rev seq_trace) in
                 if equal_res res2 (run c2 seq_sut')
                 then check_seq_cons pref cs1 cs2' seq_sut' (c2::seq_trace)
                 else (cleanup seq_sut'; false))


  let lin_prop_domain (seq_pref,cmds1,cmds2) =
    let init _ = Bytes.make 42 '0' in
    let sut = init () in
    let pref_obs = interp sut seq_pref in
    let wait = Atomic.make true in
    let dom1 = Domain.spawn (fun () -> while Atomic.get wait do Domain.cpu_relax() done; try Ok (interp sut cmds1) with exn -> Error exn) in
    let dom2 = Domain.spawn (fun () -> Atomic.set wait false; try Ok (interp sut cmds2) with exn -> Error exn) in
    let obs1 = Domain.join dom1 in
    let _ = Printf.printf("dom1 done\n%!") in
    let obs2 = Domain.join dom2 in
    let _ = Printf.printf("dom2 done\n%!") in
    let obs1 = match obs1 with Ok v -> v | Error exn -> raise exn in
    let obs2 = match obs2 with Ok v -> v | Error exn -> raise exn in
    let seq_sut = init () in
    check_seq_cons pref_obs obs1 obs2 seq_sut []
    (*  || Test.fail_reportf "  Results incompatible with sequential execution\n\n%s"
         @@ print_triple_vertical ~fig_indent:5 ~res_width:35
              (fun (c,r) -> Printf.sprintf "%s : %s" (show_cmd c) (Spec.show_res r))
              (pref_obs,obs1,obs2) *)

let triple =
  ([bytes_test_get 1209912816744509847 ; bytes_test_get 348934393235267140 ; bytes_test_get (-2634067457854331518) ; bytes_test_get (-4314343047360187018) ; bytes_test_get 410294088567392749 ; bytes_test_get 1766872306621795999 ; bytes_test_get 2316970519858334360 ; bytes_test_get 420451772664864812 ; bytes_test_get 290514961762528748 ; bytes_test_get 2891275269668022032 ], [bytes_test_get 915270716371456985 ; bytes_test_get (-1047079483732106082) ; bytes_test_get 4028426743475345206 ; bytes_test_get 2611978685954034003 ; bytes_test_get (-4450030417047947286) ; bytes_test_get 632453786650793582 ; bytes_test_get (-4186614520566403597) ; bytes_test_get
 (-2329277711986324351) ; bytes_test_get 1302613779686895045 ; bytes_test_get 1868438929971426204 ; bytes_test_get (-3071380410464670091) ], [bytes_test_get 2863181644828011918 ; bytes_test_get 2387533253582630065 ; bytes_test_get 2267678113500861224 ; bytes_test_get 2846933842175201110 ; bytes_test_get (-2108617633296180669) ; bytes_test_get 695564165279308374 ; bytes_test_get 732707760584111550 ; bytes_test_get 3658320201574723010 ; bytes_test_get 3657888938457111857 ; bytes_test_get 3831880974317787888 ; bytes_test_get 920322010625922054 ])

let rep_count = 50
let test = Test.make ~count:100000 ~retries:3 ~name:("Linearizable  with Domain")
   unit (fun _t ->
        (* Printf.printf("start\n%!"); *)
                  let out = lin_prop_domain triple in
                   Printf.printf("end\n%!"); out  
        )

let _ = QCheck_runner.run_tests ~verbose:true [test]
