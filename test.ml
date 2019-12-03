open OUnit2


(* of course add whatever code you want *)

let tests = [
]

let suite = "search test suite" >::: tests

let board_tests = [
  (* stuff below copied over to help with syntax *)
  (* "check empty listDict" >:: (fun _ ->
  assert_equal 0 (IID.size IID.empty));
  "fill" >:: (fun _ -> assert_equal 1 (IID.size (IID.insert 5 6 test_dict))); *)
]
let _ = run_test_tt_main suite