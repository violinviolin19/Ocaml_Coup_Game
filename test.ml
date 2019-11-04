open OUnit2


(* of course add whatever code you want *)

let tests = [
]

let suite = "search test suite" >::: tests

let _ = run_test_tt_main suite