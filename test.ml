open OUnit2
open Board
open Deck

(* of course add whatever code you want *)

let tests = [
]

let rec length lst accu = match lst with
  | [] -> accu
  | h :: t -> length t (accu + 1)
let test_deck = init_deck ()
let test_board = init_board test_deck 5
let tests = [
  (* stuff below copied over to help with syntax *)
  (* "check empty listDict" >:: (fun _ ->
  assert_equal 0 (IID.size IID.empty));
  "fill" >:: (fun _ -> assert_equal 1 (IID.size (IID.insert 5 6 test_dict))); *)
  "test init deck length" >:: (fun _ -> assert_equal 15 (length test_deck 0));
  "test shuffled deck length" >:: 
    (fun _ -> assert_equal 15 (length (shuffle test_deck) 0));
  "test draw from deck length" >:: 
    (fun _ -> assert_equal 14 (length (snd (draw test_deck)) 0));
  "test draw failure when empty list is given" >::
    (fun _ -> assert_raises 
    (Failure "This won't happen (draw failed)") (fun() -> draw []));
  "test draw2 from deck length" >:: 
    (fun _ -> assert_equal 13 (length (snd (draw2 test_deck)) 0));
  "test draw2 failure when empty list is given" >::
    (fun _ -> assert_raises 
    (Failure "This won't happen (draw2 failed)") (fun() -> draw2 []));
  "test draw2 from deck length" >:: 
    (fun _ -> assert_equal 13 (length (snd (draw2 test_deck)) 0));
  "test get deck names Assasin" >:: 
    (fun _ -> assert_equal "Assassin" (get_name (Assassin, Deck)));
  "test get deck names Duke" >:: 
    (fun _ -> assert_equal "Duke" (get_name (Duke, Deck)));
  "test get deck names Captain" >:: 
    (fun _ -> assert_equal "Captain" (get_name (Captain, Deck)));
  "test get deck names Ambassador" >:: 
    (fun _ -> assert_equal "Ambassador" (get_name (Ambassador, Deck)));
  "test get deck names Contessa" >:: 
    (fun _ -> assert_equal "Contessa" (get_name (Contessa, Deck)));
  "test get status in deck" >:: 
    (fun _ -> assert_equal "in the deck" (get_status (Duke, Deck)));
  "test get status out of play" >:: 
    (fun _ -> assert_equal "out of play" (get_status (Duke, FaceUp)));
  "test get status in play" >:: 
    (fun _ -> assert_equal "in play" (get_status (Duke, FaceDown)));
  "test get action tax" >:: 
    (fun _ -> assert_equal "Tax" (get_action (Duke, FaceDown)));
  "test get action assassinate" >:: 
    (fun _ -> assert_equal "Assassinate" (get_action (Assassin, FaceDown)));
  "test get action none" >:: 
    (fun _ -> assert_equal "None" (get_action (Contessa, FaceDown)));
  "test get action steal" >:: 
    (fun _ -> assert_equal "Steal" (get_action (Captain, FaceDown)));
  "test get action exchange" >:: 
    (fun _ -> assert_equal "Exchange" (get_action (Ambassador, FaceDown)));
  "test get blocks foreign_aid" >:: 
    (fun _ -> assert_equal "Foreign Aid" (get_blocks (Duke, FaceDown)));
  "test get blocks none" >:: 
    (fun _ -> assert_equal "None" (get_blocks (Assassin, FaceDown)));
  "test get blocks assassinate" >:: 
    (fun _ -> assert_equal "Assassinate" (get_blocks (Contessa, FaceDown)));
  "test get blocks steal" >:: 
    (fun _ -> assert_equal "Steal" (get_blocks (Captain, FaceDown)));
  "test get blocks steal" >:: 
    (fun _ -> assert_equal "Steal" (get_blocks (Ambassador, FaceDown)));
  "test is facedown" >:: 
    (fun _ -> assert_equal true (is_facedown (Captain, FaceDown)));
  "test is faceup" >:: 
    (fun _ -> assert_equal true (is_faceup (Captain, FaceUp)));
  "test is set status" >:: 
    (fun _ -> assert_equal 
    (Captain, FaceUp) (set_status (Captain, FaceDown) FaceUp));
  "test name to card duke" >:: 
    (fun _ -> assert_equal Duke (name_to_card "Duke"));
  "test name to card assassin" >:: 
    (fun _ -> assert_equal Assassin (name_to_card "Assassin"));
  "test name to card contessa" >:: 
    (fun _ -> assert_equal Contessa (name_to_card "Contessa"));
  "test name to card ambassador" >:: 
    (fun _ -> assert_equal Ambassador (name_to_card "Ambassador"));
  "test name to card captain" >:: 
    (fun _ -> assert_equal Captain (name_to_card "Captain"));
  "test failure using invalid card name" >:: 
    (fun _ -> assert_raises (InvalidCard "asdf") (fun () -> name_to_card "asdf"));
]
let suite = "test suite" >::: tests
let _ = run_test_tt_main suite