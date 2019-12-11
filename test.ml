open OUnit2
open Board
open Deck

(*  TEST PLAN:
    OUnit tested:
      Every public function except shuffle in deck.ml was tested thoroughly 
      below as those functions test aspects of a deck that never change.
      The fact that the deck cannot change makes the functions automatically
      testable. This is the main reason shuffle cannot be tested. Then, some
      functions of board.ml are testable when they test aspects that are 
      constant regardless of how the board randomly generates itself. These
      functions include [is_alive bd player], [victory bd], [id_is_ai id b],
      [player_names bd], and [current_player bd]. These are the only 
      functions that can be automatically tested consistently
    Manually tested:
      All other functions such as the remaining ones in board, everything in
      command.ml, and the entirety of main.ml were either best tested by 
      actually playing the game or hard to test due to being randomly 
      generated. For example, it is easy to see if parsing in command.ml
      works correctly by directly playing the game, entering commands, and
      seeing if it works as expected. The command testing was similar to 
      how a3 was tested. Then, the testing of main.ml would be impossible
      or inefficient for unit testing, as playing the game will allow you
      to see if it works as intended when information is printed on the 
      screen. Manually testing random generation dependent functions 
      was not impossible as we could play the game, copy the text or take
      a picture of results or bugs, and log it. Most random generation
      dependent functions also returned a Board.t, which would be hard to
      test an equal value for if a Board.t is not public.
    Handling glass box testing:
      Most Ounit tests below use glass box testing as we can see the code
      for functions ourselves and are able to know the expected answer. So,
      OUnit2 test cases were developed to directly test the value of the expected
      answer.
    Handling black box testing:
      However there are exceptions to the glass box testing section as 
      functions like [init_deck] and [init_board] create some randomly 
      generated values like a shuffled deck and board with random players.
      To test a direct value instead, the OUnit2 testable functions would
      test other aspects of the value instead of the direct value. 
      As a result, init_deck was tested by looking checking the expected 
      length of the deck, which was a list. Then, manual play testing 
      would evaluate whether the deck would deal cards correctly, and if
      the correct number of cards was in the deck. We have also evaluated
      through play testing the rest of the random generation dependant
      functions like [swap], which we would determine if it functioned 
      correctly/shuffled the deck randomly correctly through play testing,
      as all card info is printed for debuggging purposes during our play.
    Why these approaches test the system correctly:
      These approaches appropriately test the system because the OUnit2
      tests thoroughly check if each return value of a function plsu every
      possible value is the correct expected value. Then, the manual play
      testing allows us to test functions that depend on random generation
      by letting us see all printed info of the board to determine if the 
      information is as expected and that the game functions correctly.
      Therefore, the game is tested to behave as expected by us through
      these methods.*)

let tests = [
]

let rec length lst accu = match lst with
  | [] -> accu
  | h :: t -> length t (accu + 1)

let test_deck = init_deck ()
let test_board = init_board test_deck 5 "normal"
let test_board2 = init_board test_deck 2 "normal"
let test_board2_next = next_turn test_board2
let test_board_next = next_turn test_board
let test_board3 = init_board test_deck 3 "normal"
let test_board3_next = next_turn test_board3
let test_board4 = init_board test_deck 4 "normal"
let test_board_foreign_aid = foreign_aid "host" test_board
let tests = [
  (* draw tests *)
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
  (fun _ -> assert_raises (InvalidCard "asdf") 
      (fun () -> name_to_card "asdf"));
  (*board tests *)
  "test is_alive host" >:: 
  (fun _ -> assert_equal true (is_alive test_board "host"));
  "test is_alive ai1" >:: 
  (fun _ -> assert_equal true (is_alive test_board "ai1"));
  "test is_alive ai2" >:: 
  (fun _ -> assert_equal true (is_alive test_board "ai2"));
  "test is_alive ai3" >:: 
  (fun _ -> assert_equal true (is_alive test_board "ai3"));
  "test is_alive ai4" >:: 
  (fun _ -> assert_equal true (is_alive test_board "ai4"));
  "test is_alive Invalid player" >:: 
  (fun _ -> assert_raises (InvalidPlayer "jjj") 
      (fun () -> is_alive test_board "jjj"));
  "test victory is false with an initial board" >::
  (fun _ -> assert_equal false (fst (victory test_board)));
  "test player is ai true" >::
  (fun _ -> assert_equal true (id_is_ai "ai1" test_board));
  "test player is ai false" >::
  (fun _ -> assert_equal false (id_is_ai "host" test_board));
  "test player names has 5 correct names" >::
  (fun _ -> assert_equal ["host";"ai4";"ai3";"ai2";"ai1"] 
      (player_names test_board));
  "test player names has 2 correct names" >::
  (fun _ -> assert_equal ["host";"ai1"] 
      (player_names test_board2));
  "test player names has 3 correct names" >::
  (fun _ -> assert_equal ["host";"ai2";"ai1"] 
      (player_names test_board3));
  "test player names has 4 correct names" >::
  (fun _ -> assert_equal ["host";"ai3";"ai2";"ai1"] 
      (player_names test_board4));
  "test current_player of intialized board" >::
  (fun _ -> assert_equal "host" 
      (get_player_id (current_player test_board)));
  "test current_player of next_turn board 2 players" >::
  (fun _ -> assert_equal "ai1" 
      (get_player_id (current_player test_board2_next)));
  "test current_player of next_turn board 2 players 2 turns" >::
  (fun _ -> assert_equal "host" 
      (get_player_id (current_player (next_turn test_board2_next))));
  "test current_player of next_turn board 5 players" >::
  (fun _ -> assert_equal "ai4" 
      (get_player_id (current_player test_board_next)));
  "test current_player of next_turn board 5 players 2 turns" >::
  (fun _ -> assert_equal "ai3" 
      (get_player_id (current_player (next_turn test_board_next))));
  "test current_player of next_turn board 3 players" >::
  (fun _ -> assert_equal "ai2" 
      (get_player_id (current_player test_board3_next)));
  "test current_player of next_turn board 3 players 2 turns" >::
  (fun _ -> assert_equal "ai1" 
      (get_player_id (current_player (next_turn test_board3_next))));
]
let suite = "test suite" >::: tests
let _ = run_test_tt_main suite