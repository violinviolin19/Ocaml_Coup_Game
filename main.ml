open Command
open Deck
open Board


type card

(** [process_turn bd player] processes [player]'s turn in [bd]*)
let rec process_turn bd player=
  failwith "unimplemented"

(** [choose_card bd] is the card [player] chooses to turnover in [bd] *)
let rec choose_card bd player : card=
  failwith "unimplemented"



let rec play_game b = 
  try match parse (read_line ()) with
  | Quit -> exit 0
  | Income -> let new_b = income (current_player_id b) b in 
    if new_b != Illegal then 
      let legal_item = extract_legal new_b in
      print_endline (current_player_id b ^ " takes income.");
      print_string "\n> ";
      play_game (next_turn legal_item) 
      (*this step and similar steps below may be moved to process turn*)
    else
      print_endline "That's not a valid command to take income try again\n";
      print_string "\n> ";
      play_game b
  | Foreign_Aid -> let new_b = foreign_aid (current_player_id b) b in
    if new_b != Illegal then
      let legal_item = extract_legal (new_b) in
      print_endline (current_player_id b ^ " takes foreign aid. \n");
      print_string "\n> ";
      play_game (next_turn legal_item)
    else 
      print_endline "That's not a valid command to take foreign aid try again.\n";
      print_string "\n> ";
      play_game b

  | Tax -> let new_b = tax (current_player_id b) b in
      if new_b != Illegal then 
        let legal_item = extract_legal (new_b) in
        print_endline (current_player_id b ^ " takes tax. \n");
        print_string "\n> ";
        play_game (next_turn legal_item)
      else
        print_endline "That's not a valid command to take tax try again\n";
        print_string "\n> ";
        play_game b;
  | _ -> ()

  with
  | Empty -> print_endline "You entered nothing, try again.\n"; 
    print_string "\n> ";
    play_game b 
  | Malformed -> print_endline "That wasn't understandable plz English.\n";
    print_string "\n> ";
    play_game b 



let main ()=
  ANSITerminal.(print_string [blue]
                  "\n\nWelcome to Ocaml Coup!\n");
  print_endline "Press any key to Start\n";
  print_string  "> ";
  let deck = [] in (*need to change, init deck doesnt exist yet*)
  let board = init_board deck 5 in
  match read_line () with
  | _ -> play_game board (*fix later*)

let () = main ()