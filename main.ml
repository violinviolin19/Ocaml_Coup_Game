open Command
open Deck
open Board
open Artificial

(** [choose_card bd] is the facedown card of [player] that they choose in [bd] *)
let rec choose_card bd player : string=
  let cards= cards player bd in
  let card_ids= List.map Deck.get_name (get_cards player bd) in
  print_string ("Which card would you like to turn over? You have "^ cards ^ " facedown: ");
  match String.capitalize_ascii (read_line()) with
  |s when List.mem s card_ids->s
  |_->print_string "You do not have a copy of that card facedown, try again"; choose_card bd player


(** [player_challenge b action actor] is whether the player decides to challenge
    [actor]'s choice to perform [action] in [bd].*)
let rec player_challenge b action actor=
  print_string ("Would you like to challenge "^actor^"'s "^action^". Yes or No?"); 
  match String.lowercase_ascii (read_line()) with
  |"yes"->true
  |"no"->false
  |_->print_string ("Invalid choice, try again."); player_challenge b action actor


let rec play_game b = 
  let curr_player = current_player b in
  let cards_list = get_cards (current_player_id b) b in 
  let curr_id= current_player_id b in
  let host_id = get_player_id (get_host b) in
  if (not (is_ai curr_player) &&check_faceup cards_list) then (print_string "TEMP LOSS MESSAGE \n"; exit 0) else
  if (is_ai curr_player&& check_faceup cards_list) then (print_string "Congrats, you win! \n"; exit 0) 
  else 
  if(is_ai (current_player b)) then
    match turn curr_id b with
    |Income -> let new_b = income curr_id b in 
      if new_b != Illegal then 
        let legal_item = extract_legal new_b in
        print_endline (current_player_id b ^ " takes income.");
        print_string "\n> ";
        play_game (next_turn legal_item) 
        (*this step and similar steps below may be moved to process turn
          when more complex actions like assassinate are made*)
      else
        print_endline "That's not a valid command to take income try again\n";
      print_string "\n> ";
      play_game b
    |ForeignAid -> let new_b = foreign_aid (current_player_id b) b in
      if new_b != Illegal then
        let legal_item = extract_legal (new_b) in
        print_endline (current_player_id b ^ " takes foreign aid. \n");
        print_string "\n> ";
        play_game (next_turn legal_item)
      else 
        (print_endline "That's not a valid command to take foreign aid try again.\n";
         print_string "\n> ";
         play_game b)
    |Steal killed_id ->
      if(player_challenge b "Steal" (current_player_id b)) then
        if(can_act curr_id "Steal" b) then
          let card_choice = choose_card b host_id in
          print_string ("You have failed in your challenge, now you must turnover your "^card_choice);
          play_game (turnover_card host_id b card_choice)
        else
          let card_choice= Deck.get_name (Board.find_facedown curr_id b) in
          print_string("You were right! "^curr_id^" turns over their "^card_choice);
          play_game (turnover_card curr_id b card_choice)
      else
        let new_b= steal (current_player_id b) killed_id b in
        if(new_b!= Illegal) then
          let legal_item = extract_legal new_b in
          print_endline (current_player_id b ^ " steals from "^killed_id);
          print_string "\n ";
          (play_game(next_turn legal_item))
        else
          (print_endline "That's not a valid command to steal try again \n";
           print_string "\n> ";
           play_game b)
    | Tax -> 
      if(player_challenge b "Tax" (current_player_id b)) then
        if(can_act curr_id "Tax" b) then
          let card_choice = choose_card b host_id in
          print_string ("You have failed in your challenge, now you must turnover your "^card_choice^"\n");
          let new_b = tax (current_player_id b) (turnover_card host_id b card_choice) in
          if new_b != Illegal then 
            let legal_item = extract_legal (new_b) in
            print_endline (current_player_id b ^ " takes tax. \n");
            print_string "\n> ";
            play_game (next_turn legal_item)
          else
            (print_endline "That's not a valid command to take tax try again\n";
             print_string "\n> ";
             play_game b;)
        else
          let card_choice= Deck.get_name (Board.find_facedown curr_id b) in
          print_string("You were right! "^curr_id^" turns over their "^card_choice^"\n");
          play_game (turnover_card curr_id b card_choice)
      else
        let new_b = tax (current_player_id b) b in
        if new_b != Illegal then 
          let legal_item = extract_legal (new_b) in
          print_endline (current_player_id b ^ " takes tax. \n");
          print_string "\n> ";
          play_game (next_turn legal_item)
        else
          (print_endline "That's not a valid command to take tax try again\n";
           print_string "\n> ";
           play_game b;)
    |Assassinate killed_id ->
      if(check_id killed_id b&&check_bank (current_player_id b) 3 b) then 
        if(player_challenge b "Assassinate" (current_player_id b)) then
          if(can_act curr_id "Assassinate" b) then
            let card_choice = choose_card b host_id in
            print_string ("You have failed in your challenge, now you must turnover "^card_choice);
            let card= if(killed_id=host_id) then choose_card b host_id else Deck.get_name (Board.find_facedown killed_id b) in
            let new_b= assassinate (current_player_id b) killed_id b card in 
            if new_b != Illegal then
              let legal_item = extract_legal new_b in
              print_endline (current_player_id b ^ " assassinates "^killed_id^"'s "^card);
              print_string "\n> ";
              play_game (next_turn legal_item) 
            else 
              (* Impossible case, only here for syntatic reasons*)
              play_game b
          else
            let card_choice= Deck.get_name (Board.find_facedown curr_id b) in
            print_string("You were right! "^curr_id^" turns over their "^card_choice);
            play_game (next_turn (turnover_card curr_id b card_choice))
        else
          let card= if(killed_id=host_id) then choose_card b host_id else Deck.get_name (Board.find_facedown killed_id b) in
          let new_b= assassinate (current_player_id b) killed_id b card in 
          if new_b != Illegal then
            let legal_item = extract_legal new_b in
            print_endline (current_player_id b ^ " assassinates "^killed_id^"'s "^card);
            print_string "\n> ";
            play_game (next_turn legal_item) 
          else 
            play_game b
      else
        (if(not (check_id killed_id b)) then print_endline "That isn't a player" 
         else print_endline "Not enough coins to assassinate";
         print_string "\n> ";
         play_game b)
    |Coup killed_id ->
      if(check_id killed_id b&&check_bank (current_player_id b) 7 b) then 
        let card= if(killed_id=host_id) then choose_card b host_id else Deck.get_name (Board.find_facedown killed_id b) in
        let new_b= coup (current_player_id b) killed_id b card in 
        if new_b != Illegal then
          let legal_item = extract_legal new_b in
          print_endline (current_player_id b ^ " coups "^killed_id^"'s "^card);
          print_string "\n> ";
          play_game (next_turn legal_item) 
        else 
          play_game b
      else
        (if(not (check_id killed_id b)) then print_endline "That isn't a player" 
         else print_endline "Not enough coins to coup";
         print_string "\n> ";
         play_game b)
    | _-> failwith "unimplemented"

  (*play_game(next_turn b)*)
  else
    print_string(turn_info (get_host b) b);
  try match parse (read_line ()) with
    | Quit -> exit 0
    | Income -> let new_b = income (current_player_id b) b in 
      if new_b != Illegal then 
        let legal_item = extract_legal new_b in
        print_endline (current_player_id b ^ " takes income.");
        print_string "\n> ";
        play_game (next_turn legal_item) 
        (*this step and similar steps below may be moved to process turn
          when more complex actions like assassinate are made*)
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
        (print_endline "That's not a valid command to take foreign aid try again.\n";
         print_string "\n> ";
         play_game b)

    |Steal killed_id -> let killed_id = List.hd killed_id in
      if(check_id killed_id b) then
        let new_b= steal (current_player_id b) killed_id b in
        if(new_b!= Illegal) then
          let legal_item = extract_legal new_b in
          print_endline (current_player_id b ^ " steals from "^killed_id);
          print_string "\n ";
          (play_game(next_turn legal_item))
        else
          (print_endline "That's not a valid command to steal try again \n";
           print_string "\n> ";
           play_game b)
      else
        (print_endline "That isn't a player";
         print_string "\n> ";
         play_game b)
    | Tax -> let new_b = tax (current_player_id b) b in
      if new_b != Illegal then 
        let legal_item = extract_legal (new_b) in
        print_endline (current_player_id b ^ " takes tax. \n");
        print_string "\n> ";
        play_game (next_turn legal_item)
      else
        (print_endline "That's not a valid command to take tax try again\n";
         print_string "\n> ";
         play_game b;)
    |Assassinate killed_id -> let killed_id = List.hd killed_id in
      if(check_id killed_id b&&check_bank (current_player_id b) 3 b) then 
        let card= Deck.get_name (Board.find_facedown killed_id b) in
        let new_b= assassinate (current_player_id b) killed_id b card in 
        if new_b != Illegal then
          let legal_item = extract_legal new_b in
          print_endline (current_player_id b ^ " assassinates "^killed_id^"'s "^card);
          print_string "\n> ";
          play_game (next_turn legal_item) 
        else 
          play_game b
      else
        (if(not (check_id killed_id b)) then print_endline "That isn't a player" 
         else print_endline "Not enough coins to assassinate";
         print_string "\n> ";
         play_game b)
    |Coup killed_id -> let killed_id = List.hd killed_id in
      if(check_id killed_id b&&check_bank (current_player_id b) 7 b) then 
        let card= Deck.get_name (Board.find_facedown killed_id b) in
        let new_b= coup (current_player_id b) killed_id b card in 
        if new_b != Illegal then
          let legal_item = extract_legal new_b in
          print_endline (current_player_id b ^ " coups "^killed_id^"'s "^card);
          print_string "\n> ";
          play_game (next_turn legal_item) 
        else 
          play_game b
      else
        (if(not (check_id killed_id b)) then print_endline "That isn't a player" 
         else print_endline "Not enough coins to coup";
         print_string "\n> ";
         play_game b)

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
  let deck = init_deck ""in 
  let board = init_board deck 2 in
  match read_line () with
  | _ -> play_game board (*fix later*)

let () = main ()