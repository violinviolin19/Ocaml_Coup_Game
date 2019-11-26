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


let choose_two cards can_get_both=
  let rec list_to_string = function
    |[] -> ""
    |h::t-> h ^ " "^list_to_string t in
  let rec list_remove r= function
    |[]->[]
    |h::t when Deck.get_name h=Deck.get_name r -> t
    |h::t -> h:: list_remove r t in
  let rec card_choice list =
    print_endline ("Choose one of "^ list_to_string list ^ ".\n");
    match String.capitalize_ascii (read_line ()) with
    | s -> begin
        try
          let card= (Deck.name_to_card s, Deck.FaceDown) in
          if(List.mem (String.trim (String.capitalize_ascii s)) list ) then card 
          else begin print_endline "You cannot choose that card, try again.\n"; card_choice list end 
        with Deck.InvalidCard s ->
          print_endline (s^" is not a valid card");
          card_choice list
      end in
  let card_strings= List.map Deck.get_name cards in
  let card1= card_choice card_strings in
  let new_cards= list_remove card1 cards in
  if(can_get_both) then begin
    let card2= card_choice (List.map Deck.get_name new_cards) in
    ((card1, card2), list_remove card2 new_cards)
  end
  else 
    ((card1, card1), new_cards)

(** [challenge_block blocker challenger action b] is [b] after [challenger]'s 
    challenge against [blocker]'s block of [action] paired with whether the 
    challenge was successful. [challenge_block] only resolves whether or not the
    challenge is successful and the resultant turnover of a card, not the 
    results of the challenged action.*)
let challenge_block blocker challenger action b=
  if(can_block blocker action b) then
    let card_choice= if(id_is_ai challenger b) then Deck.get_name (Board.find_facedown challenger b) else choose_card b challenger in
    print_endline(challenger^" unsuccessfully challenged "^blocker^"'s "^action^" block.");
    (turnover_card challenger b card_choice,false)
  else
    let card_choice= if(id_is_ai blocker b) then Deck.get_name (Board.find_facedown blocker b) else choose_card b blocker in
    print_endline(challenger^" successfully challenged "^blocker^"'s "^action^ " block.");
    (turnover_card blocker b card_choice,true)



(** [player_challenge b action actor] is whether the player decides to challenge
    [actor]'s choice to perform [action] in [bd].*)
let rec player_challenge b action actor target=
  let if_target= if(action="Tax"||action="Exchange") then "" else " towards "^target in
  print_string 
    ("Would you like to challenge "^actor^"'s "^action^if_target^"? Yes or No?"); 
  match String.lowercase_ascii (read_line()) with
  |"yes"->true
  |"no"->false
  |_->print_string ("Invalid choice, try again."); player_challenge b action actor target

let rec player_block b action actor = 
  print_string ("Would you like to block "^actor^"'s "^action^"? 
  Either type block or continue. \n"); 
  match parse_block (String.lowercase_ascii (read_line())) with 
  | Continue -> false
  | Block -> true
  | _ -> failwith "unimplemented"


let rec player_challenge_block action actor =
  print_endline("Would you like to challenge "^actor^"'s block of "^action);
  match String.trim (String.lowercase_ascii (read_line())) with
  |"yes"->true
  |"no"->false
  |_->print_endline("Invalid choice, try again."); player_challenge_block action actor

let rec play_game b = 
  let curr_player = current_player b in
  let cards_list = get_cards (current_player_id b) b in 
  let curr_id= current_player_id b in
  let host_id = get_player_id (get_host b) in
  let non_cur_players= List.filter (fun x -> x<>host_id) (List.filter (fun x -> x<>curr_id) (player_names b)) in 
  let non_host_players= List.filter ( fun x -> x<>host_id) (player_names b) in
  if (not (is_ai curr_player) &&check_faceup cards_list) then (print_string "You have lost influence. Good luck next time! \n"; exit 0) else
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
      print_endline (current_player_id b ^ " tries to take foreign aid");
      if (player_block b "Foreign Aid" (current_player_id b)) then 
        (print_endline "You blocked the action.";
         let challenger= any_challenge_block non_host_players b "Foreign Aid" curr_id in
         if(fst challenger) then
           let challenger_id = snd challenger in
           let challenge_st = challenge_block host_id challenger_id "Foreign Aid" b in
           if(snd challenge_st) then
             let new_b = foreign_aid (current_player_id b) (fst challenge_st) in
             if new_b != Illegal then
               let legal_item = extract_legal (new_b) in
               print_endline (current_player_id b ^ " takes foreign aid. \n");
               print_string "\n> ";
               play_game (next_turn legal_item)
             else 
               (print_endline "That's not a valid command to take foreign aid try again.\n";
                print_string "\n> ";
                play_game b)
           else
             play_game (next_turn (fst challenge_st))

         else
         if (can_block host_id "Foreign Aid" b) then 
           play_game (next_turn b)
         else 
           play_game (next_turn (make_player_lie b)))
      else 
        let ai_blocker= should_any_block non_cur_players b "Foreign Aid" curr_id in
        if(not (fst ai_blocker)) then
          if new_b != Illegal then
            let legal_item = extract_legal (new_b) in
            print_endline (current_player_id b ^ " takes foreign aid. \n");
            print_string "\n> ";
            play_game (next_turn legal_item)
          else 
            (print_endline "That's not a valid command to take foreign aid try again.\n";
             print_string "\n> ";
             play_game b)
        else
          begin
            let blocker= snd ai_blocker in
            print_endline (blocker^ "blocked "^curr_id^"'s foreign aid."); 
            if (can_block blocker "Steal" b) then 
              play_game (next_turn b)
            else 
              play_game (next_turn (make_player_lie b))
          end
    |Steal killed_id ->
      let ai_challenger= should_any_challenge non_cur_players b "Steal" killed_id in
      print_endline (current_player_id b ^ " tries to steal from "^killed_id);
      if (player_block b "Steal" (current_player_id b)) then 
        (print_endline "You blocked the action."; 
         if (can_block killed_id "Steal" b) then 
           play_game (next_turn b)
         else 
           play_game (next_turn (make_player_lie b)))
      else
        let ai_blocker = should_any_block non_cur_players b "Steal" killed_id in
        if(not (fst ai_blocker)) then
          if(player_challenge b "Steal" (current_player_id b) killed_id) then
            if(can_act curr_id "Steal" b) then
              let card_choice = choose_card b host_id in
              print_string ("You have failed in your challenge, now you must turnover your "^card_choice ^ "\n");
              let turnover= turnover_card host_id b card_choice in
              let new_b= steal (current_player_id b) killed_id turnover in
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
              let card_choice= Deck.get_name (Board.find_facedown curr_id b) in
              print_string("You were right! "^curr_id^" turns over their "^card_choice ^ "\n");
              let turnover= turnover_card curr_id b card_choice in
              play_game (next_turn turnover)
          else if (not(fst ai_challenger)) then
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
            let challenger_id= snd ai_challenger in 
            if(can_act curr_id "Steal" b) then
              let card_choice= Deck.get_name (Board.find_facedown challenger_id b) in
              print_endline (challenger_id^" challenged "^curr_id^"'s steal unsuccessfully, they turned over their "^card_choice);
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
              (print_endline (challenger_id^"successfully challenged"^curr_id^"'s steal");
               let card_choice = choose_card b curr_id in
               play_game (next_turn (turnover_card curr_id b card_choice)))

        else begin
          let blocker= snd ai_blocker in
          print_endline (blocker^ "blocked the steal."); 
          if (can_block blocker "Steal" b) then 
            play_game (next_turn b)
          else 
            play_game (next_turn (make_player_lie b))
        end
    | Tax -> 
      print_endline (current_player_id b ^ " tries to take tax. \n");
      let ai_challenger= should_any_challenge non_cur_players b "Tax" "" in
      if(player_challenge b "Tax" (current_player_id b)"") then
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
          let turnover= turnover_card curr_id b card_choice in
          play_game (next_turn turnover)
      else 
      if(fst ai_challenger) then
        if(can_act curr_id "Tax" b) then
          let challenger_id= snd ai_challenger in
          let card_choice= Deck.get_name (Board.find_facedown challenger_id b) in
          print_endline (challenger_id^" turns over their "^card_choice^" after failing to challenge "^curr_id);
          let new_b = tax (current_player_id b) (turnover_card curr_id b card_choice) in
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
          print_string((snd ai_challenger) ^"was right! "^curr_id^" turns over their "^card_choice^ "\n");
          play_game (next_turn (turnover_card curr_id b card_choice))
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
      let ai_challenger= should_any_challenge non_cur_players b "Assassinate" killed_id in
      let ai_blocker= should_any_block non_cur_players b "Assassinate" killed_id in
      let block_challenge= any_challenge_block non_host_players b "Assassinate" curr_id in
      print_endline (current_player_id b ^ " tries to assassinate "^killed_id);
      let player_block= player_block b "Assassinate" (current_player_id b)||fst ai_blocker in
      let blocker= if(player_block||fst ai_blocker) then (if player_block then (true,host_id) else (true,snd ai_blocker)) else (false,host_id) in
      let blocker_id= snd blocker in
      if (fst blocker) then 
        let challenger= if(blocker_id=host_id) then block_challenge else (player_challenge_block "Assassinate" blocker_id, host_id) in
        let challenger= if(fst challenger= false) then block_challenge else challenger in
        if(fst challenger) then
          let challenge_st= challenge_block blocker_id (snd challenger) "Assassinate" b in
          let board= fst challenge_st in
          if(snd challenge_st) then
            let card= if(killed_id=host_id) then choose_card board host_id else Deck.get_name (Board.find_facedown killed_id board) in
            let new_b= assassinate (current_player_id b) killed_id board card in 
            if new_b != Illegal then
              let legal_item = extract_legal new_b in
              print_endline (current_player_id b ^ " assassinates "^killed_id^"'s "^card);
              print_string "\n> ";
              play_game (next_turn legal_item) 
            else 
              play_game board
          else
            (print_endline (killed_id^" blocked the assassination."); 
             if (can_block killed_id "Assassinate" b) then 
               play_game (next_turn board)
             else 
               play_game (next_turn (make_player_lie board)))
        else
          (print_endline (killed_id^" blocked the assassination."); 
           if (can_block killed_id "Assassinate" b) then 
             play_game (next_turn b)
           else 
             play_game (next_turn (make_player_lie b)))
      else
      if(check_id killed_id b&&check_bank (current_player_id b) 3 b) then 
        if(player_challenge b "Assassinate" (current_player_id b) killed_id) then
          if(can_act curr_id "Assassinate" b) then
            let card_choice = choose_card b host_id in
            print_string ("You have failed in your challenge, now you must turnover "^card_choice ^ "\n") ;
            let turnover= turnover_card host_id b card_choice in
            let card= if(killed_id=host_id) then choose_card turnover host_id else Deck.get_name (Board.find_facedown killed_id turnover) in
            let new_b= assassinate (current_player_id turnover) killed_id turnover card in 
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
            print_string("You were right! "^curr_id^" turns over their "^card_choice^ "\n");
            play_game (next_turn (turnover_card curr_id b card_choice))
        else if(not (fst ai_challenger)) then
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
          let challenger_id= snd ai_challenger in 
          if(can_act curr_id "Assassinate" b) then
            let card_choice= Deck.get_name (Board.find_facedown challenger_id b) in
            print_endline (challenger_id^" challenged "^curr_id^"'s assassination unsuccessfully, they turned over their "^card_choice);
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
            (print_endline (challenger_id^"successfully challenged"^curr_id^"'s assassination");
             let card_choice = choose_card b curr_id in
             play_game (next_turn (turnover_card curr_id b card_choice)))
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
    | _ -> failwith "unimplemented"

  (*play_game(next_turn b)*)
  else
    print_string(everyones_info b ^ "\n");
  print_string(turn_info (get_host b) b);
  try match parse (read_line ()) with
    | Quit -> exit 0
    | Exchange -> begin
        let challenger = should_any_challenge non_cur_players b "Exchange" "" in
        if(fst challenger) then
          let challenger_id= snd challenger in
          if(can_act curr_id "Exchange" b) then
            let card_choice= Deck.get_name (Board.find_facedown challenger_id b) in
            print_endline (challenger_id^" challenged your exchange unsuccessfully, they turned over their "^card_choice);
            let new_st= turnover_card challenger_id b card_choice in
            let both= has_both curr_id b in
            let cards= view_four curr_id b in
            let choice_info = choose_two (fst cards) both in
            let discards= snd choice_info in
            let chosen= fst choice_info in
            let new_b = exchange curr_id new_st (fst chosen) (snd chosen) (snd cards) discards in
            if new_b != Illegal then
              let legal_item = extract_legal (new_b) in
              print_endline (current_player_id b ^ " exchanges with the court deck. \n");
              print_string "\n> ";
              let new_legal_item = shuffle_deck legal_item in 
              play_game (next_turn new_legal_item)
            else 
              (* Only needed for syntatic reasons*)
              play_game b
          else
            (print_endline (challenger_id^"successfully challenged your Exchange");
             let card_choice = choose_card b curr_id in
             play_game (next_turn (turnover_card curr_id b card_choice)))
        else
          let both= has_both curr_id b in
          let cards= view_four curr_id b in
          let choice_info = choose_two (fst cards) both in
          let discards= snd choice_info in
          let chosen= fst choice_info in
          let new_b = exchange curr_id b (fst chosen) (snd chosen) (snd cards) discards in
          if new_b != Illegal then
            let legal_item = extract_legal (new_b) in
            print_endline (current_player_id b ^ " exchanges with the court deck. \n");
            print_string "\n> ";
            play_game (next_turn legal_item)
          else 
            (* Only needed for syntatic reasons*)
            play_game b

      end
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
      let ai_blocker =should_any_block non_cur_players b "Foreign Aid" host_id in
      let blocker_id= snd ai_blocker in
      let block_challenger= any_challenge_block non_cur_players b "Foreign Aid" host_id in
      if(fst ai_blocker) then
        let player_challenge= begin 
          if(not(fst block_challenger)) then player_challenge_block "Foreign Aid" blocker_id 
          else false 
        end in
        if(not player_challenge &&not(fst block_challenger)&&snd block_challenger <> snd ai_blocker) then
          begin
            print_endline (blocker_id^ "blocked "^curr_id^"'s foreign aid."); 
            if (can_block blocker_id "Steal" b) then 
              play_game (next_turn b)
            else 
              play_game (next_turn (make_player_lie b))
          end
        else
          let challenger= if(player_challenge) then host_id else (snd(block_challenger)) in
          let challenge= challenge_block blocker_id (challenger) "Foreign Aid" b in
          if(snd challenge) then play_game (next_turn (fst(challenge))) 
          else if new_b != Illegal then
            let legal_item = extract_legal (new_b) in
            print_endline (current_player_id b ^ " takes foreign aid. \n");
            print_string "\n> ";
            play_game (next_turn legal_item)
          else 
            (print_endline "That's not a valid command to take foreign aid try again.\n";
             print_string "\n> ";
             play_game b)
      else
      if new_b != Illegal then
        let legal_item = extract_legal (new_b) in
        print_endline (current_player_id b ^ " takes foreign aid. \n");
        print_string "\n> ";
        play_game (next_turn legal_item)
      else 
        (print_endline "That's not a valid command to take foreign aid try again.\n";
         print_string "\n> ";
         play_game b)

    |Steal killed_string -> let killed_id = List.hd killed_string in
      if (curr_id) = killed_id then 
        (print_endline "You can't steal from yourself. That.... wouldn't be 
        stealing.";
         play_game b)
      else 
        let challenger= should_any_challenge non_cur_players b "Steal" killed_id in
        (if(check_id killed_id b) then
           let ai_blocker =should_any_block non_cur_players b "Steal" killed_id in
           if(fst ai_blocker) then
             begin
               let blocker= snd ai_blocker in
               print_endline (blocker^ "blocked "^curr_id^"'s steal."); 
               if (can_block blocker "Steal" b) then 
                 play_game (next_turn b)
               else 
                 play_game (next_turn (make_player_lie b))
             end
           else if(not(fst challenger)) then
             let new_b= steal (current_player_id b) killed_id b in
             if(new_b!= Illegal && new_b != NoMoney) then
               let legal_item = extract_legal new_b in
               print_endline (current_player_id b ^ " steals from "^killed_id);
               print_string "\n ";
               (play_game(next_turn legal_item))
             else if (new_b = Illegal) then
               (print_endline "That's not a valid command to steal try again \n";
                print_string "\n> ";
                play_game b)
             else 
               (print_endline "You cannot steal from someone with no money \n";
                print_string "\n> ";
                play_game b)
           else
             let challenger_id= snd challenger in 
             if(can_act curr_id "Steal" b) then
               let card_choice= Deck.get_name (Board.find_facedown challenger_id b) in
               print_endline (challenger_id^" challenged your Steal unsuccessfully, they turned over their "^card_choice);
               let new_b = extract_legal (steal (current_player_id b) killed_id b) in
               play_game (turnover_card challenger_id new_b card_choice)
             else
               (print_endline (challenger_id^"successfully challenged your Steal");
                let card_choice = choose_card b curr_id in
                play_game (next_turn (turnover_card curr_id b card_choice)))

         else
           (print_endline "That isn't a player";
            print_string "\n> ";
            play_game b))
    | Tax -> 
      let challenger= should_any_challenge non_cur_players b "Tax" "" in
      if(not (fst challenger)) then
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
      else
        let challenger_id= snd challenger in 
        if(can_act curr_id "Tax" b) then
          let card_choice= Deck.get_name (Board.find_facedown challenger_id b) in
          print_endline (challenger_id^" challenged your Tax unsuccessfully, they turned over their "^card_choice);
          let new_b = extract_legal (tax (current_player_id b) b) in
          play_game (turnover_card challenger_id new_b card_choice)
        else
          (print_endline (challenger_id^"successfully challenged your Tax");
           let card_choice = choose_card b curr_id in
           play_game (next_turn (turnover_card curr_id b card_choice)))


    |Assassinate killed_id -> let killed_id = List.hd killed_id in
      if (curr_id) = killed_id then 
        (print_endline "Why are you trying to kill yourself? Don't you 
        want to win?";
         play_game b)
      else 
        let challenger= should_any_challenge non_cur_players b "Assassinate" killed_id in
        if(check_id killed_id b&&check_bank (current_player_id b) 3 b) then
          if(should_block killed_id b "Assassinate" killed_id) then
            begin
              print_endline (killed_id^ "blocked your assassination.");
              let host_chal= player_challenge_block "Assassinate" killed_id in
              let challenger= if(host_chal) then (true,host_id) else challenger in
              if(fst challenger) then
                let challenge_st= challenge_block killed_id (snd challenger) "Assassinate" b in 
                let new_b= fst challenge_st in
                if(snd challenge_st) then
                  let card= Deck.get_name (Board.find_facedown killed_id new_b) in
                  let new_b= assassinate (current_player_id b) killed_id new_b card in 
                  if new_b != Illegal then
                    let legal_item = extract_legal new_b in
                    print_endline (current_player_id b ^ " assassinates "^killed_id^"'s "^card);
                    print_string "\n> ";
                    play_game (next_turn legal_item) 
                  else 
                    play_game b
                else
                  (if (can_block killed_id "Assassination" b) then 
                     play_game (next_turn new_b)
                   else 
                     play_game (next_turn (make_player_lie new_b)))
              else
              if (can_block killed_id "Assassination" b) then 
                play_game (next_turn b)
              else 
                play_game (next_turn (make_player_lie b))
            end
          else if(not(fst challenger)) then
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
            let challenger_id= snd challenger in 
            if(can_act curr_id "Assassinate" b) then
              let card_choice= Deck.get_name (Board.find_facedown challenger_id b) in
              print_endline (challenger_id^" challenged your assassination unsuccessfully, they turned over their "^card_choice);
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
              (print_endline (challenger_id^"successfully challenged your assassination");
               let card_choice = choose_card b curr_id in
               play_game (next_turn (turnover_card curr_id b card_choice)))
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
    |Block -> (*let blocked_id = List.hd blocked_id in 
                           let result = block b blocked_id in 
                           if result = Illegal then (
                           print_endline "Not a valid character that can block.";
                           print_string "\n> ";
                           play_game b)
                           else 
                           let legal_item = extract_legal result in 
                           print_endline "You blocked the last action.\n";
                           play_game (next_turn legal_item) *)
      play_game (next_turn b)
    | Continue -> play_game (next_turn b)

  with
  | Empty -> print_endline "You entered nothing, try again.\n"; 
    print_string "\n> ";
    play_game b 
  | Malformed -> print_endline "That wasn't understandable, try again.\n";
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