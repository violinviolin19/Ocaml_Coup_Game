
exception InvalidPlayer of string
exception InvalidCard of string

type player = {
  id: string;
  card_one: Deck.card;
  card_two: Deck.card;   
  money: int;
  ai: bool;
  alive: bool;
  telling_truth: bool;
}

type t = {
  current_deck : Deck.t;
  current_players : player list;
  turn_order : (int*string) list;
  turn: int;
  money_pool: int;
  last_action: string;
}


type result = Legal of t | Illegal | NoMoney

(** player_names bd] is a list of the string representations(names or ids) of 
    the players of [bd].*)
let player_names bd=
  List.map (fun x->x.id) bd.current_players

(** [check_pool bd] is the current money pool of [bd].*)
let check_pool bd=
  bd.money_pool

(** [next_turn bd] is [bd] with the current turn finished and the next player's
    turn beginning.*)
let next_turn bd=
  let next = if(bd.turn=(List.length bd.current_players) -1) then 0 else bd.turn+1 in
  {bd with turn= next}

(**[find_player player bd] is the player in [bd] identified by [player]. *)
let find_player player bd =
  let rec check_lst = function
    |[]->raise(InvalidPlayer player)
    |h::t when h.id=player -> h
    |h::t-> check_lst t in
  check_lst bd.current_players

(** [get_host bd] is the player with the first turn in [bd].*)
let get_host bd=
  find_player (snd (List.hd bd.turn_order)) bd

(** [current_player bd] is the player whose turn it is currently in [bd].*)
let current_player bd=
  find_player (List.assoc bd.turn bd.turn_order) bd

(** [current_player_id bd] is the id/name of the player whose turn it currently
    is in [bd].*)
let current_player_id bd=
  (List.assoc bd.turn bd.turn_order)

(** [check_id player_id bd] is true if [player_id] is a player in [bd].*)
let check_id player_id bd=
  let rec check_list = function
    |[]-> false
    |h::t when h.id=player_id -> true
    |h::t -> check_list t in
  check_list bd.current_players

let rec everyones_info_helper accu player_list bd = (*this needs helper bc of mli*)
  match player_list with
  | [] -> accu
  | h :: t->
    let card_names= h.id ^ "'s cards are: "^Deck.get_name (h.card_one)^" and "^
                    Deck.get_name (h.card_two) in
    let card1_info= ". "^ h.id ^ " has a "^Deck.get_name h.card_one^" "
                    ^Deck.get_status h.card_one in
    let card2_info= " and a "^Deck.get_name h.card_two^" "
                    ^Deck.get_status h.card_two in
    let money_info= ". "^ h.id ^" has "^ string_of_int h.money ^ " coins. " in
    let status= h.id ^" is "^ (if(h.alive) then "" else "not ")^"alive." in
    let player_info = card_names^card1_info^card2_info^money_info^status^"\n" in
    everyones_info_helper (accu ^ "\n"^ player_info) t bd

let everyones_info bd =
  everyones_info_helper "" bd.current_players bd

(** [turn_info player bd] is the relevant information [player] will be given
    about themselves during a turn in [bd]. *)
let turn_info player bd=
  let card_names="Your cards are: "^Deck.get_name (player.card_one)^" and "^
                 Deck.get_name (player.card_two) in
  let card1_info= ". You have a "^Deck.get_name player.card_one^" "
                  ^Deck.get_status player.card_one in
  let card2_info= " and a "^Deck.get_name player.card_two^" "
                  ^Deck.get_status player.card_two in
  let money_info= ". You have "^ string_of_int player.money ^ " coins. " in
  let status= "You are "^ (if(player.alive) then "" else "not ")^"alive." in
  card_names^card1_info^card2_info^money_info^status^"\n"

(** [is_ai player] is true if [player] is an ai.*)
let is_ai player= player.ai

(** [id_is_ai id b] is true if the player with name [id] in [b] is an ai.*)
let id_is_ai id b= is_ai (find_player id b)

let deal_pair deck : ((Deck.card*Deck.card)*Deck.t)=
  (*call a function that deals a card twice, and return a tuple with a card tuple
    and a deck *)
  match Deck.draw2 deck with 
  |([c1;c2],d)->((c1,c2),d)
  |_->failwith "impossible"


(** [generate_player deck id is_ai] is a pair of a player generated with name
    [id] and from [deck], and the deck resulting from this generation. This
    player is computer controlled if [is_ai] is true, and player controlled
    otherwise.*)
let generate_player deck id is_ai=
  let set_cards pair=
    match pair with
    |((c1,c2),d)-> (Deck.set_status c1 Deck.FaceDown, Deck.set_status c2 Deck.FaceDown) in
  let pair = deal_pair deck in
  let cards = set_cards pair in
  ({
    id= id;
    card_one= fst (cards);
    card_two= snd (cards);
    money= 2;
    ai= is_ai;
    alive= true;
    telling_truth = true;
  },snd pair)

(** [generate_player_lst deck num_players] is a pair of a list of [num_players]
    different players generated with [deck] and the remaining deck left from
    this generation. This includes one host player and [num_players]-1 
    non-player-controlled players. *)
let generate_player_lst deck num_players =
  let rec last_deck = function
    |[]->failwith "empty list of players"
    |h::[]->snd h
    |h::t -> last_deck t in
  let rec generate_ai num cur_deck=
    if(num=0) then [] else
      let next_player = (generate_player cur_deck ("ai"^string_of_int num) true) in
      next_player :: generate_ai (num-1) (snd next_player) in
  let host = generate_player deck "host" false in
  let ai_enemy= generate_ai (num_players-1) (snd host) in
  ((fst host)::(List.map fst ai_enemy), last_deck ai_enemy)

(** [get_player_id player] is [player]'s id, i.e. [player]'s name.*)
let get_player_id player = 
  player.id

(** [init_board deck num_players] is the first game state of a game generated
    with [deck] and a [num_players] number of players.*)
let init_board deck num_players =
  let rec assign_turns turn=function
    |[]->[]
    |h::t-> (turn,h) :: assign_turns (turn+1) t in
  let info = generate_player_lst deck num_players in
  {
    current_deck= snd info;
    current_players= fst info;
    turn_order= assign_turns 0 (List.map (fun h -> h.id) (fst info));
    turn= 0;
    money_pool = 30;
    last_action = ""
  }


(** [check_bank player_id int bd] is true iff the player with id of [player_id]
    in [bd] has at least [cash] coins.*)
let check_bank player_id cash bd=
  let player= find_player player_id bd in player.money>=cash

(** [replace_player cur_id new_player bd] is [bd] with the player with id 
    [cur_id] replaced with [new_player]. The precondition to this function
    is that there exists a player in [bd] with [cur_id] as their id.*)
let replace_player cur_id new_player bd=
  let is_not_cur player = player.id <> cur_id in
  let players= List.filter is_not_cur bd.current_players in
  {bd with current_players=new_player::players}

(** [get_cards player bd] is the list of cards that the player identified by
    [player] controls in [bd]. If [player] is not a player in [bd] then raise
    an InvalidPlayer exception. *)
let rec get_cards player bd=
  let desired_player = find_player player bd in
  [desired_player.card_one;desired_player.card_two]

(** [get_money bd player] is the amount of money that the player identified by
    [player] has in [bd]. If [player] is not a player in [bd] then raise an
    invalid player exception. *)
let get_money bd player=
  let desired_player= find_player player bd in 
  desired_player.money

(** [change_money player_name bd cash] is [bd] with the player of id [player_name]
    having their money changed by [cash]. [cash] can be positive or negative,
    depending on whether an addition or subtraction of money is desired(postive
    [cash] corresponds to adding money).*)
let change_money player_name bd cash=
  let player= find_player player_name bd in
  let new_player= {player with money=player.money+cash} in
  let change_player=replace_player player_name new_player bd in
  {change_player with money_pool= change_player.money_pool - cash}

(** [steal stealer_id stolen_id bd] is the result of [stealer_id] stealing from
    [stolen_id] in [bd]. If [stolen_id] has no money to steal then
    [steal stealer_id stolen_id bd = NoMoney], otherwise it is a legal result
    carrying [bd] after this steal has taken place(of either one or two coins) 
    or an illegal result.*)
let steal stealer_id stolen_id bd=
  try
    (let stolen_player = (find_player stolen_id bd) in
     if stolen_player.money >= 2 then
       let stealer_given = change_money stealer_id bd 2 in
       Legal (change_money stolen_id stealer_given (-2))
     else if stolen_player.money = 1 then
       let stealer_given = change_money stealer_id bd stolen_player.money in
       Legal (change_money stolen_id stealer_given (-stolen_player.money))
     else
       NoMoney) 
  with
    _->Illegal

(** [find_player_card player_id card_id bd] is the  number(that is the first
    or the second) of [card_id] for [player_id] in [bd]. Raises an InvalidCard
    exception if [player_id] does not have a copy of [card_id].*)
let find_player_card player_id card_id bd =
  let player= find_player player_id bd in
  if(Deck.get_name player.card_one=card_id) then 1 else
  if(Deck.get_name player.card_two=card_id) then 2 else 
    raise(InvalidCard (card_id))

(** [turnover_card killed_id bd card] is [bd] with [killed_id]'s copy of [card]
    turned from facedown to faceup. If [killed_id] does not contain a copy of
    [card] then raises an InvalidCard exception. Raises a InvalidPlayer
    exception if [killed_id] is not a player of [bd].*)
let turnover_card killed_id bd card=
  let killed= find_player killed_id bd in 
  if(Deck.get_name killed.card_one<>Deck.get_name killed.card_two) then
    let killed= 
      if(find_player_card killed_id card bd = 1) then 
        {killed with card_one=Deck.set_status killed.card_one Deck.FaceUp} else
      if(find_player_card killed_id card bd = 2) then 
        {killed with card_two=Deck.set_status killed.card_two Deck.FaceUp} 
      else killed in replace_player killed_id killed bd
  else 
    let killed=
      if(not (Deck.is_facedown killed.card_one)) then
        {killed with card_two= Deck.set_status killed.card_two Deck.FaceUp}
      else 
        {killed with card_one=Deck.set_status killed.card_one Deck.FaceUp}
    in replace_player killed_id killed bd

(** [find_facedown player_id bd] is the first facedown card of [player_id] in
    [bd]. Fails if [player_id] has no facedown cards.*)
let find_facedown player_id bd=
  match get_cards player_id bd with
  |[]->failwith "impossible, no cards"
  |h::t when snd h = Deck.FaceDown -> h
  |h::t -> List.hd t

(** [check_faceup card_list] is true if all cards of [card_list] are faceup.
    Requires: [List.length card_list=2].*)
let check_faceup card_list = 
  match card_list with 
  | [card1; card2] -> (snd card1 = Deck.FaceUp && snd card2 = Deck.FaceUp)
  | _ -> failwith "Something went wrong"


(** [cards player_id bd] are the cards that [player_id] has face down in [bd].*)
let cards player_id bd=
  let card_list =get_cards player_id bd in
  let facedown_cards= List.filter Deck.is_facedown card_list in
  match List.map Deck.get_name facedown_cards with
  |c1::c2::[]->"A "^c1^" and a "^c2
  |c1::[]->"A "^c1
  |_->failwith "impossible"


(** [assassinate killer_id killed_id bd card_id] is a legal result of [bd] after
    [killer_id] assassinates [killed_id]'s [card_id] in [bd], or an illegal
    result. Requires: [card_id] is a card of [killed_id]. *)
let assassinate killer_id killed_id bd card_id=
  try 
    let killer_paid = change_money killer_id bd (-3) in
    Legal (turnover_card killed_id killer_paid card_id)
  with
    _ -> Illegal

(** [coup couper_id couped_id bd card_id] is a legal result of [bd] after
    [couper_id] coups [couped_id]'s [card_id] in [bd], or an illegal
    result. Requires: [card_id] is a card of [couped_id]. *)
let coup couper_id couped_id bd card_id=
  try
    let couper_paid = change_money couper_id bd (-7) in
    Legal (turnover_card couped_id couper_paid card_id)
  with
    _ -> Illegal

(** [has_both player_id bd] is true if [player_id] has no faceup cards in [bd].
    Raises an InvalidPlayer exception if [player_id] is not a player in [bd].*)
let has_both player_id bd =
  let cards = get_cards player_id bd in
  List.filter Deck.is_faceup cards = []

(** [view_four exchanger_id bd] is a pair of the cards [exchanger_id] would
    see in an exchange, and the deck after two cards are drawn from [bd]'s deck
    to be viewed by [exchanger_id]. Raises an InvalidPlayer exception if
    [exchanger_id] is not a valid player in [bd].*)
let view_four exchanger_id bd=
  let player_cards = get_cards exchanger_id bd in
  let player_cards= List.filter Deck.is_facedown player_cards in
  let pair= deal_pair bd.current_deck in
  let card1= Deck.set_status (fst (fst pair)) Deck.FaceDown in
  let card2= Deck.set_status (snd (fst pair)) Deck.FaceDown in
  (card1 :: card2 :: player_cards, snd pair)

(** [exchange exchanger_id bd card1 card2 deck discards] is a legal result of
    [bd] after [exchanger_id] exchanges their cards for [card1] and [card2],
    and has chosen to discard [discards] back into [deck] in [bd], or an
    illegal result if an exception is raised in execution.*)
let exchange exchanger_id bd card1 card2 deck discards=
  (* Rewritten List.map so that first arg is what is in the list*)
  let rec set_deck lst=
    match lst with
    |[]->[]
    |h::t -> Deck.set_status h Deck.FaceDown :: t in
  try 
    let exchanger= find_player exchanger_id bd in

    let exchanged= begin
      if(has_both exchanger_id bd) then {exchanger with card_one=card1; card_two=card2} else
      if(Deck.is_facedown exchanger.card_one) then {exchanger with card_one=card1} else {exchanger with card_two = card1}
    end
    in
    let player_swapped=replace_player exchanger_id exchanged bd in
    let new_board= {player_swapped with current_deck = Deck.shuffle (set_deck discards @ deck)} in 
    Legal new_board

  with 
    _ -> Illegal

(** [income player_name bd] is a legal result of [bd] after [player_name] takes
    an income(1 coin) in [bd], or an illegal result if something goes wrong.*)
let income player_name bd = 
  try 
    if bd.money_pool == 0 then Legal bd 
    else Legal (change_money player_name bd 1) 
  with
    _ -> Illegal

(** [foreign_aid player_name bd] is a legal result of [bd] after [player_name] 
    takes foreign aid(2 coins) in [bd], or an illegal result if something goes 
    wrong.*)
let foreign_aid player_name bd = 
  try 
    if bd.money_pool < 2 then Legal bd 
    else Legal (change_money player_name bd 2)
  with
    _ -> Illegal

(** [tax player_name bd] is a legal result of [bd] after [player_name] takes
    a tax(3 coins) in [bd], or an illegal result if something goes wrong.*)
let tax player_name bd = 
  try
    if bd.money_pool < 3 then Legal bd 
    else Legal (change_money player_name bd 3)
  with
    _ -> Illegal

(** [extract_legal b] is the board contained by [b]. If [b] is illegal then
    [extract_legal b] is an empty, placeholder board. *)
let extract_legal b = match b with
  | Legal i -> i
  | _ -> { (*place holder board because other results should never be used*)
      current_deck = []; (*init deck does not exist yet*)
      current_players = [];
      turn_order = [];
      turn = 0;
      money_pool = 0;
      last_action = "";
    }

(** [can_act actor_name action_name bd] is [true] if [actor_name] can perform
    [action_name] in [bd]. Raises an InvalidPlayer exception if [actor_name] is
    not a valid player in [bd].*)
let can_act actor_name action_name bd =
  let actor_cards= get_cards actor_name bd in
  let facedown_cards= List.filter Deck.is_facedown actor_cards in
  let actions= List.map Deck.get_action facedown_cards in
  match action_name with
  |s when List.mem (String.capitalize_ascii s) actions -> true
  |_ -> false

(** [can_block actor_name action_name bd] is true if [actor_name] is able to
    block [action_name] in bd. Raises an InvalidPlayer exception if [actor_name]
    is not a valid player in [bd].*)
let can_block actor_name action_name bd =
  let actor_cards= get_cards actor_name bd in
  let facedown_cards= List.filter Deck.is_facedown actor_cards in
  let actions= List.map Deck.get_blocks facedown_cards in
  match action_name with
  |s when List.mem (String.capitalize_ascii s) actions -> true
  |_ -> false


(* ---------------- Block commands begin here ----------------- *)

let get_last_action bd = 
  bd.last_action

let set_last_action bd s = 
  {bd with last_action = s}

let make_player_lie bd = 
  let curr_player = 
    current_player bd in
  replace_player (current_player_id bd) {curr_player with telling_truth = false}
    bd

let block_duke bd = 
  if bd.last_action = "foreign aid" then bd else make_player_lie bd

let block_cap_amb bd = 
  if bd.last_action = "steal" then bd else make_player_lie bd

let block_contessa bd = 
  if bd.last_action = "assasinate" then bd else make_player_lie bd

let block bd character= 
  match character with 
  |"duke" -> Legal (block_duke bd)
  |"captain"-> Legal (block_cap_amb bd)
  |"ambassador" -> Legal (block_cap_amb bd)
  |"contessa" -> Legal (block_contessa bd) 
  |_ -> Illegal
