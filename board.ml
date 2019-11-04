type deck = Deck of string | Not_deck
type card_status = string*string
exception InvalidPlayer of string
exception InvalidCard of string

type player = {
  id: string;
  card_one: string;
  card_two: string;   
  money: int;
  ai: bool;
  alive: bool;
}

type t = {
  current_deck : deck;
  current_players : player list;
  turn_order : (int*player) list;
  turn: int;
}

let next_turn bd=
  {bd with turn= bd.turn+1}

let current_player bd=
  List.assoc bd.turn bd.turn_order

(** [turn_info player bd] is the relevant information [player] will be given
    about themselves during a turn in [bd]. *)
let turn_info player bd=
  let card_names="Your cards are: "^player.card_one^" and "^player.card_two in
  let card_info= ""(* Info for which cards are flipped up*) in
  let money_info= " . You have "^ string_of_int player.money ^ "coins. " in
  let status= "You are "^ (if(player.alive) then "" else "not ")^"alive" in
  card_names^card_info^money_info^status

(** [is_ai player] is true if [player] is an ai.*)
let is_ai player= player.ai

let deal_pair deck=
  (*call a function that deals a card twice, and return a tuple with a card tuple
    and a deck *)
  failwith "unimplemented"

let generate_player deck id is_ai=
  let pair = deal_pair deck in
  ({
    id= id;
    card_one= fst (snd pair);
    card_two= snd (snd pair);
    money= 2;
    ai= is_ai;
    alive= true;
  },fst pair)

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
    turn_order= assign_turns 1 (fst info);
    turn= 0
  }

(**[find_player player bd] is the player in [bd] identified by [player]. *)
let find_player player bd =
  let rec check_lst = function
    |[]->raise(InvalidPlayer player)
    |h::t when h.id=player -> h
    |h::t-> check_lst t in
  check_lst bd.current_players

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
    an invalid player exception. *)
let rec get_cards player bd=
  let desired_player = find_player player bd in
  [desired_player.card_one;desired_player.card_two]

(** [get_money player bd] is the amount of money that the player identified by
    [player] has in [bd]. If [player] is not a player in [bd] then raise an
    invalid player exception. *)
let get_money player bd=
  let desired_player= find_player player bd in 
  desired_player.money

(** [change_money player_name bd cash] is [bd] with the player of id [player_name]
    having their money changed by [cash]. [cash] can be positive or negative,
    depending on whether an addition or subtraction of money is desired(postive
    [cash] corresponds to adding money).*)
let change_money player_name bd cash=
  let player= find_player player_name bd in
  let new_player= {player with money=player.money+cash} in
  replace_player player_name new_player bd

let steal stealer_id stolen_id bd=
  let stealer_given = change_money stealer_id bd 2 in
  change_money stolen_id stealer_given (-2)

let find_player_card player_id card bd =
  let player= find_player player_id bd in
  if(player.card_one=card) then 1 else
  if(player.card_two=card) then 2 else 
    raise(InvalidCard card)

let turnover_card killed_id bd card=
  let killed= find_player killed_id bd in 
  let killed= 
    if(find_player_card killed_id card bd = 1) then {killed with card_one=""} else
    if(find_player_card killed_id card bd = 2) then {killed with card_two= ""} else killed in
  replace_player killed_id killed bd

let assassinate killer_id killed_id bd card=
  let killer_paid = change_money killer_id bd (-3) in
  turnover_card killed_id killer_paid card

let coup couper_id couped_id bd card=
  let couper_paid = change_money couper_id bd (-7) in
  turnover_card couped_id couper_paid card

let view_four exchanger_id bd=
  let pair= deal_pair bd.current_deck in
  fst (snd pair) :: snd (snd pair) :: get_cards exchanger_id bd

let exchange exchanger_id bd card1 card2=
  let exchanger= find_player exchanger_id bd in
  {exchanger with card_one=card1; card_two=card2}

