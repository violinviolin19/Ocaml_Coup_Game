
(* type deck = Deck of Deck.t | Not_deck *)
exception InvalidPlayer of string
exception InvalidCard of string

(** The abstraction type for the state of a board(the game state)*)
type t

(** The abstraction type for a player*)
type player

type result = Legal of t | Illegal

(** Changes the board from one player's turn to the next*)
val next_turn : t -> t

(** The player whose turn it currently is in the board*)
val current_player: t->player

val current_player_id: t -> string

val turn_info : player->t->string

val is_ai : player -> bool

val init_board : Deck.t-> int-> t

val check_bank : string->int->t->bool

val get_cards : string -> t -> Deck.card list

val steal : string-> string-> t-> result

val assassinate : string->string->t->string->result

val coup : string->string->t->string->t

val income : string->t->result

val foreign_aid : string->t->result

val tax : string->t->result

val extract_legal : result -> t

val get_money : string->t->int

val get_host : t->player

val check_id : string->t->bool

val find_facedown : string->t->Deck.card



