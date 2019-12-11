(* type deck = Deck of Deck.t | Not_deck *)
exception InvalidPlayer of string
exception InvalidCard of string

(** The abstraction type for the state of a board(the game state)*)
type t

(** The abstraction type for a player*)
type player

type result = Legal of t | Illegal | NoMoney | MoneyOverflow

(** [next_turn bd] is [bd] with the current turn finished and the next player's
    turn beginning.*)
val next_turn : t -> t

(** [current_player bd] is the player whose turn it is currently in [bd].*)
val current_player: t->player

(** [current_player_id bd] is the id/name of the player whose turn it currently
    is in [bd].*)
val current_player_id: t -> string

(** [everyones_info bd] is a string that is meant to be printed during 
    gameplay to display the information of all the players of [bd]. Logic is in
    [everyones_info_helper accu player_list bd] because when called in main.ml
    the current players are not public.*)
val everyones_info: t-> string

(** [everyones_info_hidden bd] is [everyones_info bd] but will instead keep the 
    card types of noncurrent players hidden. This is necessary to play the game 
    as intended. [everyones_info bd] should not be used in the final release.*)
val everyones_info_hidden : t-> string

(** [turn_info player bd] is the relevant information [player] will be given
    about themselves during a turn in [bd], which includes what cards they have,
    if they're facedown or not, and the amount of money they have. *)
val turn_info : player->t->string

(** [is_ai player] is true if [player] is an ai.*)
val is_ai : player -> bool

(** [init_board deck num_players] is the first game state of a game generated
    with [deck] and a [num_players] number of players.*)
val init_board : Deck.t-> int-> string -> t

(** [check_bank player_id int bd] is true iff the player with id of [player_id]
    in [bd] has at least [cash] coins.*)
val check_bank : string->int->t->bool

(** [get_cards player bd] is the list of cards that the player identified by
    [player] controls in [bd]. If [player] is not a player in [bd] then raise
    an InvalidPlayer exception. *)
val get_cards : string -> t -> Deck.card list

(** [steal stealer_id stolen_id bd] is the result of [stealer_id] stealing from
    [stolen_id] in [bd]. If [stolen_id] has no money to steal then
    [steal stealer_id stolen_id bd = NoMoney], otherwise it is a legal result
    carrying [bd] after this steal has taken place(of either one or two coins) 
    or an illegal result.*)
val steal : string-> string-> t-> result

(** [assassinate killer_id killed_id bd card_id] is a legal result of [bd] after
    [killer_id] assassinates [killed_id]'s [card_id] in [bd], or an illegal
    result. Requires: [card_id] is a card of [killed_id]. *)
val assassinate : string->string->t->string->result

(** [coup couper_id couped_id bd card_id] is a legal result of [bd] after
    [couper_id] coups [couped_id]'s [card_id] in [bd], or an illegal
    result. Requires: [card_id] is a card of [couped_id]. *)
val coup : string->string->t->string->result

(** [income player_name bd] is a legal result of [bd] after [player_name] takes
    an income(1 coin) in [bd], or an illegal result if something goes wrong.*)
val income : string->t->result

(** [foreign_aid player_name bd] is a legal result of [bd] after [player_name] 
    takes foreign aid(2 coins) in [bd], or an illegal result if something goes 
    wrong.*)
val foreign_aid : string->t->result

(** [tax player_name bd] is a legal result of [bd] after [player_name] takes
    a tax(3 coins) in [bd], or an illegal result if something goes wrong.*)
val tax : string->t->result

(** [extract_legal b] is the board contained by [b]. If [b] is illegal then
    [extract_legal b] is an empty, placeholder board. *)
val extract_legal : result -> t

(** [get_money bd player] is the amount of money that the player identified by
    [player] has in [bd]. If [player] is not a player in [bd] then raise an
    invalid player exception. *)
val get_money : t->string->int

(** [get_host bd] is the player with the first turn in [bd].*)
val get_host : t->player

(** [check_id player_id bd] is true if [player_id] is a player in [bd].*)
val check_id : string->t->bool

(** [find_facedown player_id bd] is the first facedown card of [player_id] in
    [bd]. Fails if [player_id] has no facedown cards.*)
val find_facedown : string->t->Deck.card

(** [get_player_id player] is [player]'s id, i.e. [player]'s name.*)
val get_player_id : player -> string

(** [check_faceup card_list] is true if all cards of [card_list] are faceup.
    Requires: [List.length card_list=2].*)
val check_faceup : Deck.card list -> bool

(**[is_alive player bd] is true if [player] is alive in [bd], false otherwise *)
val is_alive : t->string-> bool

(** [cards player_id bd] are the cards that [player_id] has face down in [bd].*)
val cards : string -> t -> string

(** [check_pool bd] is the amount of money avalaible in the center in [bd].*)
val check_pool : t->int

(** [player_names bd] is a list of the string representations(names or ids) of 
    the players of [bd].*)
val player_names : t -> string list

(** [can_act actor_name action_name bd] is [true] if [actor_name] can perform
    [action_name] in [bd]. Raises an InvalidPlayer exception if [actor_name] is
    not a valid player in [bd].*)
val can_act : string -> string -> t -> bool

(** [turnover_card killed_id bd card] is [bd] with [killed_id]'s copy of [card]
    turned from facedown to faceup. If [killed_id] does not contain a copy of
    [card] then raises an InvalidCard exception. Raises a InvalidPlayer
    exception if [killed_id] is not a player of [bd].*)
val turnover_card : string -> t -> string -> t

(** [view_four exchanger_id bd] is a pair of the cards [exchanger_id] would
    see in an exchange, and the deck after two cards are drawn from [bd]'s deck
    to be viewed by [exchanger_id]. Raises an InvalidPlayer exception if
    [exchanger_id] is not a valid player in [bd].*)
val view_four : string -> t -> (Deck.card list * Deck.t)

(** [exchange exchanger_id bd card1 card2 deck discards] is a legal result of
    [bd] after [exchanger_id] exchanges their cards for [card1] and [card2],
    and has chosen to discard [discards] back into [deck] in [bd], or an
    illegal result if an exception is raised in execution.*)
val exchange : 
  string -> t -> Deck.card -> Deck.card -> Deck.t -> (Deck.card list) -> result

(** [has_both player_id bd] is true if [player_id] has no faceup cards in [bd].
    Raises an InvalidPlayer exception if [player_id] is not a player in [bd].*)
val has_both : string -> t -> bool

(** [make_player_lie bd] is [bd] after the current player of [bd] has lied*)
val make_player_lie : t -> t

(** [can_block actor_name action_name bd] is true if [actor_name] is able to
    block [action_name] in bd. Raises an InvalidPlayer exception if [actor_name]
    is not a valid player in [bd].*)
val can_block : string -> string -> t -> bool

(** [id_is_ai id b] is true if the player with name [id] in [b] is an ai.*)
val id_is_ai : string -> t -> bool

val get_deck: t -> Deck.t
(** [alive_players lst] is the players in [lst] that do not have two faceup
    cards.*)
val alive_players : player list -> player list

(** [victory bd] is the pair of whether a player has won in [bd], and the id of 
    the player who won. If no player has won then the second member of the pair
    is the host id.*)
val victory : t -> (bool*string)

(** [draw_new bd players] is [bd] with [player] putting their [card] back into
    the deck and drawing a new card to replace it. Requires: [player] is a 
    player in [bd], and that [card] is a facedown card controlled by [player].*)
val draw_new : t -> string -> string -> t

(** [which_block bd player] is the card title [player] uses to block a steal in 
    [bd]. Requires: [player] is able to block a steal in [bd]*)
val which_block_steal : t -> string -> string

(** [change_dead bd deck] is [bd] with [deck] as its deck rather than the deck
    it had.*)
val change_deck : t -> Deck.t -> t

(** [set_dead bd] is [bd] with any players of [bd] who have two faceup cards
    set to be dead.*)
val set_dead : t -> t

(** [turn_info_hidden player] is the information the human player will see about
    themselves on hard mode, which prevents them from being able to see their 
    cards. *)
val turn_info_hidden : player -> string

val get_mode : t -> string