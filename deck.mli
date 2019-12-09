type status= Deck| FaceUp | FaceDown 
type character = Duke | Assassin | Contessa | Captain | Ambassador
type card = character * status 

type t= card list
type money_pool

exception InvalidCard of string

(** [shuffle deck] gives a random permutation of [deck]. *)
val shuffle : t -> t

(** [shuffle deck] gives a random permutation of [deck] after [card] is shuffled
    back in. *)
val shuffle_in : t-> card -> t

(** [init_deck a] is a randomly generated deck with three of each character.*)
val init_deck : 'a->t

(** [draw deck] is a tuple of with the first element being the drawn card and
    the second element being the rest of [deck] after this draw. Requires: 
    [deck] is not empty. *)
val draw : t -> card*t

(** [draw deck] is a tuple of a list containing the two cards drawn from [deck],
    and the remainder of [deck] after this draw. Requires: [deck] has at least
    two cards in it.*)
val draw2: t -> card list * t

(** [get_deck deck] gives a list of all the cards in the deck in the 
    correct order. *)
val get_deck : t->t

(** [get_name card] is a string representation of the character [card]
    represents.*)
val get_name : card -> string

(** [get_status card] is/returns a string representation of [card].*)
val get_status : card -> string

(** [get_action card] is the action that [card] can perform. [get_action card]
    is "None" if [card] can not perform an action. The action is returned as
    a string*)
val get_action : card -> string

(** [get_blocks card] is the action that [card] can block. [get_blocks card] is
    "None" if [card] can not block an action.*)
val get_blocks : card -> string

(** [is_facedown card] is true if [card] is facedown.*)
val is_facedown : card -> bool

(** [is_faceup card] is true if [card] is faceup.*)
val is_faceup : card -> bool

(** [set_status card status] is [card] with but with a status of [status].
    if [card] already has a status of [status] then return [card].*)
val set_status : card -> status -> card

(** [name_to_card card_id] is the character represented by [card_id]. [card_id]
    can have any capitalization and any amount of whitespace so long as it is at
    the beginning or end of [card_id]. Raises an InvalidCard exception if
    [card_id] does not represent a valid card.*)
val name_to_card : string -> character