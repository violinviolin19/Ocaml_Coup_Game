
type status = Deck | FaceUp | FaceDown 
type character = Duke | Assassin | Contessa | Captain | Ambassador
type card = character * status 

type t = card list 
type money_pool = int (* denotes how much money is in the pool*)
exception InvalidCard of string


(** [shuffle deck] gives a random permutation of [deck]. *)
let shuffle deck = 
  QCheck.Gen.(generate1 (shuffle_l deck))

(** [shuffle deck] gives a random permutation of [deck] after [card] is shuffled
    back in. *)
let shuffle_in deck card =
  QCheck.Gen.(generate1 (shuffle_l (card::deck)))

let init_deck a = 
  shuffle [(Duke, Deck);(Duke, Deck);(Duke, Deck); 
           (Assassin, Deck);(Assassin, Deck);(Assassin, Deck);
           (Contessa, Deck);(Contessa, Deck);(Contessa, Deck);
           (Captain, Deck);(Captain, Deck);(Captain, Deck);
           (Ambassador, Deck);(Ambassador, Deck);(Ambassador, Deck)]

(** [draw deck] fulfills the function of drawing a card by returning
    a 2-tuple with the first element being the drawn card and the second
    being the rest of the deck *)
let draw deck = 
  match deck with 
  | [] -> failwith "This won't happen (draw failed)"
  | h :: t -> (h,t) 

(** [draw deck] fulfills the function of drawing a card by returning 
    a 3-tuple with the first two elements being the drawn cards and the
    third element being the rest of the deck. This is to be used 
    when invoking the Ambassador's swap command. *)
let draw2 deck =
  match deck with 
  | h :: h2 :: t -> ([h;h2],t)
  | _ -> failwith "This won't happen (draw2 failed)"

(** [to_list_helper deck lst] gives a list of all the cards in the deck
    in reverse order. *)
let rec to_list_helper deck lst = 
  match deck with 
  | [] -> lst 
  | h::t -> to_list_helper t (h::lst)

(** [get_deck deck] gives a list of all the cards in the deck in the 
    correct order. *)
let get_deck deck = 
  List.rev(to_list_helper deck [])

(** [get_name card] is a string representation of the character [card]
    represents.*)
let get_name card : string= 
  match fst card with 
  |Duke -> "Duke"
  |Assassin -> "Assassin"
  |Contessa -> "Contessa"
  |Captain -> "Captain"
  |Ambassador -> "Ambassador"

(** [get_status card] is/returns a string representation of [card].*)
let get_status card = 
  match snd card with 
  | Deck -> "in the deck"
  | FaceDown -> "in play"
  | FaceUp -> "out of play"

(** [get_action card] is the action that [card] can perform. [get_action card]
    is "None" if [card] can not perform an action. The action is returned as
    a string*)
let get_action card =
  match fst card with
  |Duke -> "Tax"
  |Assassin -> "Assassinate"
  |Contessa -> "None"
  |Captain -> "Steal"
  |Ambassador -> "Exchange" 

(** [get_blocks card] is the action that [card] can block. [get_blocks card] is
    "None" if [card] can not block an action.*)
let get_blocks card = 
  match fst card with 
  |Duke -> "Foreign Aid"
  |Assassin -> "None"
  |Contessa -> "Assassinate"
  |Captain -> "Steal"
  |Ambassador -> "Steal" 

(** [is_facedown card] is true if [card] is facedown.*)
let is_facedown card =
  snd card = FaceDown

(** [is_faceup card] is true if [card] is faceup.*)
let is_faceup card =
  snd card = FaceUp

(** [set_status card status] is [card] with but with a status of [status].
    if [card] already has a status of [status] then return [card].*)
let set_status card status=
  (fst card, status)


(** [name_to_card card_id] is the character represented by [card_id]. [card_id]
    can have any capitalization and any amount of whitespace so long as it is at
    the beginning or end of [card_id]. Raises an InvalidCard exception if
    [card_id] does not represent a valid card.*)
let name_to_card card_id =
  let card= String.trim (String.lowercase_ascii card_id) in
  match card with
  |"duke"->Duke
  |"assassin"->Assassin
  |"contessa"->Contessa
  |"captain"-> Captain
  |"ambassador"-> Ambassador
  |_->raise(InvalidCard card)