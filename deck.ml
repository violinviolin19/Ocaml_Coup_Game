
type status = Deck | FaceUp | FaceDown 
type character = Duke | Assassin | Contessa | Captain | Ambassador
type card = character * status 

type t = card list 
type money_pool = int (* denotes how much money is in the pool*)

(** [shuffle deck] gives a random permutation of [deck]. *)
let shuffle deck = 
  QCheck.Gen.(generate1 (shuffle_l deck))

(** [draw deck] gives the top card of the deck. *)
let draw deck = 
  match deck with 
  | [] -> failwith "This won't happen (draw failed)"
  | h :: t -> (h,t) 

(** [draw deck] gives the first two cards of the deck. This is to be used 
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

let get_name card = 
  match fst card with 
  |Duke -> "Duke"
  |Assassin -> "Assassin"
  |Contessa -> "Contessa"
  |Captain -> "Captain"
  |Ambassador -> "Ambassador"

let get_status card = 
  match snd card with 
  | Deck -> "in the deck"
  | FaceUp -> "in play"
  | FaceDown -> "out of play"

let set_status card status=
  (fst card, status)