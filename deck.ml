
type status = Deck | FaceUp | FaceDown 
type character = Duke | Assassin | Contessa | Captain | Ambassador
type card = character * status 

type t = card list 
type money_pool = int (* denotes how much money is in the pool*)
exception InvalidCard of string


let shuffle deck = 
  QCheck.Gen.(generate1 (shuffle_l deck))

let shuffle_in deck card =
  QCheck.Gen.(generate1 (shuffle_l (card::deck)))

let init_deck a = 
  shuffle [(Duke, Deck);(Duke, Deck);(Duke, Deck); 
           (Assassin, Deck);(Assassin, Deck);(Assassin, Deck);
           (Contessa, Deck);(Contessa, Deck);(Contessa, Deck);
           (Captain, Deck);(Captain, Deck);(Captain, Deck);
           (Ambassador, Deck);(Ambassador, Deck);(Ambassador, Deck)]

let draw deck = 
  match deck with 
  | [] -> failwith "This won't happen (draw failed)"
  | h :: t -> (h,t) 

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
  | FaceDown -> "in play"
  | FaceUp -> "out of play"

let get_action card =
  match fst card with
  |Duke -> "Tax"
  |Assassin -> "Assassinate"
  |Contessa -> "None"
  |Captain -> "Steal"
  |Ambassador -> "Exchange" 

let get_blocks card = 
  match fst card with 
  |Duke -> "Foreign Aid"
  |Assassin -> "None"
  |Contessa -> "Assassinate"
  |Captain -> "Steal"
  |Ambassador -> "Steal" 

let is_facedown card =
  snd card = FaceDown

let is_faceup card =
  snd card = FaceUp

let set_status card status=
  (fst card, status)

let name_to_card card_id =
  let card= String.trim (String.lowercase_ascii card_id) in
  match card with
  |"duke"->Duke
  |"assassin"->Assassin
  |"contessa"->Contessa
  |"captain"-> Captain
  |"ambassador"-> Ambassador
  |_->raise(InvalidCard card)

