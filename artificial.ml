open Board
type mood = Random | Other
type actions = Income | ForeignAid | Tax | Steal of string | Assassinate of string | Coup of string | Exchange
let actions = ["Income"; "Foreign Aid"; "Tax"; "Assassinate"; "Steal"; "Exchange"; "Coup"]
let income_actions = ["Income"; "Foreign Aid"; "Tax"; "Steal"]
let hostile_actions = ["Steal"; "Assassinate"]
let basic_actions = income_actions@hostile_actions

type t= {
  id: string;
  cards: Deck.card list;
  money: int;
  personality : mood;
  board: Board.t;
  players: string list;
}

let random_elt action_lst=
  let elt= Random.int(List.length action_lst) in
  List.nth action_lst elt

let can_steal ai=
  let moneys= List.map (get_money ai.board) ai.players in
  let more_than_one= List.filter (fun x -> x>0) moneys in
  List.length more_than_one >0


let rec rand_target ai=
  let target = random_elt ai.players in
  if(check_bank target 1 ai.board) then target else rand_target ai


let random_income ai=
  match random_elt income_actions with
  |"Income"->Income
  |"Foreign Aid"->ForeignAid
  |"Tax"->Tax
  |"Steal"->Steal (random_elt ai.players)
  |_->failwith "Impossible"

let rec random_basic ai=
  let target = random_elt ai.players in
  match random_elt basic_actions with
  |"Income"->Income
  |"Foreign Aid"->ForeignAid
  |"Tax"->Tax
  |"Steal"->if(can_steal ai) then Steal (rand_target ai) else random_basic ai
  |"Assassinate"->Assassinate target
  |_->failwith "impossible"

let rec random_hostile ai=
  let target = random_elt ai.players in
  match random_elt hostile_actions with
  |"Steal"->if(can_steal ai) then Steal (rand_target ai) else random_hostile ai
  |"Assassinate"->Assassinate target
  |_->failwith "impossible"



let new_ai player_id bd=
  let card_list =get_cards player_id bd in
  let facedown_cards= List.filter Deck.is_facedown card_list in
  {
    id= player_id;
    cards= facedown_cards;
    money= get_money bd player_id;
    personality= Random;
    board= bd;
    players= List.filter (fun x -> x<>player_id) (player_names bd);
  }


let action ai=
  if(check_pool ai.board>3&&ai.money<4) then random_income ai else
  if(check_pool ai.board<3&&ai.money<3) then Steal (random_elt ai.players) else
  if(ai.money>7) then Coup (random_elt ai.players) else
  if(check_pool ai.board>3&&ai.money>3) then random_basic ai else
  if(check_pool ai.board<4&&ai.money>3) then random_hostile ai else
    Assassinate (random_elt ai.players)

let turn player_id bd=
  let ai= new_ai player_id bd in
  action ai

(*
let action_to_string action =
  match action with
  |Income->"Income"
  |ForeignAid->"Foreign Aid"
  |Tax->"Tax"
  |Steal target ->"Steal "^target
  *)
