open Board
type mood = Random | Other
type actions = Income | ForeignAid | Tax | Steal of string | Assassinate of string | Coup of string | Exchange
let actions = ["Income"; "Foreign Aid"; "Tax"; "Assassinate"; "Steal"; "Exchange"; "Coup"]
let income_actions = ["Income"; "Foreign Aid"; "Tax"; "Steal"]
let hostile_actions = ["Steal"; "Assassinate"]
let random_nums = [0;1]
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
  Random.self_init ();
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
  if(check_pool ai.board>3&&ai.money<3) then random_income ai else
  if(check_pool ai.board<3&&ai.money<3) then Steal (random_elt ai.players) else
  if(ai.money>7) then Coup (random_elt ai.players) else
  if(check_pool ai.board>3&&ai.money>2) then random_basic ai else
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

let should_challenge ai_id action target bd=
  let ai= new_ai ai_id bd in
  Random.self_init ();
  if(Board.has_both ai.id ai.board && action^" "^target="Assassinate "^ai.id) then false else
  if(action="Assassinate "^ai.id) then true else
  if(Random.int(1)=1) then true else false

let can_block_steal card_list =
  let cards= List.map (Deck.get_name) card_list in
  List.mem "Captain" cards || List.mem "Ambassador" cards

let can_block_assassinate card_list =
  let cards= List.map (Deck.get_name) card_list in
  List.mem "Contessa" cards 

let should_block ai_id bd action target=
  let ai= new_ai ai_id bd in
  Random.self_init ();
  match String.capitalize_ascii action with
  |"Steal"-> begin
      if(target=ai_id&& can_block_steal ai.cards) then true else
      if(target=ai_id) then random_elt random_nums = 0 else false
    end
  |"Assassinate"-> begin
      if(target=ai_id && can_block_assassinate ai.cards) then true else 
      if(target=ai_id && not (Board.has_both ai_id ai.board)) then true
      else false
    end
  |"Foreign Aid"-> random_elt random_nums = 0
  |_ -> failwith "Not a blockable action"

let should_any_block ids bd action target=
  let rec blocks = function
    |[]->(false, target)
    |h::t -> if(should_block h bd action target) then (true, h) else blocks t in
  blocks ids

let should_any_challenge ids bd action target=
  let rec challenges = function
    |[]->(false, target)
    |h::t-> if(should_challenge h action target bd) then (true,h) else challenges t in
  challenges ids