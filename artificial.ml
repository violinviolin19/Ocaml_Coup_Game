open Board
type mood = Random | Other
type actions = Income | ForeignAid | Tax | Steal of string | Assassinate of string | Coup of string | Exchange
let actions = ["Income"; "Foreign Aid"; "Tax"; "Assassinate"; "Steal"; "Exchange"; "Coup"]
let income_actions = ["Income"; "Foreign Aid"; "Tax"; "Steal"]
let hostile_actions = ["Steal"; "Assassinate"]
let random_nums = [0;1;2]
let basic_actions = ["Income"; "Foreign Aid"; "Tax"; "Assassinate"; "Steal"]

type t= {
  id: string;
  cards: Deck.card list;
  money: int;
  personality : mood;
  board: Board.t;
  players: string list;
}

(** [random_elt action_lst] is a random element chosen from [action_lst]. The
    element is chosen using Ocaml's Random library and thus pseudorandom.*)
let random_elt action_lst=
  Random.self_init ();
  let elt= Random.int(List.length action_lst) in
  List.nth action_lst elt

(** [can_steal ai] is true if [ai] is able to steal from another player.*)
let can_steal ai=
  let moneys= List.map (get_money ai.board) ai.players in
  let more_than_one= List.filter (fun x -> x>0) moneys in
  List.length more_than_one >0

(** [rand_target ai] is the id of a random player in [ai] that has at least one
    money in their bank.*)
let rec rand_target ai=
  let target = random_elt ai.players in
  if(check_bank target 1 ai.board) then target else rand_target ai

(** [random_income ai] is a random action that will increase increase the money
    of [ai]. If there are possible targets for [ai] to steal from then 
    this includes stealing as an income action.*)
let rec random_income ai=
  match random_elt income_actions with
  |"Income"->Income
  |"Foreign Aid"->ForeignAid
  |"Tax"->Tax
  |"Steal"-> if(can_steal ai) then Steal (rand_target ai) else random_income ai
  |_->failwith "Impossible"

(** [random_basic ai] is a random non-coup action that [ai] can take. If there 
    are possible targets for [ai] to steal from then this includes stealing as
    a basic action.*)
let rec random_basic ai=
  let target = random_elt ai.players in
  match random_elt basic_actions with
  |"Income"->Income
  |"Foreign Aid"->ForeignAid
  |"Tax"->Tax
  |"Steal"->if(can_steal ai) then Steal (rand_target ai) else random_basic ai
  |"Assassinate"->Assassinate target
  |_->failwith "impossible"

(** [random_hostile ai] is a random action that negatively effects another 
    player, not including a coup. If there are possible targets for [ai] to 
    steal from then this includes stealing as a hostile action.*)
let rec random_hostile ai=
  let target = random_elt ai.players in
  match random_elt hostile_actions with
  |"Steal"->if(can_steal ai) then Steal (rand_target ai) else random_hostile ai
  |"Assassinate"->Assassinate target
  |_->failwith "impossible"

(** [new_ai player_id bd] is a new ai generated from the player identified by
    [player_id] in [bd].*)
let new_ai player_id bd=
  let card_list =get_cards player_id bd in
  let facedown_cards= List.filter Deck.is_facedown card_list in
  let players= List.filter (fun x -> x<>player_id) (player_names bd) in
  let players= List.filter (is_alive bd) players in 
  {
    id= player_id;
    cards= facedown_cards;
    money= get_money bd player_id;
    personality= Random;
    board= bd;
    players= players;
  }

(** [action ai] is the action that [ai] chooses to take.*)
let action ai=
  if(check_pool ai.board>3&&ai.money<3) then random_income ai else
  if(check_pool ai.board<3&&ai.money<3) then Steal (random_elt ai.players) else
  if(ai.money>7) then Coup (random_elt ai.players) else
  if(check_pool ai.board>3&&ai.money>2) then random_basic ai else
  if(check_pool ai.board<4&&ai.money>3) then random_hostile ai else
    Assassinate (random_elt ai.players)

(** [turn player_id bd] is the action that the non-player-controlled [player_id]
    chooses to take in [bd].*)
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

(** [should_challenge ai_id action target bd] is true if the ai identified by
    [ai_id] will challenge the [action] directed towards [target] in [bd]. [target] is
    "" if there is no target of [action].*)
let should_challenge ai_id action target bd=
  let ai= new_ai ai_id bd in
  Random.self_init ();
  if(Board.has_both ai.id ai.board && action^" "^target="Assassinate "^ai.id) 
  then false else
  if(action="Assassinate "^ai.id) then true else
  if(random_elt random_nums = 0) then true else false

(** [can_block_steal card_list] is true if a player with [card_list] as their
    facedown cards can block a steal.*)
let can_block_steal card_list =
  let cards= List.map (Deck.get_name) card_list in
  List.mem "Captain" cards || List.mem "Ambassador" cards

(** [can_block_assassinate card_list] is true if a player with [card_list] as
    their facedown cards can block an assassination.*)
let can_block_assassinate card_list =
  let cards= List.map (Deck.get_name) card_list in
  List.mem "Contessa" cards 

(** [should_block ai_id bd action target] is true if the non-player-controlled
    [ai_id] will choose to block the [action] directed at [target] in [bd].*)
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

(** [should_any_block ids bd action target] is a pair of whether any of [ids] 
    will choose to block the [action] directed towards [target] in [bd], and
    the member of [ids] that will do this blocking. If none of [ids] chooses to
    block then [should_any_block ids bd action target] is (false,target).*)
let should_any_block ids bd action target=
  let rec blocks = function
    |[]->(false, target)
    |h::t -> if(should_block h bd action target) then (true, h) else blocks t in
  blocks ids

(** [should_any_challenge ids bd action target] is a pair of whether any of
    [ids] will choose to challenge the [action] directed towards [target] in 
    [bd], and the member of [ids] that will do this challenging. If none of 
    [ids] chooses to challenge then [should_any_challenge ids bd action target] 
    is (false,target).*)
let should_any_challenge ids bd action target=
  let rec challenges = function
    |[]->(false, target)
    |h::t-> if(should_challenge h action target bd) then (true,h)
      else challenges t in
  challenges ids

(** should_challenge_block id action actor bd] is true if the 
    non-player-controlled [id] will challenge the block to [actor]'s choice to
    [action] in [bd].*)
let should_challenge_block id action actor bd=
  Random.self_init ();
  match String.capitalize_ascii action with
  |"Steal"
  |"Assassinate"
  |"Foreign Aid"-> if(actor=id) then true else false
  |_-> failwith "Not a blockable action"

(** [any_challenge_block ids bd action actor] is a pair of whether any of [ids]
    will choose to challenge the block towards the [action] performed by [actor]
    in [bd], and the member of [ids] that will do this challenging. If none of 
    [ids] chooses to challenge then [any_challenge_block ids bd action actor] 
    is (false,target).*)
let any_challenge_block ids bd action actor=
  let rec blocks = function
    |[]->(false,actor)
    |h::t -> if(should_challenge_block h action actor bd) then (true,h) 
      else blocks t in
  blocks ids