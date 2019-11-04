
type status = Deck | FaceUp | FaceDown 
type character = Duke | Assassin | Contessa | Captain | Ambassador
type card = character * status 

type deck = card list 
type money_pool = int (* denotes how much money is in the pool*)

let rec shuffle deck n = 
  QCheck.Gen.(generate1 (shuffle_l deck))
