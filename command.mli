type object_phrase = string list

type command = 
  | Quit
  | Steal of object_phrase
  | Assassinate of object_phrase
  | Coup of object_phrase
  | Income
  | Foreign_Aid 
  | Tax
  | Exchange
  | Block 
  | Continue

exception Empty

exception Malformed

(* [parse str] gives a command associated with head of the list from 
   [make_word_list str]. 
   Raises an exception if the string is empty or doesn't match the correct 
   structure, which is based individually on each instruction.  *)
val parse : string -> command

(* [parse_block str] either gives the Block command if the head of the list 
   made by [make_word_list str] is "block" or it continues. 
   Raises an exception if the head is empty or if the player types block 
   followed by any other word. *)
val parse_block: string -> command