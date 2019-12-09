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

(* [make_word_list str] splits str into a list of its individual words, ignoring
   whitespace or empty strings. *)
let make_word_list str = 
  if String.trim str = "" then raise Empty 
  else String.split_on_char ' ' (String.trim str) |> 
       List.filter (fun str -> str <> "") 


let parse str =
  let word_list = make_word_list str in 
  match word_list with 
  | [] -> raise Empty 
  | h::t -> begin 
      match h with 
      | "steal" -> if t = [] then raise Malformed else Steal t
      | "assassinate" -> if List.length t = 1 then Assassinate t else 
          raise Malformed
      | "coup" -> if t = [] then raise Malformed else Coup t
      | "income" -> if t = [] then Income else raise Malformed 
      | "foreign" -> if t = ["aid"] then Foreign_Aid else raise Malformed
      | "tax" -> if t = [] then Tax else raise Malformed
      | "quit" -> if t = [] then Quit else raise Malformed
      | "exchange" -> if t=[] then Exchange else raise Malformed
      | "block" -> if t=[] then Block else raise Malformed
      | _ -> raise Malformed
    end


let parse_block str = 
  let word_list = make_word_list str in 
  match word_list with 
  | [] -> raise Empty 
  | h::t -> begin
      match h with 
      | "block" -> if t=[] then Block else raise Malformed
      | "continue" -> Continue
      | _ -> raise Malformed
    end