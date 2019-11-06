type object_phrase = string list

type command = 
  | Quit
  | Steal of object_phrase
  | Assassinate of object_phrase
  | Coup of object_phrase
  | Income
  | Foreign_Aid 
  | Tax

exception Empty

exception Malformed

let parse str =
  if String.trim str = "" then raise Empty 
  else let word_list = String.split_on_char ' ' (String.trim str) |> 
                       List.filter (fun str -> str <> "") in 
    match word_list with 
    | [] -> raise Empty 
    | h::t -> begin 
        match h with 
        | "steal" -> if t = [] then raise Malformed else Steal t
        | "assassinate" -> if List.length t = 1 then Assassinate t else raise Malformed
        | "coup" -> if t = [] then raise Malformed else Coup t
        | "income" -> if t = [] then Income else raise Malformed 
        | "foreign" -> if t = ["aid"] then Foreign_Aid else raise Malformed
        | "tax" -> if t = [] then Tax else raise Malformed
        | "quit" -> if t = [] then Quit else raise Malformed
        | _ -> raise Malformed
      end