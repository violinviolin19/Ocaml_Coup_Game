type actions = Income | ForeignAid | Tax | Steal of string 
             | Assassinate of string | Coup of string | Exchange

type t

(** [turn player_id bd] is the action that the non-player-controlled [player_id]
    chooses to take in [bd].*)
val turn : string -> Board.t -> actions

(** [should_block ai_id bd action target] is true if the non-player-controlled
    [ai_id] will choose to block the [action] directed at [target] in [bd].*)
val should_block : string -> Board.t -> string -> string -> bool

(** [should_any_block ids bd action target] is a pair of whether any of [ids] 
    will choose to block the [action] directed towards [target] in [bd], and
    the member of [ids] that will do this blocking. If none of [ids] chooses to
    block then [should_any_block ids bd action target] is (false,target).*)
val should_any_block : string list -> Board.t -> string -> string-> bool*string

(** [should_any_challenge ids bd action target] is a pair of whether any of
    [ids] will choose to challenge the [action] directed towards [target] in 
    [bd], and the member of [ids] that will do this challenging. If none of 
    [ids] chooses to challenge then [should_any_challenge ids bd action target] 
    is (false,target).*)
val should_any_challenge : 
  string list -> Board.t -> string -> string -> bool*string

(** [any_challenge_block ids bd action actor] is a pair of whether any of [ids]
    will choose to challenge the block towards the [action] performed by [actor]
    in [bd], and the member of [ids] that will do this challenging. If none of 
    [ids] chooses to challenge then [any_challenge_block ids bd action actor] 
    is (false,target).*)
val any_challenge_block : 
  string list -> Board.t -> string -> string -> bool*string