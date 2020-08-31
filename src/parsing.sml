signature PARSING =
sig

  exception Error of string

  val error: Region.Region * string -> 'a
  val wrongTokenError : Region.Region * Token.Token * Token.Token -> 'a 
  val wrongTokenError': Region.Region * Token.Token list * Token.Token -> 'a 

end (* signature PARSING *)

structure Parsing :> PARSING =
struct
 
  exception Error of string

  fun error (r, msg) = raise Error (Region.wrap (r, msg))

  fun wrongTokenError (r, tExpected, tFound) = 
        error (r, "Expected " ^ Token.toString (tExpected) ^ ", found "
	       ^ Token.toString (tFound))

  fun tokensToString ([t]) = Token.toString (t)
    | tokensToString ([t1, t2]) = Token.toString (t1) ^ " or " ^ Token.toString (t2)
    | tokensToString (t :: ts) = Token.toString (t) ^ ", " ^ tokensToString (ts)

  fun wrongTokenError' (r, tsExpected, tFound) = 
        error (r, "Expected " ^ tokensToString (tsExpected) ^ ", found "
	       ^ Token.toString (tFound))

end (* structure Parsing *)
