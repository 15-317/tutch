(* $Id: parse-proof.sml,v 1.1 2000/10/17 22:23:10 abel Exp $ *)

signature PARSEPROOF =
sig

  val parseDecl : Lexer.front -> (string * Syntax.Exp) * Lexer.front
  val parseProof: Lexer.front -> Syntax.Proof * Lexer.front

end; (* signature PARSEPROOF *)


structure ParseProof :> PARSEPROOF =
struct

  (*  parseDecl (f) = ((name, A), f') 
  *) 
  fun parseDecl (IStream.Cons ((Token.ID (_, name), r), s)) =
        parseDecl1 (name, IStream.expose s)
    | parseDecl (IStream.Cons ((t, r), s)) =
        Parsing.error (r, "Expected identifier, found " ^ Token.toString t)

  and parseDecl1 (name, IStream.Cons ((Token.COLON, r), s)) =
      let 
	  val (A, f') = ParseExp.parseExp (IStream.expose s)
      in ((name, A), f') end
    | parseDecl1 (name, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.COLON, t)


  (*  parseProof (f) = (P, f')
  *)  
  fun parseProof (f as IStream.Cons ((t, r), s)) = 
      let val (POpt, f') = parseProof' (f)
      in case POpt of
	  NONE => Parsing.error (r, "Expected proof, found " ^ Token.toString (t))
	| SOME P => (P, f')
      end

  (*  parseProof' (f) = (POpt, f')
  *)
  and parseProof' (IStream.Cons ((Token.SEMICOLON, r), s)) =
        parseProof' (IStream.expose s)
    | parseProof' (IStream.Cons ((Token.LBRACKET, r), s)) = 
	parseFrame (parseHyps (IStream.expose s))
    | parseProof' (f) = 
	let val (AOpt, f') = ParseExp.parseExpOpt (f)
	in case AOpt of
	    NONE => (NONE, f')
	  | SOME A => let val (POpt, f'') = parseProof'1 (f')
		      in case POpt of
			  NONE => (SOME (Syntax.mkFinal (Syntax.mkLine (A))), f'')
			| SOME P => (SOME (Syntax.mkStep (Syntax.mkLine (A), P)), f'')
		      end
	end

  and parseProof'1 (IStream.Cons ((Token.SEMICOLON, r), s)) =
        parseProof' (IStream.expose s)
    | parseProof'1 (f as IStream.Cons ((t, r), s)) = (NONE, f)

  (*  parseHyps (f) = (Hs, f') 
  *)
  and parseHyps (f as IStream.Cons ((t, r), s)) = (case parseHyps' (f) of
          (NONE, f') => Parsing.error (r, "Expected hypothesis, found "
                        ^Token.toString (t))
	| (SOME Hs, f') => (Hs, f')) 

  (*  parseHyps' (f) = (HsOpt, f') 
  *)
  and parseHyps' (f) =
        (case ParseExp.parseExpOpt (f) of
	     (NONE, f') => (NONE, f')
	   | (SOME (A), f') => (case parseHyps'1 (f') of
                     (NONE, f'') => (SOME (Syntax.mkLast (Syntax.mkHyp (A))), f'') 
		   | (SOME Hs, f'') => (SOME (Syntax.mkHyps (Syntax.mkHyp (A), Hs)), f'')))

 and parseHyps'1 (IStream.Cons ((Token.COMMA, r), s)) =
       parseHyps' (IStream.expose s)
   | parseHyps'1 (f) = (NONE, f)

  (*  parseFrame ((x, A), f) = (SOME P, f')
  *)
(*
  and parseFrame ((x, A), IStream.Cons ((Token.SEMICOLON, r), s)) = 
        parseFrame1 (Syntax.mkHyp (x, A), parseProof (IStream.expose s))
    | parseFrame ((x, A), f as IStream.Cons ((Token.LBRACKET, r), s)) =
        parseFrame1 (Syntax.mkHyp (x, A), parseProof (f))
    | parseFrame ((x, A), f as IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError' (r, [Token.SEMICOLON, Token.LBRACKET], t)
*)

  (*  parseFrame (h, f) = (SOME P, f')
  *)
  and parseFrame (h, IStream.Cons ((Token.SEMICOLON, r), s)) = 
        parseFrame1 (h, parseProof (IStream.expose s))
    | parseFrame (h, f as IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.SEMICOLON, t)

  (*  parseFrame1 (h, (P, f)) = (P'Opt, f')
  *)
  and parseFrame1 (h, (P, IStream.Cons ((Token.RBRACKET, r), s))) =
      let val (P'Opt, f) = parseProof'1 (IStream.expose s)
      in case P'Opt of
	  NONE => (SOME (Syntax.mkFinal (Syntax.mkFrame (h, P))), f)
	| SOME P' => (SOME (Syntax.mkStep (Syntax.mkFrame (h, P), P')), f)
      end
        (* proofFrame (h, P, parseProof' (IStream.expose s)) *)
    | parseFrame1 (h, (P, f as IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.RBRACKET, t)

end; (* structure ParseProof *)
