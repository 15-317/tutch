(* $Id: parse-spec.sml,v 1.2 2000/10/24 21:09:54 abel Exp $ *)

signature PARSESPEC =
sig

  val parseStream : TextIO.instream -> Spec.specs

end (* signature PARSESPEC *)


structure ParseSpec :> PARSESPEC =
struct

  fun parse (IStream.Cons ((Token.SEMICOLON, r), s)) = parse (IStream.expose (s))
    | parse (IStream.Cons ((Token.PROOF, r), s)) = parseProofSpec (IStream.expose (s))
    | parse (IStream.Cons ((Token.ANNOTATED, r), s)) = parseAnnProofSpec (IStream.expose s)
    | parse (IStream.Cons ((Token.TERM, r), s)) = parseTermSpec (IStream.expose s)
    | parse (IStream.Cons ((Token.VAL, r), s)) = parseExpSpec (IStream.expose s)
    | parse (IStream.Cons ((Token.EOF, r), s)) = []
    | parse (IStream.Cons ((t, r), s)) = Parsing.wrongTokenError (r, Token.PROOF, t)

  and parseAnnProofSpec (IStream.Cons ((Token.PROOF, r), s)) = 
      let val ((x, p), f') = ParseProp.parseDecl (IStream.expose s)
      in Spec.AnnProof (x, ExtSyn.getProp (p)) :: parse (f')
      end
    | parseAnnProofSpec (IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.PROOF, t)

  and parseTermSpec (f) =
      let val ((x, p), f') = ParseExp.parseAmbigDecl (f)
      in Spec.Term (x, Syntax.getProp (p)) :: parse (f')
      end

  and parseExpSpec (f) =
      let val ((x, t), f') = ParseExp.parseAmbigDeclLower (f)
      in Spec.Exp (x, Syntax.getType (t)) :: parse (f')
      end

  and parseProofSpec (f) = 
      let val ((x, p), f') = ParseExp.parseAmbigDecl (f)
      in Spec.Proof (x, Syntax.getProp (p)) :: parse (f')
      end

  fun parseStream (instream) =
      let val s = Lexer.lexStream (instream)
      in
	  parse (IStream.expose s)
      end

end (* structure ParseSpec *)
