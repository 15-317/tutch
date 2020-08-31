(* $Id: parser.sml,v 1.3 2002/10/24 19:25:48 abel Exp $ *)

signature PARSER =
sig

  val parseStream : TextIO.instream -> Decl.Decl Stream.stream

end;  (* signature PARSER *)

structure Parser :> PARSER =
struct

  fun parseStream (instream) =
      let val s = Lexer.lexStream (instream)
      in
	  Stream.delay (fn () => parseStream' (IStream.expose s))
      end

  (* parseStream' : (Token.Token * Path.region) IStream.front 
                  -> Decl.Decl Stream.front *)
  (* parseStream' switches between various specialized parsers *)

  and parseStream' (IStream.Cons ((Token.SEMICOLON, r), s)) = 
        parseStream' (IStream.expose s)

    (* old : *)
    | parseStream' (IStream.Cons ((Token.ANNOTATED, r), s)) = 
        parseAnnProofDecl (IStream.expose s)

    (* new: *)
    | parseStream' (IStream.Cons ((Token.CLASSICAL, r), s)) = 
        parseClassProofDecl (IStream.expose s)
     | parseStream' (IStream.Cons ((Token.VAL, r), s)) = 
        parseExpDecl (IStream.expose s)
    | parseStream' (IStream.Cons ((Token.PROOF, r), s)) = 
        parseProofDecl (false, IStream.expose s)
       (* Default: intutionistic proof [false] *)
    | parseStream' (IStream.Cons ((Token.TERM, r), s)) = 
        parseTermDecl (IStream.expose s)

    | parseStream' (IStream.Cons ((Token.EOF, r), s)) = Stream.Empty 
    | parseStream' (IStream.Cons ((t, r), s)) = 
        Parsing.wrongTokenError' (r, [Token.PROOF, Token.TERM, Token.ANNOTATED, Token.VAL], t)

  (*  parseStream'1 parses final semicolon of each declaration *)
  and parseStream'1 (D, IStream.Cons ((Token.SEMICOLON, r), s)) =
        Stream.Cons (D, Stream.delay (fn () => parseStream' (IStream.expose s)))
    | parseStream'1 (D, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.SEMICOLON, t)
        

  (* annotated proof x : A = ... *)
  and parseAnnProofDecl (IStream.Cons ((Token.PROOF, r), s)) =
        parseAnnProofDecl1 (ParseProp.parseDecl (IStream.expose s))
    | parseAnnProofDecl (IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.PROOF, t)
  
  and parseAnnProofDecl1 ((name, A), IStream.Cons ((Token.EQUAL, r), s)) =
        parseAnnProofDecl2 ((name, A), IStream.expose s)
    | parseAnnProofDecl1 ((name, A), IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.EQUAL, t)

  and parseAnnProofDecl2 ((name, A), IStream.Cons ((Token.BEGIN, r), s)) =
        parseAnnProofDecl3 ((name, A), ParseTerm.parseProof (IStream.expose s)) 
    | parseAnnProofDecl2 ((name, A), IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.BEGIN, t)

  and parseAnnProofDecl3 ((name, A), (P, IStream.Cons ((Token.END, r), s))) =
        parseStream'1 (Decl.AnnProof (name, A, P), IStream.expose s)
    | parseAnnProofDecl3 ((name, A), (P, IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.END, t)

  (* val x : A = ... *)
  and parseExpDecl (f) = parseExpDecl0 (ParseExp.parseAmbigDeclLower (f))

  and parseExpDecl0 ((name, A), IStream.Cons ((Token.EQUAL, r), s)) =
        parseExpDecl1 ((name, A), ParseExp.parseExp (IStream.expose s))
    | parseExpDecl0 ((name, A), IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.EQUAL, t)

  and parseExpDecl1 ((name, A), (M, f)) = 
        parseStream'1 (Decl.Exp (name, A, M), f)

  (* classical proof x : A = ... *)
  and parseClassProofDecl (IStream.Cons ((Token.PROOF, r), s)) =
        parseProofDecl (true, IStream.expose s)
        (* classical proof [true] *)
    | parseClassProofDecl (IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.PROOF, t)

  (* proof x : A = ... *)
  and parseProofDecl (class, f) = 
        parseProofDecl1 (class, ParseExp.parseAmbigDecl (f))
  
  and parseProofDecl1 (class, ((name, A), IStream.Cons ((Token.EQUAL, r), s))) =
        parseProofDecl2 ((class, name, A), IStream.expose s)
    | parseProofDecl1 (class, ((name, A), IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.EQUAL, t)

  and parseProofDecl2 (state, IStream.Cons ((Token.BEGIN, r), s)) =
        parseProofDecl3 (state, ParseProof.parseProof (IStream.expose s)) 
    | parseProofDecl2 (state, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.BEGIN, t)

  and parseProofDecl3 ((class, name, A), (P, IStream.Cons ((Token.END, r), s))) =
        parseStream'1 (Decl.Proof (class, name, A, P), IStream.expose s)
    | parseProofDecl3 ((class, name, A), (P, IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.END, t)

  (* term x : A = ... *)
  and parseTermDecl (f) = parseTermDecl0 (ParseExp.parseAmbigDecl (f))

  and parseTermDecl0 ((name, A), IStream.Cons ((Token.EQUAL, r), s)) =
        parseTermDecl1 ((name, A), ParseExp.parseExp (IStream.expose s))
    | parseTermDecl0 ((name, A), IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.EQUAL, t)

  and parseTermDecl1 ((name, A), (M, f)) = 
        parseStream'1 (Decl.Term (name, A, M), f)

end;  (* structure Parser *)
