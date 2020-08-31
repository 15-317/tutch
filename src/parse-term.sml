(* $Id: parse-term.sml,v 1.1 2000/10/17 22:23:11 abel Exp $ *)

signature PARSETERM =
sig

  val parseTerm : Lexer.front -> ExtSyn.term * Lexer.front
  val parseProof: Lexer.front -> ExtSyn.annProof * Lexer.front

end; (* signature PARSETERM *)


structure ParseTerm :> PARSETERM =
struct

  (* Operators and atoms for operator precedence parsing *)
  datatype 'a operator =
      Atom of 'a
    | Infix of (Fixity.precedence * Fixity.associativity) * ('a * 'a -> 'a)
    | Prefix of Fixity.precedence * ('a -> 'a)
    | Postfix of Fixity.precedence * ('a -> 'a)

  (* Predeclared operators *)
  val juxOp = Infix ((Fixity.inc Fixity.maxPrec, Fixity.Left), ExtSyn.mkApp) (* juxtaposition *)
  (* val colonOp = Infix ((Fixity.dec (Fixity.dec Fixity.minPrec), Fixity.Left), ExtSyn.hastype) *)
  (* val andOp = Infix ((Fixity.Strength 12, Fixity.Right), ExtSyn.mkAnd) *)

  fun fstOp (f) = Prefix (Fixity.inc Fixity.maxPrec, f)
  fun mkFstOp (r) = fstOp (fn (m) => ExtSyn.mkFst (m, r))
  fun sndOp (f) = Prefix (Fixity.inc Fixity.maxPrec, f)
  fun mkSndOp (r) = sndOp (fn (m) => ExtSyn.mkSnd (m, r))
  fun inlOp (f) = Prefix (Fixity.inc Fixity.maxPrec, f)
  fun mkInlOp (r) = inlOp (fn (m) => ExtSyn.mkInl (m, r))
  fun inrOp (f) = Prefix (Fixity.inc Fixity.maxPrec, f)
  fun mkInrOp (r) = inrOp (fn (m) => ExtSyn.mkInr (m, r))
  fun abortOp (f) = Prefix (Fixity.inc Fixity.maxPrec, f)
  fun mkAbortOp (r) = abortOp (fn (m) => ExtSyn.mkAbort (m, r))
  fun lamOp (f) = Prefix (Fixity.minPrec, f)
  fun mkLamOp (x, r) = lamOp (fn (m) => ExtSyn.mkLam (x, r, m))


  type opr = ExtSyn.term operator
  type stack = opr list

  (* The next section deals generically with fixity parsing          *)
  (* Because of juxtaposition, it is not clear how to turn this      *)
  (* into a separate module without passing a juxtaposition operator *)
  (* into the shift and resolve functions                            *)

  structure P :>
    sig
      val reduce : stack -> stack
      val reduceAll : Region.Region * stack -> ExtSyn.term
      (* val shiftAtom : ExtSyn.term * stack -> stack *)
      val shift : Region.Region * opr * stack -> stack
      val resolve : Region.Region * opr * stack -> stack
    end =
  struct
    (* Stack invariants, refinements of operator list *)
    (*
       <p>       ::= <pStable> | <pRed>
       <pStable> ::= <pAtom> | <pOp?>
       <pAtom>   ::= Atom _ :: <pOp?>
       <pOp?>    ::= nil | <pOp>
       <pOp>     ::= Infix _ :: <pAtom> :: <pOp?>
		   | Prefix _ :: <pOp?>
       <pRed>    ::= Postfix _ :: Atom _ :: <pOp?>
		   | Atom _ :: <pOp>
    *)
    (* val reduce : <pRed> -> <p> *)
    fun reduce (Atom(tm2)::Infix(_,con)::Atom(tm1)::p') =
	   Atom(con(tm1,tm2))::p'
      | reduce (Atom(tm)::Prefix(_,con)::p') = Atom(con(tm))::p'
      | reduce (Postfix(_,con)::Atom(tm)::p') = Atom(con(tm))::p'
      (* no other cases should be possible by stack invariant *)

    (* val reduceRec : <pStable> -> ExtSyn.term *)
    fun reduceRec (Atom(e)::nil) = e
      | reduceRec (p) = reduceRec (reduce p)

    (* val reduceAll : <p> -> ExtSyn.term *)
    fun reduceAll (r, Atom(e)::nil) = e
      | reduceAll (r, Infix _::p') = Parsing.error (r, "Incomplete infix expression")
      | reduceAll (r, Prefix _::p') = Parsing.error (r, "Incomplete prefix expression")
      | reduceAll (r, nil) = Parsing.error (r, "Empty expression")
      | reduceAll (r, p) = reduceRec (reduce p)

    (* val shiftAtom : term * <pStable> -> <p> *)
    (* does not raise Error exception *)
	(* insert juxOp operator and reduce *)
	(* juxtaposition binds most strongly *)
(*    fun shiftAtom (m, p as (Atom _::p')) =
	reduce (Atom(m)::juxOp::p) 
      | shiftAtom (m, p) = Atom(m)::p *)

    (* val shift : Region.Region * opr * <pStable> -> <p> *)
    fun shift (r, opr as Atom _, p as (Atom _::p')) =
	  (* insert juxOp operator and reduce *)
	  (* NO LONGER: juxtaposition binds most strongly *)
	  (*           reduce (opr::juxOp::p) *) 
	  shift (r, opr,  resolve (r, juxOp, p)) 
      (* Atom/Infix: shift *)
      (* Atom/Prefix: shift *)
      (* Atom/Postfix cannot arise *)
      (* Atom/Empty: shift *)
      (* Infix/Atom: shift *)
      | shift (r, Infix _, Infix _::p') =
	  Parsing.error (r, "Consective infix operators")
      | shift (r, Infix _, Prefix _::p') =
	  Parsing.error (r, "Infix operator following prefix operator")
      (* Infix/Postfix cannot arise *)
      | shift (r, Infix _, nil) =
	  Parsing.error (r, "Leading infix operator")
      | shift (r, opr as Prefix _, p as (Atom _::p')) =
	 (* insert juxtaposition operator *)
	 (* will be reduced later *)
	 (* opr::juxOp::p *)
	  Parsing.error (r, "Prefix operator used in infix position")
      (* Prefix/{Infix,Prefix,Empty}: shift *)
      (* Prefix/Postfix cannot arise *)
      (* Postfix/Atom: shift, reduced immediately *)
      | shift (r, Postfix _, Infix _::p') =
	  Parsing.error (r, "Postfix operator following infix operator")
      | shift (r, Postfix _, Prefix _::p') =
	  Parsing.error (r, "Postfix operator following prefix operator")
      (* Postfix/Postfix cannot arise *)
      | shift (r, Postfix _, nil) =
	  Parsing.error (r, "Leading postfix operator")
      | shift (r, opr, p) = opr::p

    (* val resolve : Region.Region * opr * <pStable> -> <p> *)
    (* Decides, based on precedence of opr compared to the top of the
       stack whether to shift the new operator or reduce the stack
    *)
    and resolve (r, opr as Infix((prec, assoc), _),
		   p as (Atom(_)::Infix((prec', assoc'), _)::p')) =
	(case (Fixity.compare(prec,prec'), assoc, assoc')
	   of (GREATER,_,_) => shift(r, opr, p)
	    | (LESS,_,_) => resolve (r, opr, reduce(p))
	    | (EQUAL, Fixity.Left, Fixity.Left) => resolve (r, opr, reduce(p))
	    | (EQUAL, Fixity.Right, Fixity.Right) => shift(r, opr, p)
	    | _ => Parsing.error (r, "Ambiguous: infix following infix of identical precedence"))
      | resolve (r, opr as Infix ((prec, assoc), _),
		   p as (Atom(_)::Prefix(prec', _)::p')) =
	(case Fixity.compare(prec,prec')
	   of GREATER => shift(r, opr, p)
	    | LESS => resolve (r, opr, reduce(p))
	    | EQUAL => Parsing.error (r, "Ambiguous: infix following prefix of identical precedence"))
      (* infix/atom/atom cannot arise *)
      (* infix/atom/postfix cannot arise *)
      (* infix/atom/<empty>: shift *)

      (* always shift prefix *)
      | resolve (r, opr as Prefix _, p) =
	  shift(r, opr, p)

      (* always reduce postfix, possibly after prior reduction *)
      | resolve (r, opr as Postfix(prec, _),
		   p as (Atom _::Prefix(prec', _)::p')) =
	  (case Fixity.compare(prec,prec')
	     of GREATER => reduce (shift (r, opr, p))
	      | LESS => resolve (r, opr, reduce (p))
	      | EQUAL => Parsing.error (r, "Ambiguous: postfix following prefix of identical precedence"))
      (* always reduce postfix *)
      | resolve (r, opr as Postfix(prec, _),
		   p as (Atom _::Infix((prec', _), _)::p')) =
	  (case Fixity.compare(prec,prec')
	     of GREATER => reduce (shift (r, opr, p))
	      | LESS => resolve (r, opr, reduce (p))
	      | EQUAL => Parsing.error (r, "Ambiguous: postfix following infix of identical precedence"))
      | resolve (r, opr as Postfix _, p as (Atom _::nil)) =
	  reduce (shift (r, opr, p))

      (* default is shift *)
      | resolve (r, opr, p) =
	  shift(r, opr, p)

  end  (* structure P *)

  
  datatype Result =
      None
    | Term of ExtSyn.term
    | Ann of ExtSyn.term * ExtSyn.prop

  (*  parseTerm (f) = (M, f') 
  *)
  fun parseTerm (f as IStream.Cons ((t, r), s)) =  
       (case parseTerm' (f, []) of
	    (None, f') => Parsing.error (r, "Expected term, found " ^ Token.toString (t))
	  | (Term (M), f') => (M, f')
	  | (Ann (M, A), f') => (ExtSyn.mkAnn (M, A), f'))

  (*  parseTerm' : Lexer.front * stack -> Result * Lexer.front
      parseTerm' (f, p) = (MAnnOpt, f')
  *)
  and parseTerm' (IStream.Cons ((Token.ID (Token.Lower, name), r), s), p) =
        parseTerm' (IStream.expose s, P.shift (r, Atom (ExtSyn.mkVar (name, r)), p))
    | parseTerm' (IStream.Cons ((t as Token.ID (Token.Upper, name), r), s), p) =
        Parsing.error (r, "Unexpected " ^ Token.toString (t) ^" in term -- Variables must be lowercase")

    | parseTerm' (IStream.Cons ((Token.FST, r), s), p) =
        parseTerm' (IStream.expose s, P.resolve (r, mkFstOp (r), p))
    | parseTerm' (IStream.Cons ((Token.SND, r), s), p) =
        parseTerm' (IStream.expose s, P.resolve (r, mkSndOp (r), p))
    | parseTerm' (IStream.Cons ((Token.INL, r), s), p) =
        parseTerm' (IStream.expose s, P.resolve (r, mkInlOp (r), p))
    | parseTerm' (IStream.Cons ((Token.INR, r), s), p) =
	parseTerm' (IStream.expose s, P.resolve (r, mkInrOp (r), p))
    | parseTerm' (IStream.Cons ((Token.ABORT, r), s), p) =
	parseTerm' (IStream.expose s, P.resolve (r, mkAbortOp (r), p))

    | parseTerm' (IStream.Cons ((Token.CASE, r), s), p) =
        parseCase (r, parseTerm (IStream.expose s), p)
    | parseTerm' (IStream.Cons ((Token.FN, r), s), p) =
        parseLam (r, IStream.expose s, p)

    | parseTerm' (IStream.Cons ((Token.COLON, r), s), p) =
        let 
            val (A, f) = ParseProp.parseProp (IStream.expose s)
	in
	    (Ann (P.reduceAll (r, p), A), f)
	end (* Do not continue to parse term after annotation *)

    | parseTerm' (IStream.Cons ((Token.LPAREN, r), s), p) =
        decideRParen (r, parseTerm' (IStream.expose s, []), p)

    | parseTerm' (f as IStream.Cons ((t, r), s), []) = (None, f)
    | parseTerm' (f as IStream.Cons ((t, r), s), p) = 
        (Term (P.reduceAll (r, p)), f)

  and decideRParen (r0, (Term M, IStream.Cons ((Token.RPAREN, r), s)), p) =
        parseTerm' (IStream.expose s, P.shift (Region.join (r0, r), Atom M, p))
    | decideRParen (r0, (Ann (M, A), IStream.Cons ((Token.RPAREN, r), s)), p) =
        parseTerm' (IStream.expose s, P.shift (Region.join (r0, r), Atom (ExtSyn.mkAnn (M, A)), p))
    | decideRParen (r0, (None, IStream.Cons ((Token.RPAREN, r), s)), p) =
        let val r' = Region.join (r0, r)
        in
            parseTerm' (IStream.expose s, P.shift (r', Atom (ExtSyn.mkStar (r')), p))
        end
    | decideRParen (r0, (Term M, IStream.Cons ((Token.COMMA, r), s)), p) =
        decideRParen1 (r0, M, parseTerm' (IStream.expose s, []), p) 
    | decideRParen (r0, (Ann (M, A), IStream.Cons ((Token.COMMA, r), s)), p) =
        decideRParen1 (r0, ExtSyn.mkAnn (M, A), parseTerm' (IStream.expose s, []), p) 
    | decideRParen (r0, (None, IStream.Cons ((Token.COMMA, r), s)), p) =
        Parsing.error (Region.join(r0, r), "First component of pair missing")
    | decideRParen (r0, (MAnnOpt, IStream.Cons ((_, r), s)), p) =
        Parsing.error (Region.join(r0, r), "Unmatched open parenthesis")

  and decideRParen1 (r0, M, (Term N, IStream.Cons ((Token.RPAREN, r), s)), p) =
        parseTerm' (IStream.expose s, P.shift (Region.join (r0, r), Atom (ExtSyn.mkPair (M, N)), p))
    | decideRParen1 (r0, M, (Ann (N, A), IStream.Cons ((Token.RPAREN, r), s)), p) =
        parseTerm' (IStream.expose s, P.shift (Region.join (r0, r), Atom (ExtSyn.mkPair (M, ExtSyn.mkAnn (N, A))), p))
    | decideRParen1 (r0, M, (None, IStream.Cons ((t, r), s)), p) =
	Parsing.error (Region.join (r0, r), "Expected second component of pair, found " 
          ^ Token.toString (t)) 
    | decideRParen1 (r0, M, (NAnnOpt, IStream.Cons ((t, r), s)), p) =
        Parsing.error (Region.join(r0, r), "Uncompleted pair: closing parenthesis missing, found instead " 
          ^ Token.toString (t))

  and parseLam (r0, IStream.Cons ((Token.ID (Token.Lower, x), r), s), p) =
        parseLam1 (r0, x, IStream.expose s, p)
    | parseLam (r0, IStream.Cons ((t as Token.ID (Token.Upper, x), r), s), p) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseLam (r0, IStream.Cons ((t, r), s), p) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseLam1 (r0, x, IStream.Cons ((Token.DOUBLEARROW, r), s), p) =
      let val r' = Region.join (r0, r) in
        parseTerm' (IStream.expose s, P.resolve (r, mkLamOp (x, r), p))
      end
    | parseLam1 (r0, x, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.DOUBLEARROW, t)


  and parseCase (r0, (M, IStream.Cons ((Token.OF, r), s)), p) =
        parseCase1 (r0, M, IStream.expose s, p)
    | parseCase (r0, (M, IStream.Cons ((t, r), s)), p) =
        Parsing.wrongTokenError (r, Token.OF, t)
  
  and parseCase1 (r0, M, IStream.Cons ((Token.INL, r), s), p) =
        parseCase2 (r0, M, parseBind (IStream.expose s), p)
    | parseCase1 (r0, M, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.INL, t)

  and parseCase2 (r0, M, (B, IStream.Cons ((Token.BAR, r), s)), p) =
        parseCase3 (r0, M, B, IStream.expose s, p)
    | parseCase2 (r0, M, (B, IStream.Cons ((t, r), s)), p) =
        Parsing.wrongTokenError (r, Token.BAR, t)

  and parseCase3 (r0, M, B, IStream.Cons ((Token.INR, r), s), p) =
        parseCase4 (r0, M, B, parseBind (IStream.expose s), p)
    | parseCase3 (r0, M, B, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.INR, t)

  and parseCase4 (r0, M, B, (B', IStream.Cons ((Token.END, r), s)), p) =
        let val r' = Region.join (r0, r)
        in
            parseTerm' (IStream.expose s, P.shift (r', Atom (ExtSyn.mkCase (M, B, B', r')), p))
        end
    | parseCase4 (r0, M, B, (B', IStream.Cons ((t, r), s)), p) =
        Parsing.wrongTokenError (r, Token.END, t)

  and parseBind (IStream.Cons ((Token.ID (Token.Lower, x), r), s)) =
        parseBind1 (r, x, IStream.expose s)
    | parseBind (IStream.Cons ((t as Token.ID (Token.Upper, x), r), s)) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseBind (IStream.Cons ((t, r), s)) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseBind1 (r0, x, IStream.Cons ((Token.DOUBLEARROW, r), s)) =
      let 
	  val (M, f) = parseTerm (IStream.expose s)
      in 
	  (ExtSyn.mkBind (x, r0, M), f) 
      end
    | parseBind1 (r0, x, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.DOUBLEARROW, t)



  (*  parseDecl (f) = ((name, A), f') 
  *) 
  fun parseDecl (IStream.Cons ((Token.ID (Token.Lower, name), r), s)) =
        parseDecl1 (r, name, IStream.expose s)
    | parseDecl (IStream.Cons ((t as Token.ID (Token.Upper, name), r), s)) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseDecl (IStream.Cons ((t, r), s)) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseDecl1 (r0, name, IStream.Cons ((Token.COLON, r), s)) =
      let val (A, f') = ParseProp.parseProp (IStream.expose s)
      in ((name, r0, A), f') end
    | parseDecl1 (r0, name, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.COLON, t)


  (*  parseLine (f) = (EOpt, f') *)
  fun parseLine (f) = 
       (case parseTerm' (f, []) of
	    (None, f') => (NONE, f')
	  | (Term (M), IStream.Cons ((t, r), s)) => 
		Parsing.wrongTokenError (r, Token.COLON, t)
	  | (Ann (M, A), f') => (SOME (ExtSyn.mkAnnLine (M, A)), f'))
 

  (*  parseProof (f) = (P, f')
  *)  
  fun parseProof (f as IStream.Cons ((t, r), s)) = 
      let val (POpt, f') = parseProof' (f)
      in case POpt of
	  NONE => Parsing.error (r, "Expected annotated proof, found " ^ Token.toString (t))
	| SOME P => (P, f')
      end

  (*  parseProof' (f) = (POpt, f')
  *)
  and parseProof' (IStream.Cons ((Token.SEMICOLON, r), s)) =
        parseProof' (IStream.expose s)
    | parseProof' (IStream.Cons ((Token.LBRACKET, r), s)) = 
	parseFrame (parseDecl (IStream.expose s))
(*	parseFrame (parseDecl' (IStream.expose s)) *)
    | parseProof' (f) = 
	let val (EOpt, f') = parseLine (f)
	in case EOpt of
	    NONE => (NONE, f')
	  | SOME E => let val (POpt, f'') = parseProof'1 (f')
		      in case POpt of
			  NONE => (SOME (ExtSyn.mkAnnFinal (E)), f'')
			| SOME P => (SOME (ExtSyn.mkAnnStep (E, P)), f'')
		      end
	end

  and parseProof'1 (IStream.Cons ((Token.SEMICOLON, r), s)) =
        parseProof' (IStream.expose s)
    | parseProof'1 (f as IStream.Cons ((t, r), s)) = (NONE, f)

  (*  parseFrame (A, f) = (SOME P, f')
  *)
  and parseFrame ((x, r, A), IStream.Cons ((Token.SEMICOLON, r'), s)) = 
        parseFrame1 (ExtSyn.mkAnnHyp (x, r, A), parseProof (IStream.expose s))
    | parseFrame ((x, r, A), f as IStream.Cons ((t, r'), s)) =
        Parsing.wrongTokenError (r', Token.SEMICOLON, t)

  (*  parseFrame1 (h, (P, f)) = (P'Opt, f')
  *)
  and parseFrame1 (h, (P, IStream.Cons ((Token.RBRACKET, r), s))) =
      let val (P'Opt, f) = parseProof'1 (IStream.expose s)
      in case P'Opt of
	  NONE => (SOME (ExtSyn.mkAnnFinal (ExtSyn.mkAnnFrame (h, P))), f)
	| SOME P' => (SOME (ExtSyn.mkAnnStep (ExtSyn.mkAnnFrame (h, P), P')), f)
      end
        (* proofFrame (h, P, parseProof' (IStream.expose s)) *)
    | parseFrame1 (h, (P, f as IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.RBRACKET, t)

end; (* structure ParseTerm *)
