(* $Id: parse-prop.sml,v 1.1 2000/10/17 22:23:10 abel Exp $ *)

signature PARSEPROP =
sig

  type lexFront = (Token.Token * Region.Region) IStream.front 

  val parseProp : lexFront -> ExtSyn.prop * lexFront
  val parseDecl : lexFront -> (string * ExtSyn.prop) * lexFront
  val parseDecl': lexFront -> (string option * ExtSyn.prop) * lexFront
  val parseProof: lexFront -> ExtSyn.proof * lexFront

end; (* signature PARSEPROP *)


structure ParseProp :> PARSEPROP =
struct

  type lexFront = (Token.Token * Region.Region) IStream.front 

  (* Operators and atoms for operator precedence parsing *)
  datatype 'a operator =
      Atom of 'a
    | Infix of (Fixity.precedence * Fixity.associativity) * ('a * 'a -> 'a)
    | Prefix of Fixity.precedence * ('a -> 'a)
    | Postfix of Fixity.precedence * ('a -> 'a)

  (* Predeclared operators *)
  (* val juxOp = Infix ((Fixity.inc Fixity.maxPrec, Fixity.Left), ExtSyn.app) *) (* juxtaposition *)
  (* val colonOp = Infix ((Fixity.dec (Fixity.dec Fixity.minPrec), Fixity.Left), ExtSyn.hastype) *)

  val andOp = Infix ((Fixity.Strength 12, Fixity.Right), ExtSyn.mkAnd)
  val orOp = Infix ((Fixity.Strength 11, Fixity.Right), ExtSyn.mkOr)
  val impliesOp = Infix ((Fixity.Strength 10, Fixity.Right), ExtSyn.mkImplies)
  fun notOp (f) = Prefix (Fixity.Strength 13, f)
  val equivOp = Infix ((Fixity.Strength 9, Fixity.None), ExtSyn.mkEquiv)

  fun mkNotOp (r) = notOp (fn (A) => ExtSyn.mkNot (A, r))

  type opr = ExtSyn.prop operator
  type stack = opr list

  (* The next section deals generically with fixity parsing          *)
  (* Because of juxtaposition, it is not clear how to turn this      *)
  (* into a separate module without passing a juxtaposition operator *)
  (* into the shift and resolve functions                            *)

  structure P :>
    sig
      val reduce : stack -> stack
      val reduceAll : Region.Region * stack -> ExtSyn.prop
      (* val shiftAtom : ExtSyn.prop * stack -> stack *)
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
    (*
    fun shiftAtom (tm, p as (Atom _::p')) =
	(* insert juxOp operator and reduce *)
	(* juxtaposition binds most strongly *)
	(* reduce (Atom(tm)::juxOp::p) *)
        Parsing.error (r, "Consecutive atoms")
      | shiftAtom (tm, p) = Atom(tm)::p
    *)

    (* val shift : Region.Region * opr * <pStable> -> <p> *)
    fun shift (r, opr as Atom _, p as (Atom _::p')) =
	  (* insert juxOp operator and reduce *)
	  (* juxtaposition binds most strongly *)
	  (* reduce (opr::juxOp::p) *)
          (* in the propositional case, consecutive atoms are an error *)
	  Parsing.error (r, "Expected infix operator")
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
    fun resolve (r, opr as Infix((prec, assoc), _),
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

  (*  parseProp (f) = (A, f') 
  *)
  fun parseProp (f as IStream.Cons ((t, r), s)) = 
      let val (AOpt, f') = parseProp' (f, [])
      in case AOpt of
	  NONE => Parsing.error (r, "Expected proposition, found " ^ Token.toString (t))
	| SOME A => (A, f')
      end

  (*  parseProp' : lexFront * stack -> ExtSyn.prop option * lexFront
      parseProp' (f, p) = (AOpt, f')
  *)
  and parseProp' (IStream.Cons ((Token.ID (Token.Upper, name), r), s), p) =
        parseProp' (IStream.expose s, P.shift (r, Atom (ExtSyn.mkAtom (name, r)), p))
    | parseProp' (IStream.Cons ((t as Token.ID (Token.Lower, name), r), s), p) =
        Parsing.error (r, "Unexpected " ^ Token.toString (t) ^" in proposition -- Atoms must be capitalized")
    | parseProp' (IStream.Cons ((Token.T, r), s), p) =
        parseProp' (IStream.expose s, P.shift (r, Atom (ExtSyn.mkTrue (r)), p))
    | parseProp' (IStream.Cons ((Token.F, r), s), p) =
        parseProp' (IStream.expose s, P.shift (r, Atom (ExtSyn.mkFalse (r)), p))

    | parseProp' (IStream.Cons ((Token.AMPERSAND, r), s), p) =
        parseProp' (IStream.expose s, P.resolve (r, andOp, p))
    | parseProp' (IStream.Cons ((Token.BAR, r), s), p) =
        parseProp' (IStream.expose s, P.resolve (r, orOp, p))
    | parseProp' (IStream.Cons ((Token.DOUBLEARROW, r), s), p) =
        parseProp' (IStream.expose s, P.resolve (r, impliesOp, p))
    | parseProp' (IStream.Cons ((Token.BIDOUBLEARROW, r), s), p) =
	parseProp' (IStream.expose s, P.resolve (r, equivOp, p))
    | parseProp' (IStream.Cons ((Token.TILDE, r), s), p) =
        parseProp' (IStream.expose s, P.resolve (r, mkNotOp (r), p))

    | parseProp' (IStream.Cons ((Token.LPAREN, r), s), p) =
        decideRParen (r, parseProp' (IStream.expose s, []), p)

    | parseProp' (f as IStream.Cons ((t, r), s), []) = (NONE, f)
    | parseProp' (f as IStream.Cons ((t, r), s), p) = 
        (SOME (P.reduceAll (r, p)), f)

  and decideRParen (r0, (SOME A, IStream.Cons((Token.RPAREN, r), s)), p) =
        parseProp' (IStream.expose s, P.shift(Region.join (r0, r), Atom A, p))
    | decideRParen (r0, (NONE, IStream.Cons((Token.RPAREN, r), s)), p) =
        Parsing.error (Region.join(r0, r), "Empty parentheses")
    | decideRParen (r0, (AOpt, IStream.Cons((_, r), s)), p) =
        Parsing.error (Region.join(r0, r), "Unmatched open parenthesis")

  (*  parseDecl (f) = ((name, A), f') 
  *) 
  fun parseDecl (IStream.Cons ((Token.ID (_, name), r), s)) =
        parseDecl1 (name, IStream.expose s)
    | parseDecl (IStream.Cons ((t, r), s)) =
        Parsing.error (r, "Expected identifier, found " ^ Token.toString t)

  and parseDecl1 (name, IStream.Cons ((Token.COLON, r), s)) =
      let val (A, f') = parseProp (IStream.expose s)
      in ((name, A), f') end
    | parseDecl1 (name, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.COLON, t)

  (*  parseDecl' (f) = ((nameOpt, A), f') 
  *) 
  fun parseDecl' (IStream.Cons (tr as (Token.ID (_, name), r), s)) =
        parseDecl1' (tr, IStream.expose s)
    | parseDecl' (f as IStream.Cons ((t, r), s)) =
      let val (AOpt, f') = parseProp' (f, [])
      in case AOpt of
	  SOME A => ((NONE, A), f') 
	| NONE => Parsing.error (r, "Expected proposition or declaration, found " ^ Token.toString t)
      end 

  and parseDecl1' ((Token.ID (_, name), r'), IStream.Cons ((Token.COLON, r), s)) =
      let val (A, f') = parseProp (IStream.expose s)
      in ((SOME name, A), f') end
    | parseDecl1' ((t, r), f) =
      let val (AOpt, f') = parseProp' (IStream.Cons ((t, r), IStream.delay (fn () => f)), []) (* Push token back *)
      in case AOpt of
	  SOME A => ((NONE, A), f') 
	| NONE => Parsing.error (r, "Expected proposition or declaration, found " ^ Token.toString t)
      end 


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
	parseFrame (parseProp (IStream.expose s))
(*	parseFrame (parseDecl' (IStream.expose s)) *)
    | parseProof' (f) = 
	let val (AOpt, f') = parseProp' (f, [])
	in case AOpt of
	    NONE => (NONE, f')
	  | SOME A => let val (POpt, f'') = parseProof'1 (f')
		      in case POpt of
			  NONE => (SOME (ExtSyn.mkFinal (ExtSyn.mkLine (A))), f'')
			| SOME P => (SOME (ExtSyn.mkStep (ExtSyn.mkLine (A), P)), f'')
		      end
	end

  and parseProof'1 (IStream.Cons ((Token.SEMICOLON, r), s)) =
        parseProof' (IStream.expose s)
    | parseProof'1 (f as IStream.Cons ((t, r), s)) = (NONE, f)

  (*  parseFrame ((x, A), f) = (SOME P, f')
  *)
(*
  and parseFrame ((x, A), IStream.Cons ((Token.SEMICOLON, r), s)) = 
        parseFrame1 (ExtSyn.mkHyp (x, A), parseProof (IStream.expose s))
    | parseFrame ((x, A), f as IStream.Cons ((Token.LBRACKET, r), s)) =
        parseFrame1 (ExtSyn.mkHyp (x, A), parseProof (f))
    | parseFrame ((x, A), f as IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError' (r, [Token.SEMICOLON, Token.LBRACKET], t)
*)

  (*  parseFrame (A, f) = (SOME P, f')
  *)
  and parseFrame (A, IStream.Cons ((Token.SEMICOLON, r), s)) = 
        parseFrame1 (ExtSyn.mkHyp A, parseProof (IStream.expose s))
    | parseFrame (A, f as IStream.Cons ((Token.LBRACKET, r), s)) =
        parseFrame1 (ExtSyn.mkHyp A, parseProof (f))
    | parseFrame (A, f as IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError' (r, [Token.SEMICOLON, Token.LBRACKET], t)

  (*  parseFrame1 (h, (P, f)) = (P'Opt, f')
  *)
  and parseFrame1 (h, (P, IStream.Cons ((Token.RBRACKET, r), s))) =
      let val (P'Opt, f) = parseProof'1 (IStream.expose s)
      in case P'Opt of
	  NONE => (SOME (ExtSyn.mkFinal (ExtSyn.mkFrame (h, P))), f)
	| SOME P' => (SOME (ExtSyn.mkStep (ExtSyn.mkFrame (h, P), P')), f)
      end
        (* proofFrame (h, P, parseProof' (IStream.expose s)) *)
    | parseFrame1 (h, (P, f as IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.RBRACKET, t)

end; (* structure ParseProp *)
