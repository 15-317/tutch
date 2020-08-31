(* $Id: parse-exp.sml,v 1.4 2000/10/24 21:09:54 abel Exp $ *)

signature PARSEEXP =
sig
  
(*
  datatype Result =
      None
    | Exp of Syntax.Exp
    | Ann of Syntax.Exp * Syntax.Exp
*)

  (* for cases the = is non-ambigous *)
  val parseExp    : Lexer.front -> Syntax.Exp * Lexer.front
  val parseExpOpt : Lexer.front -> Syntax.Exp option * Lexer.front
  val parseDecl : Lexer.front -> (string * Syntax.Exp) * Lexer.front
  (* here identifier must be lowercase *)

  (* for cases the = is ambigous *)
  val parseAmbigExp : Lexer.front -> Syntax.Exp * Lexer.front
  val parseAmbigDecl : Lexer.front -> (string * Syntax.Exp) * Lexer.front
  (* here identifier can be capital also *)
  val parseAmbigDeclLower : Lexer.front -> (string * Syntax.Exp) * Lexer.front
  (* here identifier must be lowercase *)


end (* signature PARSEEXP *)


structure ParseExp :> PARSEEXP =
struct

  datatype Quantifier =
      Forall
    | Exists

  (* Operators and atoms for operator precedence parsing *)
  datatype 'a operator =
      Atom of 'a
    | Infix of (Fixity.precedence * Fixity.associativity) * ('a * 'a -> 'a)
    | Prefix of Fixity.precedence * ('a -> 'a)
    | Postfix of Fixity.precedence * ('a -> 'a)

  (* Binding strength for operators *)

  val fixAnn = Fixity.minPrec               (* annotation *)
  (* Quantifiers *)
  val fixQuant = Fixity.Strength  8
  (* Types and proposition connectives *)
  val fixEquiv = Fixity.Strength 10
  val fixArrow = Fixity.Strength 12
  val fixImplies = Fixity.Strength 12
  val fixSum   = Fixity.Strength 14
  val fixOr    = Fixity.Strength 14
  val fixProd  = Fixity.Strength 16
  val fixAnd   = Fixity.Strength 16
  val fixNot   = Fixity.Strength 18
  val fixList  = Fixity.maxPrec
  (* Non-assoc relation symbols *)
  val fixEqual = Fixity.Strength 20  (* hack: make binding of less stronger *)
  val fixLess  = Fixity.Strength 22  (*   to resolve some ambiguities       *)
  (* Terms: weak prefix operators *)
  val fixLam   = Fixity.Strength 30
  val fixLet   = Fixity.Strength 31
  val fixIf    = Fixity.Strength 32
  (* Terms: cons and prefix operators *)
  val fixCons  = Fixity.Strength 40
  val fixJux   = Fixity.maxPrec             (* juxtaposition *)
  val fixInl   = Fixity.maxPrec
  val fixInr   = Fixity.maxPrec
  val fixFst   = Fixity.maxPrec
  val fixSnd   = Fixity.maxPrec
  val fixAbort = Fixity.maxPrec
  val fixSucc  = Fixity.maxPrec
  val fixPFix  = Fixity.maxPrec

  (* Operators: Annotation *)
  val annOp = Infix ((fixAnn, Fixity.None), Syntax.mkAnn)

  (* Propositions *)
  fun allOp (f) = Prefix (fixQuant, f)
  fun mkAllOp (x, n, r) = allOp (fn (m) => Syntax.mkAll (x, n, r, m))
  fun exOp (f) = Prefix (fixQuant, f)
  fun mkExOp  (x, n, r) = exOp (fn (m) => Syntax.mkExists (x, n, r, m))

  fun mkQuantOp (Forall, x, n, r) = mkAllOp (x, n, r)
    | mkQuantOp (Exists, x, n, r) = mkExOp (x, n, r)

  (* hack: make = left fix to resolve ambiguities *)
  val equalOp   = Infix ((fixEqual, Fixity.Left), Syntax.mkEqual)
  val lessOp    = Infix ((fixLess, Fixity.None), Syntax.mkLess)

  val andOp     = Infix ((fixAnd, Fixity.Right), Syntax.mkAnd)
  val orOp      = Infix ((fixOr, Fixity.Right), Syntax.mkOr)
  val impliesOp = Infix ((fixImplies, Fixity.Right), Syntax.mkImplies)
  fun notOp (f) = Prefix (fixNot, f)
  fun mkNotOp (r) = notOp (fn (A) => Syntax.mkNot (A, r))
  val equivOp   = Infix ((fixEquiv, Fixity.None), Syntax.mkEquiv)

  (* Types *)
  val prodOp     = Infix  ((fixProd, Fixity.Right), Syntax.mkProd)
  val sumOp      = Infix  ((fixSum, Fixity.Right), Syntax.mkSum)
  val arrowOp    = Infix  ((fixArrow, Fixity.Right), Syntax.mkArrow)
  fun listOp (f) = Postfix (fixList, f)
  fun mkListOp (r) = listOp (fn (m) => Syntax.mkList (m, r))

  (* Terms *)
  val juxOp = Infix ((fixJux, Fixity.Left), Syntax.mkApp)

  fun fstOp (f) = Prefix (fixFst, f)
  fun mkFstOp (r) = fstOp (fn (m) => Syntax.mkFst (m, r))
  fun sndOp (f) = Prefix (fixSnd, f)
  fun mkSndOp (r) = sndOp (fn (m) => Syntax.mkSnd (m, r))
  fun inlOp (f) = Prefix (fixInl, f)
  fun mkInlOp (r) = inlOp (fn (m) => Syntax.mkInl (m, r))
  fun inrOp (f) = Prefix (fixInr, f)
  fun mkInrOp (r) = inrOp (fn (m) => Syntax.mkInr (m, r))
  fun abortOp (f) = Prefix (fixAbort, f)
  fun mkAbortOp (r) = abortOp (fn (m) => Syntax.mkAbort (m, r))
  fun lamOp (f) = Prefix (fixLam, f)
  fun mkLamOp (x, r) = lamOp (fn (m) => Syntax.mkLam (x, r, m))
  fun letOp (f) = Prefix (fixLet, f)
  fun mkLetOp (x, y, m, r) = lamOp (fn (n) => Syntax.mkLetPair (x, y, m, r, n))

  fun sOp (f) = Prefix (fixSucc, f)
  fun mkSOp (r) = fstOp (fn (m) => Syntax.mkSucc (m, r))
  val consOp = Infix ((fixCons, Fixity.Right), Syntax.mkCons)
  fun ifOp (f) = Prefix (fixIf, f)
  fun mkIfOp (m, n, r) = ifOp (fn (n') => Syntax.mkIf (m, n, n', r)) 

(* template for find-and-replace programming:
  fun seppOp (f) = Prefix (fixPFix, f)
  fun mkSeppOp (r) = seppOp (fn (m) => Syntax.mkSepp (m, r))
*)
  fun eqSOp (f) = Prefix (fixPFix, f)
  fun mkEqSOp (r) = eqSOp (fn (m) => Syntax.mkEqS (m, r))
  fun eqE0SOp (f) = Prefix (fixPFix, f)
  fun mkEqE0SOp (r) = eqE0SOp (fn (m) => Syntax.mkEqE0S (m, r))
  fun eqES0Op (f) = Prefix (fixPFix, f)
  fun mkEqES0Op (r) = eqES0Op (fn (m) => Syntax.mkEqES0 (m, r))
  fun eqESSOp (f) = Prefix (fixPFix, f)
  fun mkEqESSOp (r) = eqESSOp (fn (m) => Syntax.mkEqESS (m, r))
  fun lessSOp (f) = Prefix (fixPFix, f)
  fun mkLessSOp (r) = lessSOp (fn (m) => Syntax.mkLessS (m, r))
  fun lessE0Op (f) = Prefix (fixPFix, f)
  fun mkLessE0Op (r) = lessE0Op (fn (m) => Syntax.mkLessE0 (m, r))
  fun lessESOp (f) = Prefix (fixPFix, f)
  fun mkLessESOp (r) = lessESOp (fn (m) => Syntax.mkLessES (m, r))
  fun eqCOp (f) = Prefix (fixPFix, f)
  fun mkEqCOp (r) = eqCOp (fn (m) => Syntax.mkEqC (m, r))
  fun eqENCOp (f) = Prefix (fixPFix, f)
  fun mkEqENCOp (r) = eqENCOp (fn (m) => Syntax.mkEqENC (m, r))
  fun eqECNOp (f) = Prefix (fixPFix, f)
  fun mkEqECNOp (r) = eqECNOp (fn (m) => Syntax.mkEqECN (m, r))
  fun eqECCOp (f) = Prefix (fixPFix, f)
  fun mkEqECCOp (r) = eqECCOp (fn (m) => Syntax.mkEqECC (m, r))

  type opr = Syntax.Exp operator
  type stack = opr list

  (* The next section deals generically with fixity parsing          *)
  (* Because of juxtaposition, it is not clear how to turn this      *)
  (* into a separate module without passing a juxtaposition operator *)
  (* into the shift and resolve functions                            *)

  structure P :>
    sig
      val reduce : stack -> stack
      val reduceAll : Region.Region * stack -> Syntax.Exp
      (* val shiftAtom : Syntax.Exp * stack -> stack *)
      val shift : Region.Region * opr * stack -> stack
      val resolve : Region.Region * opr * stack -> stack
      val top2 : stack -> (opr * opr) option
(*      val top : stack -> opr option
      val pop  : stack -> stack *)
    end =
  struct
    (* fun top (nil) = NONE
      | top (opr :: p) = SOME opr *)
    fun top2 (optop :: op2 :: p) = SOME (optop, op2)
      | top2 _ = NONE
    (* fun pop (opr :: p) = p *)

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

    (* val reduceRec : <pStable> -> Syntax.Exp *)
    fun reduceRec (Atom(e)::nil) = e
      | reduceRec (p) = reduceRec (reduce p)

    (* val reduceAll : <p> -> Syntax.Exp *)
    fun reduceAll (r, Atom(e)::nil) = e
      | reduceAll (r, Infix _::p') = Parsing.error (r, "Incomplete infix expression")
      | reduceAll (r, Prefix _::p') = Parsing.error (r, "Incomplete prefix expression")
      | reduceAll (r, nil) = Parsing.error (r, "Empty expression")
      | reduceAll (r, p) = reduceRec (reduce p)

    (* val shiftAtom : exp * <pStable> -> <p> *)
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
    | Exp of Syntax.Exp
    | Ann of Syntax.Exp * Syntax.Exp

(******************************* parseObjExp ******************************)
(* parse a simple object *)


  (*  parseObjExp (f) = (M, f') 
  *)
  fun parseObjExp (f as IStream.Cons ((t, r), s)) =  
       (case parseObjExp' (f, []) of
	    (None, f') => Parsing.error (r, "Expected term, found " ^ Token.toString (t))
	  | (Exp (M), f') => (M, f')
	  | (Ann (M, A), f') => (Syntax.mkAnn (M, A), f'))

(*
  and parseObjExpOpt (f) =
       (case parseObjExp' (f, []) of
	    (None, f') => (NONE, f')
	  | (Exp (M), f') => (SOME M, f')
	  | (Ann (M, A), f') => (SOME (Syntax.mkAnn (M, A)), f'))
*)
  (*  parseObjExp' : Lexer.front * stack -> Result * Lexer.front
      parseObjExp' (f, p) = (MAnnOpt, f')
  *)

  (* Identifiers: *)   
  and parseObjExp' (IStream.Cons ((Token.ID (Token.Lower, name), r), s), p) =
        parseObjExp' (IStream.expose s, P.shift (r, Atom (Syntax.mkVar (name, r)), p))
    | parseObjExp' (IStream.Cons ((t as Token.ID (Token.Upper, name), r), s), p) =
                Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")


  (* Terms: *)
    | parseObjExp' (IStream.Cons ((Token.ZERO, r), s), p) = 
        parseObjExp' (IStream.expose s, P.shift (r, Atom (Syntax.mkZero (r)), p))
    | parseObjExp' (IStream.Cons ((Token.TRUE, r), s), p) = 
        parseObjExp' (IStream.expose s, P.shift (r, Atom (Syntax.mkTrue (r)), p))
    | parseObjExp' (IStream.Cons ((Token.FALSE, r), s), p) = 
        parseObjExp' (IStream.expose s, P.shift (r, Atom (Syntax.mkFalse (r)), p))
    | parseObjExp' (IStream.Cons ((Token.NIL, r), s), p) = 
        parseObjExp' (IStream.expose s, P.shift (r, Atom (Syntax.mkNil (r)), p))
    | parseObjExp' (IStream.Cons ((Token.EQ0, r), s), p) = 
        parseObjExp' (IStream.expose s, P.shift (r, Atom (Syntax.mkEq0 (r)), p))
    | parseObjExp' (IStream.Cons ((Token.LESS0, r), s), p) = 
        parseObjExp' (IStream.expose s, P.shift (r, Atom (Syntax.mkLess0 (r)), p))
    | parseObjExp' (IStream.Cons ((Token.EQN, r), s), p) = 
        parseObjExp' (IStream.expose s, P.shift (r, Atom (Syntax.mkEqN (r)), p))

    | parseObjExp' (IStream.Cons ((Token.FST, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkFstOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.SND, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkSndOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.INL, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkInlOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.INR, r), s), p) =
	parseObjExp' (IStream.expose s, P.resolve (r, mkInrOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.ABORT, r), s), p) =
	parseObjExp' (IStream.expose s, P.resolve (r, mkAbortOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.S, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkSOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.DOUBLECOLON, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, consOp, p))
    | parseObjExp' (IStream.Cons ((Token.EQS, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkEqSOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.EQE0S, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkEqE0SOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.EQES0, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkEqES0Op (r), p))
    | parseObjExp' (IStream.Cons ((Token.EQESS, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkEqESSOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.LESSS, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkLessSOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.LESSE0, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkLessE0Op (r), p))
    | parseObjExp' (IStream.Cons ((Token.LESSES, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkLessESOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.EQC, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkEqCOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.EQENC, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkEqENCOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.EQECN, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkEqECNOp (r), p))
    | parseObjExp' (IStream.Cons ((Token.EQECC, r), s), p) =
        parseObjExp' (IStream.expose s, P.resolve (r, mkEqECCOp (r), p))

    | parseObjExp' (IStream.Cons ((Token.FN, r), s), p) =
        parseObjLam (r, IStream.expose s, p)
    | parseObjExp' (IStream.Cons ((Token.LET, r), s), p) =
        parseObjLet (r, IStream.expose s, p)
    | parseObjExp' (IStream.Cons ((Token.IF, r), s), p) =
        parseObjIf (r, parseObjExp (IStream.expose s), p)

    | parseObjExp' (IStream.Cons ((Token.COLON, r), s), p) =
        let 
            val (A, f) = parseExp (IStream.expose s)
	in
	    (Ann (P.reduceAll (r, p), A), f)
	end (* Do not continue to parseObj exp after annotation *)

      (* calls of sub parsers *)
    | parseObjExp' (IStream.Cons ((Token.CASE, r), s), p) =
        let
	    val (f, r', M) = parseCase (r, IStream.expose s)
	in
	    parseObjExp' (f, P.shift (r', Atom M, p))
        end
    | parseObjExp' (IStream.Cons ((Token.REC, r), s), p) =
	let
	    val (f, r', M) = parseRec (r, IStream.expose s)
	in
	    parseObjExp' (f, P.shift (r', Atom M, p))
	end
    | parseObjExp' (IStream.Cons ((Token.LPAREN, r), s), p) =
        let
            val (f, r', M) = parseParen (r, IStream.expose s)
        in 
            parseObjExp' (f, P.shift (r', Atom M, p))
        end

    | parseObjExp' (f as IStream.Cons ((t, r), s), []) = (None, f)
    | parseObjExp' (f as IStream.Cons ((t, r), s), p) = 
        (Exp (P.reduceAll (r, p)), f)


  and parseObjLam (r0, IStream.Cons ((Token.ID (Token.Lower, x), r), s), p) =
        parseObjLam1 (r0, x, IStream.expose s, p)
    | parseObjLam (r0, IStream.Cons ((t as Token.ID (Token.Upper, x), r), s), p) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseObjLam (r0, IStream.Cons ((t, r), s), p) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseObjLam1 (r0, x, IStream.Cons ((Token.DOUBLEARROW, r), s), p) =
      let val r' = Region.join (r0, r) in
        parseObjExp' (IStream.expose s, P.resolve (r', mkLamOp (x, r'), p))
      end
    | parseObjLam1 (r0, x, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.DOUBLEARROW, t)

  and parseObjLet (r0, IStream.Cons ((Token.LPAREN, r), s), p) =
        parseObjLet1 (r0, IStream.expose s, p)
    | parseObjLet (r0, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.LPAREN, t)

  and parseObjLet1 (r0, IStream.Cons ((Token.ID (Token.Lower, x), r), s), p) =
        parseObjLet2 (r0, x, IStream.expose s, p)
    | parseObjLet1 (r0, IStream.Cons ((t as Token.ID (Token.Upper, x), r), s), p) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseObjLet1 (r0, IStream.Cons ((t, r), s), p) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseObjLet2 (r0, x, IStream.Cons ((Token.COMMA, r), s), p) =
        parseObjLet3 (r0, x, IStream.expose s, p)
    | parseObjLet2 (r0, x, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.COMMA, t)

  and parseObjLet3 (r0, x, IStream.Cons ((Token.ID (Token.Lower, y), r), s), p) =
        parseObjLet4 (r0, x, y, IStream.expose s, p)
    | parseObjLet3 (r0, x, IStream.Cons ((t as Token.ID (Token.Upper, y), r), s), p) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseObjLet3 (r0, x, IStream.Cons ((t, r), s), p) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)
  
  and parseObjLet4 (r0, x, y, IStream.Cons ((Token.RPAREN, r), s), p) =
        parseObjLet5 (r0, x, y, IStream.expose s, p)
    | parseObjLet4 (r0, x, y, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.RPAREN, t)

  and parseObjLet5 (r0, x, y, IStream.Cons ((Token.EQUAL, r), s), p) =
        parseObjLet6 (r0, x, y, parseExp (IStream.expose s), p)
    | parseObjLet5 (r0, x, y, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.EQUAL, t)

  and parseObjLet6 (r0, x, y, (M, IStream.Cons ((Token.IN, r), s)), p) =
      let val r' = Region.join (r0, r) in
        parseObjExp' (IStream.expose s, P.resolve (r', mkLetOp (x, y, M, r'), p))
      end
    | parseObjLet6 (r0, x, y, (M, IStream.Cons ((t, r), s)), p) =
        Parsing.wrongTokenError (r, Token.IN, t)

  and parseObjBind (IStream.Cons ((Token.ID (Token.Lower, x), r), s)) =
        parseObjBind1 (r, x, IStream.expose s)
    | parseObjBind (IStream.Cons ((t as Token.ID (Token.Upper, x), r), s)) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseObjBind (IStream.Cons ((t, r), s)) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseObjBind1 (r0, x, IStream.Cons ((Token.DOUBLEARROW, r), s)) =
      let 
	  val (M, f) = parseObjExp (IStream.expose s)
      in 
	  (Syntax.mkBind (x, r0, M), f) 
      end
    | parseObjBind1 (r0, x, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.DOUBLEARROW, t)

  and parseObjIf (r0, (M, IStream.Cons ((Token.THEN, r), s)), p) =
        parseObjIf1 (r0, M, parseObjExp (IStream.expose s), p)
    | parseObjIf (r0, (M, IStream.Cons ((t, r), s)), p) =
        Parsing.wrongTokenError (r, Token.THEN, t)

  and parseObjIf1 (r0, M, (N, IStream.Cons ((Token.ELSE, r), s)), p) =
        let val r' = Region.join (r0, r)
        in
	  parseObjExp' (IStream.expose s, 
            P.resolve (r', mkIfOp (M, N, r0), p))
        end
    | parseObjIf1 (r0, M, (N, IStream.Cons ((t, r), s)), p) =
        Parsing.wrongTokenError (r, Token.ELSE, t)

(******************************* parseExp ******************************)

  (*  parseExp (f) = (M, f') 
  *)
  and parseExp (f) = parseAmbigExp' (false, f)
  and parseExpOpt (f) = parseAmbigExpOpt' (false, f)

  (*  parseAmbigExp (f) = (M, f') 
  *)
  and parseAmbigExp (f) = parseAmbigExp' (true, f)
  and parseAmbigExpOpt (f) = parseAmbigExpOpt' (true, f)

  (*  parseAmbigExp' (b, f) = (M, f') 
  *)
  and parseAmbigExp' (eqA, f as IStream.Cons ((t, r), s)) =  
       (case parseExp' (eqA, f, []) of
	    (None, f') => Parsing.error (r, "Expected expression, found " ^ Token.toString (t))
	  | (Exp (M), f') => (M, f')
	  | (Ann (M, A), f') => (Syntax.mkAnn (M, A), f'))

  and parseAmbigExpOpt' (eqA, f) =
       (case parseExp' (eqA, f, []) of
	    (None, f') => (NONE, f')
	  | (Exp (M), f') => (SOME M, f')
	  | (Ann (M, A), f') => (SOME (Syntax.mkAnn (M, A)), f'))

  (*  parseExp' : bool * Lexer.front * stack -> Result * Lexer.front
      parseExp' (b, f, p) = (MAnnOpt, f')
  *)

  (* Identifiers: *)   
  and parseExp' (eqA, IStream.Cons ((Token.ID (Token.Lower, name), r), s), p) =
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkVar (name, r)), p))
    | parseExp' (eqA, IStream.Cons ((Token.ID (Token.Upper, name), r), s), p) =
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkAtom (name, r)), p))

  (* Propositions: *)
    | parseExp' (eqA, IStream.Cons ((Token.T, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkTruth (r)), p))
    | parseExp' (eqA, IStream.Cons ((Token.F, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkAbsurd (r)), p))

    | parseExp' (eqA, IStream.Cons ((Token.AMPERSAND, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, andOp, p))
    | parseExp' (eqA, IStream.Cons ((Token.BAR, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, orOp, p))
    | parseExp' (eqA, IStream.Cons ((Token.DOUBLEARROW, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, impliesOp, p))
    | parseExp' (eqA, IStream.Cons ((Token.BIDOUBLEARROW, r), s), p) =
	parseExp' (eqA, IStream.expose s, P.resolve (r, equivOp, p))
    | parseExp' (eqA, IStream.Cons ((Token.TILDE, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkNotOp (r), p))

    | parseExp' (eqA, IStream.Cons ((Token.EXCLAMATIONMARK, r), s), p) =
        parseQuant (eqA, r, Forall, IStream.expose s, p)
    | parseExp' (eqA, IStream.Cons ((Token.QUESTIONMARK, r), s), p) =
        parseQuant (eqA, r, Exists, IStream.expose s, p)

    | parseExp' (eqA, f as IStream.Cons ((Token.EQUAL, r), s), p) =
        if eqA then (* equal symbol ambigous? *)
        let
	    val p' = P.resolve (r, equalOp, p)
	in
	    case P.top2 (p') of
		SOME (equalOp, Atom M) => (case Syntax.getCategory (M) of
	           Category.Term => (* clearly a term? *)
                     parseExp' (eqA, IStream.expose s, p') (* parse on! *)
		 | _ => (* prop, type or ambigous *) 
                     (Exp (P.reduceAll (r, p)), f)) (* pop =, stop! *)
	      | _ => parseExp' (eqA, IStream.expose s, p') (* parse on! *)
        end else parseExp' (eqA, IStream.expose s, P.resolve (r, equalOp, p))
    | parseExp' (eqA, IStream.Cons ((Token.LESS, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, lessOp, p))

  (* Types: *)
    | parseExp' (eqA, IStream.Cons ((Token.ONE, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkUnit (r)), p))
(*    | parseExp' (eqA, IStream.Cons ((Token.ZERO, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkEmpty (r)), p)) *)
    | parseExp' (eqA, IStream.Cons ((Token.NAT, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkNat (r)), p))
    | parseExp' (eqA, IStream.Cons ((Token.BOOL, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkBool (r)), p))

    | parseExp' (eqA, IStream.Cons ((Token.TIMES, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, prodOp, p))
    | parseExp' (eqA, IStream.Cons ((Token.PLUS, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, sumOp, p))
    | parseExp' (eqA, IStream.Cons ((Token.ARROW, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, arrowOp, p))
    | parseExp' (eqA, IStream.Cons ((Token.LIST, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkListOp (r), p))

  (* Terms: *)
    | parseExp' (eqA, IStream.Cons ((Token.ZERO, r), s), p) = 
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkZero (r)), p))
    | parseExp' (eqA, IStream.Cons ((Token.TRUE, r), s), p) = 
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkTrue (r)), p))
    | parseExp' (eqA, IStream.Cons ((Token.FALSE, r), s), p) = 
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkFalse (r)), p))
    | parseExp' (eqA, IStream.Cons ((Token.NIL, r), s), p) = 
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkNil (r)), p))
    | parseExp' (eqA, IStream.Cons ((Token.EQ0, r), s), p) = 
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkEq0 (r)), p))
    | parseExp' (eqA, IStream.Cons ((Token.LESS0, r), s), p) = 
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkLess0 (r)), p))
    | parseExp' (eqA, IStream.Cons ((Token.EQN, r), s), p) = 
        parseExp' (eqA, IStream.expose s, P.shift (r, Atom (Syntax.mkEqN (r)), p))

    | parseExp' (eqA, IStream.Cons ((Token.FST, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkFstOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.SND, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkSndOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.INL, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkInlOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.INR, r), s), p) =
	parseExp' (eqA, IStream.expose s, P.resolve (r, mkInrOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.ABORT, r), s), p) =
	parseExp' (eqA, IStream.expose s, P.resolve (r, mkAbortOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.S, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkSOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.DOUBLECOLON, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, consOp, p))
    | parseExp' (eqA, IStream.Cons ((Token.EQS, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkEqSOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.EQE0S, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkEqE0SOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.EQES0, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkEqES0Op (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.EQESS, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkEqESSOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.LESSS, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkLessSOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.LESSE0, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkLessE0Op (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.LESSES, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkLessESOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.EQC, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkEqCOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.EQENC, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkEqENCOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.EQECN, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkEqECNOp (r), p))
    | parseExp' (eqA, IStream.Cons ((Token.EQECC, r), s), p) =
        parseExp' (eqA, IStream.expose s, P.resolve (r, mkEqECCOp (r), p))

    | parseExp' (eqA, IStream.Cons ((Token.FN, r), s), p) =
        parseLam (eqA, r, IStream.expose s, p)
    | parseExp' (eqA, IStream.Cons ((Token.IF, r), s), p) =
        parseIf (eqA, r, parseExp (IStream.expose s), p)
    | parseExp' (eqA, IStream.Cons ((Token.LET, r), s), p) =
        parseLet (eqA, r, IStream.expose s, p)

    | parseExp' (eqA, IStream.Cons ((Token.COLON, r), s), p) =
        let 
            val (A, f) = parseExp (IStream.expose s)
	in
	    (Ann (P.reduceAll (r, p), A), f)
	end (* Do not continue to parse exp after annotation *)

    (* calls to sub-parsers *)
    | parseExp' (eqA, IStream.Cons ((Token.CASE, r), s), p) =
        let
	    val (f, r', M) = parseCase (r, IStream.expose s)
	in
	    parseExp' (eqA, f, P.shift (r', Atom M, p))
        end
    | parseExp' (eqA, IStream.Cons ((Token.REC, r), s), p) =
	let
	    val (f, r', M) = parseRec (r, IStream.expose s)
	in
	    parseExp' (eqA, f, P.shift (r', Atom M, p))
	end
    | parseExp' (eqA, IStream.Cons ((Token.LPAREN, r), s), p) =
        let
            val (f, r', M) = parseParen (r, IStream.expose s)
        in 
            parseExp' (eqA, f, P.shift (r', Atom M, p))
        end

    | parseExp' (eqA, f as IStream.Cons ((t, r), s), []) = (None, f)
    | parseExp' (eqA, f as IStream.Cons ((t, r), s), p) = 
        (Exp (P.reduceAll (r, p)), f)

  and parseQuant (eqA, r0, Q, f, p) = parseQuant1 (eqA, r0, Q, parseDecl (f), p)

  and parseQuant1 (eqA, r0, Q, ((x, A), IStream.Cons ((Token.DOT, r), s)), p) = 
      let val r' = Region.join (r0, r) in 
        parseExp' (eqA, IStream.expose s, P.resolve (r', mkQuantOp (Q, x, A, r'), p))
      end
    | parseQuant1 (eqA, r0, Q, ((x, A), IStream.Cons ((t, r), s)), p) =
        Parsing.wrongTokenError (r, Token.DOT, t)

  and parseLam (eqA, r0, IStream.Cons ((Token.ID (Token.Lower, x), r), s), p) =
        parseLam1 (eqA, r0, x, IStream.expose s, p)
    | parseLam (eqA, r0, IStream.Cons ((t as Token.ID (Token.Upper, x), r), s), p) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseLam (eqA, r0, IStream.Cons ((t, r), s), p) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseLam1 (eqA, r0, x, IStream.Cons ((Token.DOUBLEARROW, r), s), p) =
      let val r' = Region.join (r0, r) in
        parseExp' (eqA, IStream.expose s, P.resolve (r', mkLamOp (x, r'), p))
      end
    | parseLam1 (eqA, r0, x, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.DOUBLEARROW, t)

  and parseLet (eqA, r0, IStream.Cons ((Token.LPAREN, r), s), p) =
        parseLet1 (eqA, r0, IStream.expose s, p)
    | parseLet (eqA, r0, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.LPAREN, t)

  and parseLet1 (eqA, r0, IStream.Cons ((Token.ID (Token.Lower, x), r), s), p) =
        parseLet2 (eqA, r0, x, IStream.expose s, p)
    | parseLet1 (eqA, r0, IStream.Cons ((t as Token.ID (Token.Upper, x), r), s), p) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseLet1 (eqA, r0, IStream.Cons ((t, r), s), p) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseLet2 (eqA, r0, x, IStream.Cons ((Token.COMMA, r), s), p) =
        parseLet3 (eqA, r0, x, IStream.expose s, p)
    | parseLet2 (eqA, r0, x, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.COMMA, t)

  and parseLet3 (eqA, r0, x, IStream.Cons ((Token.ID (Token.Lower, y), r), s), p) =
        parseLet4 (eqA, r0, x, y, IStream.expose s, p)
    | parseLet3 (eqA, r0, x, IStream.Cons ((t as Token.ID (Token.Upper, y), r), s), p) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseLet3 (eqA, r0, x, IStream.Cons ((t, r), s), p) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)
  
  and parseLet4 (eqA, r0, x, y, IStream.Cons ((Token.RPAREN, r), s), p) =
        parseLet5 (eqA, r0, x, y, IStream.expose s, p)
    | parseLet4 (eqA, r0, x, y, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.RPAREN, t)

  and parseLet5 (eqA, r0, x, y, IStream.Cons ((Token.EQUAL, r), s), p) =
        parseLet6 (eqA, r0, x, y, parseExp (IStream.expose s), p)
    | parseLet5 (eqA, r0, x, y, IStream.Cons ((t, r), s), p) =
        Parsing.wrongTokenError (r, Token.EQUAL, t)

  and parseLet6 (eqA, r0, x, y, (M, IStream.Cons ((Token.IN, r), s)), p) =
      let val r' = Region.join (r0, r) in
        parseExp' (eqA, IStream.expose s, P.resolve (r', mkLetOp (x, y, M, r'), p))
      end
    | parseLet6 (eqA, r0, x, y, (M, IStream.Cons ((t, r), s)), p) =
        Parsing.wrongTokenError (r, Token.IN, t)



  (*  parseBind (eqA, f) = (B, f) 
  *)
  and parseBind (eqA, IStream.Cons ((Token.ID (Token.Lower, x), r), s)) =
        parseBind1 (eqA, r, x, IStream.expose s)
    | parseBind (eqA, IStream.Cons ((t as Token.ID (Token.Upper, x), r), s)) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseBind (eqA, IStream.Cons ((t, r), s)) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseBind1 (eqA, r0, x, IStream.Cons ((Token.DOUBLEARROW, r), s)) =
      let 
	  val (M, f) = parseAmbigExp' (eqA, IStream.expose s)
      in 
	  (Syntax.mkBind (x, r0, M), f) 
      end
    | parseBind1 (eqA, r0, x, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.DOUBLEARROW, t)

  and parseIf (eqA, r0, (M, IStream.Cons ((Token.THEN, r), s)), p) =
        parseIf1 (eqA, r0, M, parseExp (IStream.expose s), p)
    | parseIf (eqA, r0, (M, IStream.Cons ((t, r), s)), p) =
        Parsing.wrongTokenError (r, Token.THEN, t)

  and parseIf1 (eqA, r0, M, (N, IStream.Cons ((Token.ELSE, r), s)), p) =
        let val r' = Region.join (r0, r)
        in
	  parseExp' (eqA, IStream.expose s, 
            P.resolve (r', mkIfOp (M, N, r0), p))
        end
    | parseIf1 (eqA, r0, M, (N, IStream.Cons ((t, r), s)), p) =
        Parsing.wrongTokenError (r, Token.ELSE, t)

  (* subroutines -- all non-ambigous *)

  (*  parseParen (r0, f) = (f', r, M) 
  *)
  and parseParen (r0, f) = parseParen1 (r0, parseExp' (false, f, []))

  and parseParen1 (r0, (Exp M, IStream.Cons ((Token.RPAREN, r), s))) =
        (IStream.expose s, Region.join (r0, r), M)
    | parseParen1 (r0, (Ann (M, A), IStream.Cons ((Token.RPAREN, r), s))) =
        (IStream.expose s, Region.join (r0, r), Syntax.mkAnn (M, A))
    | parseParen1 (r0, (None, IStream.Cons ((Token.RPAREN, r), s))) =
        let val r' = Region.join (r0, r)
        in
            (IStream.expose s, r', Syntax.mkStar (r'))
        end
    | parseParen1 (r0, (Exp M, IStream.Cons ((Token.COMMA, r), s))) =
        parseParen2 (r0, M, parseExp' (false, IStream.expose s, [])) 
    | parseParen1 (r0, (Ann (M, A), IStream.Cons ((Token.COMMA, r), s))) =
        parseParen2 (r0, Syntax.mkAnn (M, A), parseExp' (false, IStream.expose s, [])) 
    | parseParen1 (r0, (None, IStream.Cons ((Token.COMMA, r), s))) =
        Parsing.error (Region.join(r0, r), "First component of pair missing")
    | parseParen1 (r0, (MAnnOpt, IStream.Cons ((_, r), s))) =
        Parsing.error (Region.join(r0, r), "Unmatched open parenthesis")

  and parseParen2 (r0, M, (Exp N, IStream.Cons ((Token.RPAREN, r), s))) =
        (IStream.expose s, Region.join (r0, r), Syntax.mkPair (M, N))
    | parseParen2 (r0, M, (Ann (N, A), IStream.Cons ((Token.RPAREN, r), s))) =
        (IStream.expose s, Region.join (r0, r), Syntax.mkPair (M, Syntax.mkAnn (N, A)))
    | parseParen2 (r0, M, (None, IStream.Cons ((t, r), s))) =
	Parsing.error (Region.join (r0, r), "Expected second component of pair, found " 
          ^ Token.toString (t)) 
    | parseParen2 (r0, M, (NAnnOpt, IStream.Cons ((t, r), s))) =
        Parsing.error (Region.join(r0, r), "Uncompleted pair: closing parenthesis missing, found instead " 
          ^ Token.toString (t))

  (*  parseCase (r0, f) = (f', r', M') 
  *)
  and parseCase (r0, f) = parseCase0 (r0, parseExp (f))
  
  and parseCase0 (r0, (M, IStream.Cons ((Token.OF, r), s))) =
        parseCase1 (r0, M, IStream.expose s)
    | parseCase0 (r0, (M, IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.OF, t)
  
  and parseCase1 (r0, M, IStream.Cons ((Token.INL, r), s)) =
        parseCase2 (r0, M, parseObjBind (IStream.expose s))
    | parseCase1 (r0, M, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.INL, t)

  and parseCase2 (r0, M, (B, IStream.Cons ((Token.BAR, r), s))) =
        parseCase3 (r0, M, B, IStream.expose s)
    | parseCase2 (r0, M, (B, IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.BAR, t)

  and parseCase3 (r0, M, B, IStream.Cons ((Token.INR, r), s)) =
        parseCase4 (r0, M, B, parseObjBind (IStream.expose s))
    | parseCase3 (r0, M, B, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.INR, t)

  and parseCase4 (r0, M, B, (B', IStream.Cons ((Token.END, r), s))) =
        let val r' = Region.join (r0, r)
        in
            (IStream.expose s, r', Syntax.mkCase (M, B, B', r'))
        end
    | parseCase4 (r0, M, B, (B', IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.END, t)

  (*  parseRec (r0, f) = (f', r', M')
  *)
  and parseRec (r0, f) = parseRec0 (r0, parseExp (f))

  and parseRec0 (r0, (M, IStream.Cons ((Token.OF, r), s))) =
        parseRec1 (r0, M, IStream.expose s)
    | parseRec0 (r0, (M, IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.OF, t)
										 and parseRec1 (r0, M, IStream.Cons ((Token.ID (Token.Lower, g), r), s)) =
        parseRec2 (r0, M, g, parseObjExp (IStream.expose s))
    | parseRec1 (r0, M, IStream.Cons ((t as (Token.ID (Token.Upper, g)), r), s)) =
        Parsing.error (r, "Invalid variable identifier " ^ Token.toString t ^ " -- Variables start with a lowercase letter")
    | parseRec1 (r0, M, IStream.Cons ((t, r), s)) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseRec2 (r0, M, g, (N, f)) =
        if Syntax.isZero (N) then parseRecNat2 (r0, M, g, f)
        else if Syntax.isNil (N) then parseRecList2 (r0, M, g, f)
        else Parsing.error (Syntax.getRegion (M),
          "Expected `0' or `nil' in recursion clause")

  and parseRecNat2 (r0, M, g, IStream.Cons ((Token.DOUBLEARROW, r), s)) =
        parseRecNat3 (r0, M, g, parseObjExp (IStream.expose s))
    | parseRecNat2 (r0, M, g, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.DOUBLEARROW, t)

  and parseRecNat3 (r0, M, g, (N, IStream.Cons ((Token.BAR, r), s))) =
        parseRecNat4 (r0, M, g, N, IStream.expose s)
    | parseRecNat3 (r0, M, g, (N, IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.BAR, t)

  and parseRecNat4 (r0, M, g, N, IStream.Cons ((t as (Token.ID (Token.Lower, x)), r), s)) =
        if x=g then parseRecNat5 (r0, M, g, N, parseObjExp (IStream.expose s))
        else Parsing.error (r, "Expected variable identifier `"^g^"', found " ^ Token.toString t)
    | parseRecNat4  (r0, M, g, N, IStream.Cons ((t, r), s)) =
        Parsing.error (r, "Expected variable identifier `"^g^"', found " ^ Token.toString t)

  and parseRecNat5 (r0, M, g, N, (O, f)) = (case Syntax.isSucc (O) of
            NONE => Parsing.error (Syntax.getRegion (O),
          "Expected `(s <var>)' in recursion clause")
	  | SOME (x) => parseRecNat6 (r0, M, g, N, x, f))
  
  and parseRecNat6 (r0, M, g, N, x, IStream.Cons ((Token.DOUBLEARROW, r), s)) =
        parseRecNat7 (r0, M, g, N, x, parseObjExp (IStream.expose s))
    | parseRecNat6 (r0, M, g, N, x, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.DOUBLEARROW, t)

  and parseRecNat7 (r0, M, g, N, x, (O, IStream.Cons ((Token.END, r), s))) =
        let val r' = Region.join (r0, r)
        in
            (IStream.expose s, r', Syntax.mkRecNat (M, g, N, (x, O), r'))
        end
    | parseRecNat7 (r0, M, g, N, x, (O, IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.END, t)


  and parseRecList2 (r0, M, g, IStream.Cons ((Token.DOUBLEARROW, r), s)) =
        parseRecList3 (r0, M, g, parseObjExp (IStream.expose s))
    | parseRecList2 (r0, M, g, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.DOUBLEARROW, t)

  and parseRecList3 (r0, M, g, (N, IStream.Cons ((Token.BAR, r), s))) =
        parseRecList4 (r0, M, g, N, IStream.expose s)
    | parseRecList3 (r0, M, g, (N, IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.BAR, t)

  and parseRecList4 (r0, M, g, N, IStream.Cons ((t as (Token.ID (Token.Lower, x)), r), s)) =
        if x=g then parseRecList5 (r0, M, g, N, parseObjExp (IStream.expose s))
        else Parsing.error (r, "Expected variable identifier `"^g^"', found " ^ Token.toString t)
    | parseRecList4  (r0, M, g, N, IStream.Cons ((t, r), s)) =
        Parsing.error (r, "Expected variable identifier `"^g^"', found " ^ Token.toString t)

  and parseRecList5 (r0, M, g, N, (O, f)) = (case Syntax.isCons (O) of
            NONE => Parsing.error (Syntax.getRegion (O),
          "Expected `(<var> :: <var>)' in recursion clause")
	  | SOME (x, xs) => parseRecList6 (r0, M, g, N, x, xs, f))
  
  and parseRecList6 (r0, M, g, N, x, xs, IStream.Cons ((Token.DOUBLEARROW, r), s)) =
        parseRecList7 (r0, M, g, N, x, xs, parseObjExp (IStream.expose s))
    | parseRecList6 (r0, M, g, N, x, xs, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.DOUBLEARROW, t)

  and parseRecList7 (r0, M, g, N, x, xs, (O, IStream.Cons ((Token.END, r), s))) =
        let val r' = Region.join (r0, r)
        in
           (IStream.expose s, r', Syntax.mkRecList (M, g, N, (x, xs, O), r'))
        end
    | parseRecList7 (r0, M, g, N, x, xs, (O, IStream.Cons ((t, r), s))) =
        Parsing.wrongTokenError (r, Token.END, t)

  (*  parseDecl (f) = ((name, A), f') 
  *) 
  and parseDecl (f) = parseAmbigDecl' (false, true, f)

  (*  parseAmbigDecl (f) = ((name, A), f') 
  *) 
  and parseAmbigDecl (f) = parseAmbigDecl' (true, false, f)
  and parseAmbigDeclLower (f) = parseAmbigDecl' (true, true, f)

  (*  parseAmbigDecl' (eqA, lower, f) = ((name, A), f') 
  *) 
  and parseAmbigDecl' (eqA, lower, IStream.Cons ((t as (Token.ID (C, name)), r), s)) =
        if lower andalso C = Token.Upper then
	  Parsing.error (r, "Invalid variable identifier " ^ Token.toString t 
            ^ " -- Variables start with a lowercase letter")
        else parseAmbigDecl'1 (eqA, r, name, IStream.expose s)
    | parseAmbigDecl' (eqA, lower, IStream.Cons ((t, r), s)) =
        Parsing.error (r, "Expected variable identifier, found " ^ Token.toString t)

  and parseAmbigDecl'1 (eqA, r0, name, IStream.Cons ((Token.COLON, r), s)) =
      let val (A, f') = parseAmbigExp' (eqA, IStream.expose s)
      in ((name, A), f') end
    | parseAmbigDecl'1 (eqA, r0, name, IStream.Cons ((t, r), s)) =
        Parsing.wrongTokenError (r, Token.COLON, t)

end (* structure ParseExp *)
