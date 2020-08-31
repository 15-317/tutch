(* $Id: syntax.sml,v 1.3 2000/10/24 21:09:54 abel Exp $ *)

structure  Category = struct
  datatype Category =
      Prop
    | Type
    | Term
    | Var    (* type var or term var *)
    | Zero   (* empty type or zero in nat *)

  fun toString (Prop) = "proposition"
    | toString (Type) = "type"
    | toString (Term) = "term"
    | toString (Var)  = "variable"
    | toString (Zero) = "0"
end (* structure Category *)

signature SYNTAX =
sig
    
    type Exp (* = Exp.Exp * Region.Exp * Category.Category *)

    (* construction functions *)

    (* propositions *)
    val mkAtom    : string * Region.Region -> Exp 
    val mkAnd     : Exp * Exp -> Exp
    val mkOr      : Exp * Exp -> Exp
    val mkImplies : Exp * Exp -> Exp
    val mkTruth   : Region.Region -> Exp
    val mkAbsurd  : Region.Region -> Exp

    val mkAll     : string * Exp * Region.Region * Exp -> Exp
    val mkExists  : string * Exp * Region.Region * Exp -> Exp
    val mkEqual   : Exp * Exp -> Exp
    val mkLess    : Exp * Exp -> Exp
    
    val mkNot     : Exp * Region.Region -> Exp
    val mkEquiv   : Exp * Exp -> Exp

    (* types *)
    val mkProd    : Exp * Exp -> Exp
    val mkSum     : Exp * Exp -> Exp
    val mkArrow   : Exp * Exp -> Exp
    val mkUnit    : Region.Region -> Exp
(*    val mkEmpty   : Region.Region -> Exp   -- mkZero *)
    val mkNat     : Region.Region -> Exp
    val mkBool    : Region.Region -> Exp
    val mkList    : Exp * Region.Region -> Exp
    
    type Bind 

    (* terms *)
    val mkVar  : string * Region.Region -> Exp
    val mkPair : Exp * Exp -> Exp
    val mkFst  : Exp * Region.Region -> Exp
    val mkSnd  : Exp * Region.Region -> Exp
    val mkInl  : Exp * Region.Region -> Exp
    val mkInr  : Exp * Region.Region -> Exp
    val mkCase : Exp * Bind * Bind * Region.Region -> Exp
    val mkLam  : string * Region.Region * Exp -> Exp
    val mkApp  : Exp * Exp -> Exp
    val mkStar : Region.Region -> Exp
    val mkAbort: Exp * Region.Region -> Exp
    val mkZero : Region.Region -> Exp
    val mkSucc : Exp * Region.Region -> Exp
    val mkRecNat : Exp * string * Exp * (string * Exp) * Region.Region -> Exp
    val mkTrue : Region.Region -> Exp
    val mkFalse: Region.Region -> Exp
    val mkIf   : Exp * Exp * Exp * Region.Region -> Exp
    val mkNil  : Region.Region -> Exp
    val mkCons : Exp * Exp -> Exp
    val mkRecList: Exp * string * Exp * (string * string * Exp) * Region.Region -> Exp
    val mkLetPair: string * string * Exp * Region.Region * Exp -> Exp
    val mkEq0  : Region.Region -> Exp
    val mkEqS  : Exp * Region.Region -> Exp
    val mkEqE0S: Exp * Region.Region -> Exp
    val mkEqES0: Exp * Region.Region -> Exp
    val mkEqESS: Exp * Region.Region -> Exp
    val mkLess0: Region.Region -> Exp
    val mkLessS: Exp * Region.Region -> Exp
    val mkLessE0:Exp * Region.Region -> Exp
    val mkLessES:Exp * Region.Region -> Exp
    val mkEqN  : Region.Region -> Exp
    val mkEqC  : Exp * Region.Region -> Exp
    val mkEqENC: Exp * Region.Region -> Exp
    val mkEqECN: Exp * Region.Region -> Exp
    val mkEqECC: Exp * Region.Region -> Exp
    val mkAnn  : Exp * Exp -> Exp

    val mkBind : string * Region.Region * Exp  -> Bind 

    val mkTerm : Exp.Exp * RegTree.Exp -> Exp
    val mkType : Exp.Exp * RegTree.Exp -> Exp
    val mkProp : Exp.Exp * RegTree.Exp -> Exp

    (* selectors *)

    val getExp        : Exp -> Exp.Exp
    val getRegion     : Exp -> Region.Region
    val getCategory   : Exp -> Category.Category
    val getRegTree    : Exp -> RegTree.Exp

    (* selectors which could raise an exception *)
    val getProp : Exp -> Exp.Exp
    val getType : Exp -> Exp.Exp
    val getTerm : Exp -> Exp.Exp
    val getUniv : Exp -> Univ.Univ
    val getUExp : Exp -> UExp.UExp
    val getTypeProp : Exp -> Exp.Exp

    (* for rec patterns: *)
    val isZero : Exp -> bool
    val isSucc : Exp -> string option
    val isNil  : Exp -> bool
    val isCons : Exp -> (string * string) option

    val toString : Exp -> string
 
    (* proofs *)
    type Hyp
    type Hyps
    type Assertion
    type Proof
    
    val mkHyp     : Exp -> Hyp
    val mkLast    : Hyp -> Hyps
    val mkHyps    : Hyp * Hyps -> Hyps
    val mkLine    : Exp -> Assertion
    val mkFrame   : Hyps * Proof -> Assertion
    val mkFinal   : Assertion -> Proof
    val mkStep    : Assertion * Proof -> Proof
   
    (* selectors *)
    val getProof        : Proof -> Proof.Proof
    val getRegTreeProof : Proof -> RegTree.Exp

end (* signature SYNTAX *)


structure Syntax :> SYNTAX =
struct

    structure C = Category

    fun toString (M, m, C.Term) = Exp.termToString (M)
      | toString (T, t, C.Type) = Exp.typeToString (T)
      | toString (A, a, C.Prop) = Exp.propToString (A)
      | toString (Z, z, C.Zero) = "0"
      | toString (Exp.Var(x), a, C.Var) = x

    (* Invariant: at least one Exp in the list is of the wrong category *)
    fun categoryMismatch ((Aa as (A, a, C), C') :: ps) = 
          if C = C' then categoryMismatch (ps)
	  else Parsing.error (RegTree.toRegion (a), "Category mismatch: "
            ^ toString (Aa) ^ " is a " ^ C.toString (C)^", but a "
            ^ C.toString (C') ^ " is expected in this place")

    fun categoryMismatch2 ((Aa as (A, a, C), C1, C2) :: ps) = 
          if C = C1 orelse C = C2 then categoryMismatch2(ps)
	  else Parsing.error (RegTree.toRegion (a), "Category mismatch: "
            ^ toString (Aa) ^ " is a " ^ C.toString (C)^", but a "
            ^ C.toString (C1) ^ " or a " ^ C.toString(C2) 
            ^ " is expected in this place")


    type Exp = Exp.Exp * RegTree.Exp * C.Category

    (* propositions *)

    fun mkAtom (s, r) = (Exp.Atom (s), RegTree.Atom (r), C.Prop)

    fun mkAnd ((A, a, C.Prop), (B, b, C.Prop)) = 
          (Exp.Sigma (Range.Prop, A, B), RegTree.Infix (a, b), C.Prop)
      | mkAnd (Aa, Bb) = categoryMismatch [(Aa, C.Prop), (Bb, C.Prop)]

    fun mkOr ((A, a, C.Prop), (B, b, C.Prop)) = 
          (Exp.Sum (A, B), RegTree.Infix (a, b), C.Prop)
      | mkOr (Aa, Bb) = categoryMismatch [(Aa, C.Prop), (Bb, C.Prop)]

    fun mkImplies ((A, a, C.Prop), (B, b, C.Prop)) = 
          (Exp.Pi (Range.Prop, A, B), RegTree.Infix (a, b), C.Prop)
      | mkImplies (Aa, Bb) = categoryMismatch [(Aa, C.Prop), (Bb, C.Prop)]

    fun mkTruth  (r) = (Exp.Unit, RegTree.Atom (r), C.Prop)
    fun mkAbsurd (r) = (Exp.Empty, RegTree.Atom (r), C.Prop)

    fun mkAll (x, (T, t, C.Type), r, (A, a, C.Prop)) =
          (Exp.Pi (Range.Dep (x), T, A), RegTree.Quant (t, r, a), C.Prop)
      | mkAll (x, (T, t, C.Zero), r, (A, a, C.Prop)) =
          (Exp.Pi (Range.Dep (x), Exp.Empty, A), RegTree.Quant (t, r, a), C.Prop)
      | mkAll (x, (Exp.Var (y), t, C.Var), r, (A, a, C.Prop)) =
          (Exp.Pi (Range.Dep (x), Exp.Atom (y), A), RegTree.Quant (t, r, a), C.Prop)
      | mkAll (x, Tt, r, Aa) = categoryMismatch [(Tt, C.Type), (Aa, C.Prop)]

    fun mkExists (x, (T, t, C.Type), r, (A, a, C.Prop)) =
          (Exp.Sigma (Range.Dep (x), T, A), RegTree.Quant (t, r, a), C.Prop)
      | mkExists (x, (T, t, C.Zero), r, (A, a, C.Prop)) =
          (Exp.Sigma (Range.Dep (x), Exp.Empty, A), RegTree.Quant (t, r, a), C.Prop)
      | mkExists (x, (Exp.Var (y), t, C.Var), r, (A, a, C.Prop)) =
          (Exp.Sigma (Range.Dep (x), Exp.Atom (y), A), RegTree.Quant (t, r, a), C.Prop)
      | mkExists (x, Tt, r, Aa) = categoryMismatch [(Tt, C.Type), (Aa, C.Prop)]

    fun mkEqual ((M, m, C.Term), (N, n, C.Term)) =
          (Exp.Equal (M, N), RegTree.Infix (m, n), C.Prop)
      | mkEqual ((M, m, C.Var), Nn) = mkEqual ((M, m, C.Term), Nn)
      | mkEqual (Mm, (N, n, C.Var)) = mkEqual (Mm, (N, n, C.Term))
      | mkEqual ((M, m, C.Zero), Nn) = mkEqual ((M, m, C.Term), Nn)
      | mkEqual (Mm, (N, n, C.Zero)) = mkEqual (Mm, (N, n, C.Term))
      | mkEqual (Mm, Nn) = categoryMismatch [(Mm, C.Term), (Nn, C.Term)]

    fun mkLess ((M, m, C.Term), (N, n, C.Term)) =
          (Exp.Less (M, N), RegTree.Infix (m, n), C.Prop)
      | mkLess ((M, m, C.Var), Nn) = mkLess ((M, m, C.Term), Nn)
      | mkLess (Mm, (N, n, C.Var)) = mkLess (Mm, (N, n, C.Term))
      | mkLess ((M, m, C.Zero), Nn) = mkLess ((M, m, C.Term), Nn)
      | mkLess (Mm, (N, n, C.Zero)) = mkLess (Mm, (N, n, C.Term))
      | mkLess (Mm, Nn) = categoryMismatch [(Mm, C.Term), (Nn, C.Term)]

    fun mkNot ((A, a, C.Prop), r) = (Exp.Not (A), RegTree.PFix (r, a), C.Prop)
      | mkNot (Aa, r) = categoryMismatch [(Aa, C.Prop)]

    fun mkEquiv ((A, a, C.Prop), (B, b, C.Prop)) =
          (Exp.Equiv (A, B), RegTree.Infix (a, b), C.Prop)
      | mkEquiv (Aa, Bb) = categoryMismatch [(Aa, C.Prop), (Bb, C.Prop)]


    (* types *)

    fun mkProd ((S, s, C.Type), (T, t, C.Type)) =
         (Exp.Sigma (Range.Type, S, T), RegTree.Infix (s, t), C.Type)
      | mkProd ((Exp.Var (x), s, C.Var), Tt) = mkProd ((Exp.Atom (x), s, C.Type), Tt)
      | mkProd (Ss, (Exp.Var (x), t, C.Var)) = mkProd (Ss, (Exp.Atom (x), t, C.Type))
      | mkProd ((S, s, C.Zero), Tt) = mkProd ((Exp.Empty, s, C.Type), Tt)
      | mkProd (Ss, (T, t, C.Zero)) = mkProd (Ss, (Exp.Empty, t, C.Type))
      | mkProd (Ss, Tt) = categoryMismatch [(Ss, C.Type), (Tt, C.Type)]

    fun mkSum ((S, s, C.Type), (T, t, C.Type)) =
         (Exp.Sum (S, T), RegTree.Infix (s, t), C.Type)
      | mkSum ((Exp.Var (x), s, C.Var), Tt) = mkSum ((Exp.Atom (x), s, C.Type), Tt)
      | mkSum (Ss, (Exp.Var (x), t, C.Var)) = mkSum (Ss, (Exp.Atom (x), t, C.Type))
      | mkSum ((S, s, C.Zero), Tt) = mkSum ((Exp.Empty, s, C.Type), Tt)
      | mkSum (Ss, (T, t, C.Zero)) = mkSum (Ss, (Exp.Empty, t, C.Type))
      | mkSum (Ss, Tt) = categoryMismatch [(Ss, C.Type), (Tt, C.Type)]

    fun mkArrow ((S, s, C.Type), (T, t, C.Type)) =
         (Exp.Pi (Range.Type, S, T), RegTree.Infix (s, t), C.Type)
      | mkArrow ((Exp.Var (x), s, C.Var), Tt) = mkArrow ((Exp.Atom (x), s, C.Type), Tt)
      | mkArrow (Ss, (Exp.Var (x), t, C.Var)) = mkArrow (Ss, (Exp.Atom (x), t, C.Type))
      | mkArrow ((S, s, C.Zero), Tt) = mkArrow ((Exp.Empty, s, C.Type), Tt)
      | mkArrow (Ss, (T, t, C.Zero)) = mkArrow (Ss, (Exp.Empty, t, C.Type))
      | mkArrow (Ss, Tt) = categoryMismatch [(Ss, C.Type), (Tt, C.Type)]

    fun mkUnit  (r) = (Exp.Unit, RegTree.Atom (r), C.Type)
(*    fun mkEmpty (r) = (Exp.Empty, RegTree.Atom (r), C.Type)  -- mkZero*)
    fun mkNat  (r) = (Exp.Nat, RegTree.Atom (r), C.Type)
    fun mkBool (r) = (Exp.Bool, RegTree.Atom (r), C.Type)

    fun mkList ((T, t, C.Type), r) = 
         (Exp.List (T), RegTree.PFix (r, t), C.Type)
      | mkList ((Exp.Var (x), t, C.Var), r) = mkList ((Exp.Atom (x), t, C.Type), r)
      | mkList ((T, t, C.Zero), r) = mkList ((Exp.Empty, t, C.Type), r)
      | mkList (Tt, r) = categoryMismatch [(Tt, C.Type)]

    (* terms *)

    type Bind = string * Region.Region * Exp

    fun mkVar (x, r) = (Exp.Var (x), RegTree.Atom (r), C.Var)

    fun mkPair ((M, m, C.Term), (N, n, C.Term)) = 
          (Exp.Pair (M, N), RegTree.Infix (m, n), C.Term)
      | mkPair ((M, m, C.Var), Nn) = mkPair ((M, m, C.Term), Nn)
      | mkPair (Mm, (N, n, C.Var)) = mkPair (Mm, (N, n, C.Term))
      | mkPair ((M, m, C.Zero), Nn) = mkPair ((M, m, C.Term), Nn)
      | mkPair (Mm, (N, n, C.Zero)) = mkPair (Mm, (N, n, C.Term))
      | mkPair (Mm, Nn) = categoryMismatch [(Mm, C.Term), (Nn, C.Term)]

    fun mkFst ((M, m, C.Term), r) = (Exp.Fst (M), RegTree.PFix (r, m), C.Term)
      | mkFst ((M, m, C.Var), r) = mkFst ((M, m, C.Term), r)
      | mkFst ((M, m, C.Zero), r) = mkFst ((M, m, C.Term), r)
      | mkFst (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkSnd ((M, m, C.Term), r) = (Exp.Snd (M), RegTree.PFix (r, m), C.Term)
      | mkSnd ((M, m, C.Var), r) = mkSnd ((M, m, C.Term), r)
      | mkSnd ((M, m, C.Zero), r) = mkSnd ((M, m, C.Term), r)
      | mkSnd (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkInl ((M, m, C.Term), r) = (Exp.Inl (M), RegTree.PFix (r, m), C.Term)
      | mkInl ((M, m, C.Var), r) = mkInl ((M, m, C.Term), r)
      | mkInl ((M, m, C.Zero), r) = mkInl ((M, m, C.Term), r)
      | mkInl (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkInr ((M, m, C.Term), r) = (Exp.Inr (M), RegTree.PFix (r, m), C.Term)
      | mkInr ((M, m, C.Var), r) = mkInr ((M, m, C.Term), r)
      | mkInr ((M, m, C.Zero), r) = mkInr ((M, m, C.Term), r)
      | mkInr (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkCase ((M, m, C.Term), (x, r', (N, n, C.Term)), (y, r'', (O, oo, C.Term)), r) = 
          (Exp.Case (M, (x, N), (y, O)), RegTree.Tern (m, n, oo, r), C.Term)
      | mkCase ((M, m, C.Var), Nn, Oo, r) = mkCase ((M, m, C.Term), Nn, Oo, r)
      | mkCase ((M, m, C.Zero), Nn, Oo, r) = mkCase ((M, m, C.Term), Nn, Oo, r)
      | mkCase (Mm, (x, r', (N, n, C.Var)), Oo, r) = mkCase (Mm, (x, r', (N, n, C.Term)), Oo, r)
      | mkCase (Mm, (x, r', (N, n, C.Zero)), Oo, r) = mkCase (Mm, (x, r', (N, n, C.Term)), Oo, r)
      | mkCase (Mm, Nn, (y, r'', (O, oo, C.Var)), r) = mkCase (Mm, Nn, (y, r'', (O, oo, C.Term)), r)
      | mkCase (Mm, Nn, (y, r'', (O, oo, C.Zero)), r) = mkCase (Mm, Nn, (y, r'', (O, oo, C.Term)), r)
      | mkCase (Mm, (x, r', Nn), (y, r'', Oo), r) = 
          categoryMismatch [(Mm, C.Term), (Nn, C.Term), (Oo, C.Term)]

    fun mkLam (x, r, (M, m, C.Term)) = (Exp.Lam (x, M), RegTree.PFix (r, m), C.Term)
      | mkLam (x, r, (M, m, C.Var)) = mkLam (x, r, (M, m, C.Term))
      | mkLam (x, r, (M, m, C.Zero)) = mkLam (x, r, (M, m, C.Term))
      | mkLam (x, r, Mm) = categoryMismatch [(Mm, C.Term)]

    fun mkApp ((M, m, C.Term), (N, n, C.Term)) = 
          (Exp.App (M, N), RegTree.Infix (m, n), C.Term)
      | mkApp ((M, m, C.Prop), (N, n, C.Term)) = 
          (Exp.App (M, N), RegTree.Infix (m, n), C.Prop)
      | mkApp ((M, m, C.Type), (N, n, C.Term)) = 
          categoryMismatch2 [((M, m, C.Type), C.Term, C.Prop)]
      | mkApp ((M, m, C.Var), Nn) = mkApp ((M, m, C.Term), Nn)    (* with dependent types, we will run into trouble here! *)
      | mkApp (Mm, (N, n, C.Var)) = mkApp (Mm, (N, n, C.Term))
      | mkApp ((M, m, C.Zero), Nn) = mkApp ((M, m, C.Term), Nn)
      | mkApp (Mm, (N, n, C.Zero)) = mkApp (Mm, (N, n, C.Term))
      | mkApp (Mm, Nn) = categoryMismatch [(Nn, C.Term)]

    fun mkStar (r) = (Exp.Star, RegTree.Atom (r), C.Term)
    fun mkAbort ((M, m, C.Term), r) = (Exp.Abort (M), RegTree.PFix (r, m), C.Term)
      | mkAbort ((M, m, C.Var), r) = mkAbort ((M, m, C.Term), r)
      | mkAbort ((M, m, C.Zero), r) = mkAbort ((M, m, C.Term), r)
      | mkAbort (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkZero (r) = (Exp.Zero, RegTree.Atom (r), C.Term)
    fun mkSucc ((M, m, C.Term), r) = (Exp.Succ (M), RegTree.PFix (r, m), C.Term)
      | mkSucc ((M, m, C.Var), r) = mkSucc ((M, m, C.Term), r)
      | mkSucc ((M, m, C.Zero), r) = mkSucc ((M, m, C.Term), r)
      | mkSucc (Mm, r) = categoryMismatch [(Mm, C.Term)]

    (* Defining types by recursion will cause trouble *)
    fun mkRecNat ((M, m, C.Term), f, (N, n, C.Term), (x, (O, oo, C.Term)), r)= 
          (Exp.RecNat (M, f, N, (x, O)), RegTree.Tern (m, n, oo, r), C.Term)
      | mkRecNat ((M, m, C.Term), f, (N, n, C.Prop), (x, (O, oo, C.Prop)), r)= 
          (Exp.RecNat (M, f, N, (x, O)), RegTree.Tern (m, n, oo, r), C.Prop)
      | mkRecNat ((M, m, C.Var), f, Nn, Oo, r) = mkRecNat ((M, m, C.Term), f, Nn, Oo, r)
      | mkRecNat ((M, m, C.Zero), f, Nn, Oo, r) = mkRecNat ((M, m, C.Term), f, Nn, Oo, r)
      | mkRecNat (Mm, f, (N, n, C.Var), Oo, r) = mkRecNat (Mm, f, (N, n, C.Term), Oo, r)
      | mkRecNat (Mm, f, (N, n, C.Zero), Oo, r) = mkRecNat (Mm, f, (N, n, C.Term), Oo, r)
      | mkRecNat (Mm, f, Nn, (x, (O, oo, C.Var)), r) = mkRecNat (Mm, f, Nn, (x, (O, oo, C.Term)), r)
      | mkRecNat (Mm, f, Nn, (x, (O, oo, C.Zero)), r) = mkRecNat (Mm, f, Nn, (x, (O, oo, C.Term)), r)
      | mkRecNat (Mm, f, Nn, (x, Oo), r) = 
          categoryMismatch2 [(Mm, C.Term, C.Term), (Nn, C.Term, C.Prop), (Oo, C.Term, C.Prop)]

    fun mkTrue (r) = (Exp.True, RegTree.Atom (r), C.Term)
    fun mkFalse (r) = (Exp.False, RegTree.Atom (r), C.Term)
    fun mkIf ((M, m, C.Term), (N, n, C.Term), (O, oo, C.Term), r) =
          (Exp.If (M, N, O), RegTree.Tern' (m, n, r, oo), C.Term)
      | mkIf ((M, m, C.Var), Nn, Oo, r) = mkIf ((M, m, C.Term), Nn, Oo, r)
      | mkIf (Mm, (N, n, C.Var), Oo, r) = mkIf (Mm, (N, n, C.Term), Oo, r)
      | mkIf (Oo, Mm, (N, n, C.Var), r) = mkIf (Oo, Mm, (N, n, C.Term), r)
      | mkIf ((M, m, C.Zero), Nn, Oo, r) = mkIf ((M, m, C.Term), Nn, Oo, r)
      | mkIf (Mm, (N, n, C.Zero), Oo, r) = mkIf (Mm, (N, n, C.Term), Oo, r)
      | mkIf (Oo, Mm, (N, n, C.Zero), r) = mkIf (Oo, Mm, (N, n, C.Term), r)
      | mkIf (Mm, Nn, Oo, r) = 
          categoryMismatch [(Mm, C.Term), (Nn, C.Term), (Oo, C.Term)]

    fun mkNil (r) = (Exp.Nil, RegTree.Atom (r), C.Term)
    fun mkCons ((M, m, C.Term), (N, n, C.Term)) = 
          (Exp.Cons (M, N), RegTree.Infix (m, n), C.Term)
      | mkCons ((M, m, C.Var), Nn) = mkCons ((M, m, C.Term), Nn)
      | mkCons (Mm, (N, n, C.Var)) = mkCons (Mm, (N, n, C.Term))
      | mkCons ((M, m, C.Zero), Nn) = mkCons ((M, m, C.Term), Nn)
      | mkCons (Mm, (N, n, C.Zero)) = mkCons (Mm, (N, n, C.Term))
      | mkCons (Mm, Nn) = categoryMismatch [(Mm, C.Term), (Nn, C.Term)]

    fun mkRecList ((M, m, C.Term), f, (N, n, C.Term), (x, xs, (O, oo, C.Term)), r) = 
         (Exp.RecList (M, f, N, (x, xs, O)), RegTree.Tern (m, n, oo, r), C.Term)
      | mkRecList ((M, m, C.Term), f, (N, n, C.Prop), (x, xs, (O, oo, C.Prop)), r)= 
          (Exp.RecList (M, f, N, (x, xs, O)), RegTree.Tern (m, n, oo, r), C.Prop)
      | mkRecList ((M, m, C.Var), f, Nn, Oo, r) = mkRecList ((M, m, C.Term), f, Nn, Oo, r)
      | mkRecList ((M, m, C.Zero), f, Nn, Oo, r) = mkRecList ((M, m, C.Term), f, Nn, Oo, r)
      | mkRecList (Mm, f, (N, n, C.Var), Oo, r) = mkRecList (Mm, f, (N, n, C.Term), Oo, r)
      | mkRecList (Mm, f, (N, n, C.Zero), Oo, r) = mkRecList (Mm, f, (N, n, C.Term), Oo, r)
      | mkRecList (Mm, f, Nn, (x, xs, (O, oo, C.Var)), r) = mkRecList (Mm, f, Nn, (x, xs, (O, oo, C.Term)), r)
      | mkRecList (Mm, f, Nn, (x, xs, (O, oo, C.Zero)), r) = mkRecList (Mm, f, Nn, (x, xs, (O, oo, C.Term)), r)
      | mkRecList (Mm, f, Nn, (x, xs, Oo), r) = 
          categoryMismatch [(Mm, C.Term), (Nn, C.Term), (Oo, C.Term)]

    fun mkLetPair (x, y, (M, m, C.Term), r, (N, n, C.Term)) = 
          (Exp.LetPair (M, (x, y, N)), RegTree.Quant (m, r, n), C.Term)
      | mkLetPair (x, y, (M, m, C.Var), r, Nn) = mkLetPair (x, y, (M, m, C.Term), r, Nn)
      | mkLetPair (x, y, Mm, r, (N, n, C.Var)) = mkLetPair (x, y, Mm, r, (N, n, C.Term))
      | mkLetPair (x, y, (M, m, C.Zero), r, Nn) = mkLetPair (x, y, (M, m, C.Term), r, Nn)
      | mkLetPair (x, y, Mm, r, (N, n, C.Zero)) = mkLetPair (x, y, Mm, r, (N, n, C.Term))
      | mkLetPair (x, y, Mm, r, Nn) = categoryMismatch [(Mm, C.Term), (Nn, C.Term)]

    fun mkEq0 (r) = (Exp.Eq0, RegTree.Atom (r), C.Term)

    fun mkEqS ((M, m, C.Term), r) = (Exp.EqS (M), RegTree.PFix (r, m), C.Term)
      | mkEqS ((M, m, C.Var), r) = mkEqS ((M, m, C.Term), r)
      | mkEqS ((M, m, C.Zero), r) = mkEqS ((M, m, C.Term), r)
      | mkEqS (Mm, r) = categoryMismatch [(Mm, C.Term)]
      
    fun mkEqE0S ((M, m, C.Term), r) = (Exp.EqE0S (M), RegTree.PFix (r, m), C.Term)
      | mkEqE0S ((M, m, C.Var), r) = mkEqE0S ((M, m, C.Term), r)
      | mkEqE0S ((M, m, C.Zero), r) = mkEqE0S ((M, m, C.Term), r)
      | mkEqE0S (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkEqES0 ((M, m, C.Term), r) = (Exp.EqES0 (M), RegTree.PFix (r, m), C.Term)
      | mkEqES0 ((M, m, C.Var), r) = mkEqES0 ((M, m, C.Term), r)
      | mkEqES0 ((M, m, C.Zero), r) = mkEqES0 ((M, m, C.Term), r)
      | mkEqES0 (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkEqESS ((M, m, C.Term), r) = (Exp.EqESS (M), RegTree.PFix (r, m), C.Term)
      | mkEqESS ((M, m, C.Var), r) = mkEqESS ((M, m, C.Term), r)
      | mkEqESS ((M, m, C.Zero), r) = mkEqESS ((M, m, C.Term), r)
      | mkEqESS (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkLess0 (r) = (Exp.Less0, RegTree.Atom (r), C.Term)

    fun mkLessS ((M, m, C.Term), r) = (Exp.LessS (M), RegTree.PFix (r, m), C.Term)
      | mkLessS ((M, m, C.Var), r) = mkLessS ((M, m, C.Term), r)
      | mkLessS ((M, m, C.Zero), r) = mkLessS ((M, m, C.Term), r)
      | mkLessS (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkLessE0 ((M, m, C.Term), r) = (Exp.LessE0 (M), RegTree.PFix (r, m), C.Term)
      | mkLessE0 ((M, m, C.Var), r) = mkLessE0 ((M, m, C.Term), r)
      | mkLessE0 ((M, m, C.Zero), r) = mkLessE0 ((M, m, C.Term), r)
      | mkLessE0 (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkLessES ((M, m, C.Term), r) = (Exp.LessES (M), RegTree.PFix (r, m), C.Term)
      | mkLessES ((M, m, C.Var), r) = mkLessES ((M, m, C.Term), r)
      | mkLessES ((M, m, C.Zero), r) = mkLessES ((M, m, C.Term), r)
      | mkLessES (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkEqN (r) = (Exp.EqN, RegTree.Atom (r), C.Term)

    fun mkEqC ((M, m, C.Term), r) = (Exp.EqC (M), RegTree.PFix (r, m), C.Term)
      | mkEqC ((M, m, C.Var), r) = mkEqC ((M, m, C.Term), r)
      | mkEqC ((M, m, C.Zero), r) = mkEqC ((M, m, C.Term), r)
      | mkEqC (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkEqENC ((M, m, C.Term), r) = (Exp.EqENC (M), RegTree.PFix (r, m), C.Term)
      | mkEqENC ((M, m, C.Var), r) = mkEqENC ((M, m, C.Term), r)
      | mkEqENC ((M, m, C.Zero), r) = mkEqENC ((M, m, C.Term), r)
      | mkEqENC (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkEqECN ((M, m, C.Term), r) = (Exp.EqECN (M), RegTree.PFix (r, m), C.Term)
      | mkEqECN ((M, m, C.Var), r) = mkEqECN ((M, m, C.Term), r)
      | mkEqECN ((M, m, C.Zero), r) = mkEqECN ((M, m, C.Term), r)
      | mkEqECN (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkEqECC ((M, m, C.Term), r) = (Exp.EqECC (M), RegTree.PFix (r, m), C.Term)
      | mkEqECC ((M, m, C.Var), r) = mkEqECC ((M, m, C.Term), r)
      | mkEqECC ((M, m, C.Zero), r) = mkEqECC ((M, m, C.Term), r)
      | mkEqECC (Mm, r) = categoryMismatch [(Mm, C.Term)]

    fun mkAnn ((M, m, C.Term), (T, t, C.Type)) =
          (Exp.Ann (M, (T, Univ.Type)), RegTree.Infix (m, t), C.Term)
      | mkAnn ((M, m, C.Term), (T, t, C.Prop)) =
          (Exp.Ann (M, (T, Univ.Prop)), RegTree.Infix (m, t), C.Term)
      | mkAnn ((M, m, C.Var), Nn) = mkAnn ((M, m, C.Term), Nn)
      | mkAnn (Mm, (Exp.Var (x), n, C.Var)) = mkAnn (Mm, (Exp.Atom (x), n, C.Type))
      | mkAnn ((M, m, C.Zero), Nn) = mkAnn ((M, m, C.Term), Nn)
      | mkAnn (Mm, (N, n, C.Zero)) = mkAnn (Mm, (Exp.Empty, n, C.Type))
      | mkAnn (Mm, Tt) = categoryMismatch [(Mm, C.Term), (Tt, C.Type)]


    fun mkBind  (x, r, (M, m, C))   = (x, r, (M, m, C)) 


    fun mkTerm (M, m) = (M, m, C.Term)
    fun mkType (T, t) = (T, t, C.Type)
    fun mkProp (A, a) = (A, a, C.Prop)

    (* for rec patterns: *)
    fun isZero (Exp.Zero, m, C) = true
      | isZero (M, m, C) = false

    fun isSucc (Exp.Succ (Exp.Var (x)), m, C) = SOME x
      | isSucc (M, m, C) = NONE

    fun isNil  (Exp.Nil, m, C) = true
      | isNil  (M, m, C) = false

    fun isCons (Exp.Cons (Exp.Var (x), Exp.Var (xs)), m, C) = SOME (x, xs)
      | isCons (M, m, C) = NONE


    (* selectors *)
    fun getExp      (A, a, C) = A
    fun getRegTree  (A, a, C) = a
    fun getCategory (A, a, C) = C

    fun getRegion   (A, a, C) = RegTree.toRegion (a)

    (* selectors with could raise exception *)
    fun getProp (A, a, C.Prop) = A
      | getProp Aa = categoryMismatch [(Aa, C.Prop)]
(*
Parsing.error (RegTree.toRegion (a), 
          "Expected proposition, but found the "^C.toString (C)^" "^Exp.toString (A))
*)
    fun getType (T, t, C.Type) = T
      | getType (Exp.Var(x), t, C.Var) = Exp.Atom (x)
      | getType (T, t, C.Zero) = Exp.Empty
      | getType Tt = categoryMismatch [(Tt, C.Type)]

    fun getTerm (M, m, C.Term) = M
      | getTerm (M, m, C.Var) = M
      | getTerm (M, m, C.Zero) = M
      | getTerm Mm = categoryMismatch [(Mm, C.Term)]

    fun getUniv (T, t, C.Type) = Univ.Type
      | getUniv (A, a, C.Prop) = Univ.Prop
      | getUniv (Tt as (M, m, C.Term)) = categoryMismatch2 [(Tt, C.Type, C.Prop)]
      | getUniv (T, t, C) = Univ.Type

    fun getUExp (A, a, C.Prop) = (A, Univ.Prop)
      | getUExp (T, t, C.Type) = (T, Univ.Type)
      | getUExp (Exp.Var(x), t, C.Var) = (Exp.Atom (x), Univ.Type)
      | getUExp (T, t, C.Zero) = (Exp.Empty, Univ.Type)
      | getUExp (Tt as (M, m, C.Term)) = categoryMismatch2 [(Tt, C.Type, C.Prop)]
    
    fun getTypeProp (T, t, C.Type) = T
      | getTypeProp (A, a, C.Prop) = A
      | getTypeProp (Exp.Var(x), t, C.Var) = Exp.Atom (x)
      | getTypeProp (T, t, C.Zero) = Exp.Empty
      | getTypeProp Tt = categoryMismatch2 [(Tt, C.Type, C.Prop)]

    (* proofs *)

    type Hyp       = Proof.Hyp  * RegTree.Exp
    type Hyps      = Proof.Hyps * RegTree.Exp
    type Assertion = Proof.Assertion * RegTree.Exp
    type Proof     = Proof.Proof * RegTree.Exp

    fun mkHyp  ((Exp.Ann (Exp.Var (x), (T, Univ.Type)), a, C.Term)) = (Proof.Var (x, T), a)
      | mkHyp  ((A, a, C.Prop))  = (Proof.Ass (A), a)
      | mkHyp  (Mm as (M, m, C)) = Parsing.error (RegTree.toRegion (m), 
          "The expression "^toString(Mm)^" is not a valid assumption") 

    fun mkLast ((H, h))       = (Proof.Last (H), h)
    fun mkHyps ((H, h),(Hs, hs)) = (Proof.Ext (H, Hs), RegTree.Infix (h, hs))
    fun mkLine ((Exp.Ann (M, (T, Univ.Type)), a, C.Term))  = (Proof.Line (Proof.HasType (M, T)), a)
      | mkLine ((A, a, C.Prop))  = (Proof.Line (Proof.IsTrue (A)), a)
      | mkLine (Aa as (A, a, C)) = Parsing.error (RegTree.toRegion (a),
          "Expected assertion, found "^toString(Aa))
    fun mkFrame((Hs, hs), (P, p))= (Proof.Frame (Hs, P), RegTree.Infix (hs, p))
    fun mkFinal((E, e))          = (Proof.Final (E), e) 
    fun mkStep ((E, e), (P, p))  = (Proof.Step (E, P), RegTree.Infix (e, p))  

    fun getProof        (P, p) = P
    fun getRegTreeProof (P, p) = p

end (* structure Syntax *)
