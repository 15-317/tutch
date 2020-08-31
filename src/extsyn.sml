(* $Id: extsyn.sml,v 1.1 2000/10/17 22:23:09 abel Exp $ *)

signature EXTSYN =
sig
    
    type prop (* = Prop.Prop * Paths.RegProp *)

    type hyp  (* = Proofs.Hyp * Paths.RegHyp *)
    type assertion (* = Proofs.Assertion * Paths.RegAssertion *)
    type proof (* = Proofs.Proof * Paths.RegProof *)
 
    (* construction functions *)
    val mkAtom    : string * Region.Region -> prop 
    val mkAnd     : prop * prop -> prop
    val mkOr      : prop * prop -> prop
    val mkImplies : prop * prop -> prop
    val mkTrue    : Region.Region -> prop
    val mkFalse   : Region.Region -> prop
    val mkNot     : prop * Region.Region -> prop
    val mkEquiv   : prop * prop -> prop

(*    val mkHyp     : string option * prop -> hyp *)
    val mkHyp     : prop -> hyp
    val mkLine    : prop -> assertion
    val mkFrame   : hyp * proof -> assertion
    val mkFinal   : assertion -> proof
    val mkStep    : assertion * proof -> proof

    (* selectors *)
    val getProp  : prop -> Prop.Prop
    val getProof : proof -> Proofs.Proof  
    val getRegionTreeProp  : prop  -> Paths.RegProp
    val getRegionTreeProof : proof -> Paths.RegProof
	
    type term
    type bind

    type annHyp
    type annAssertion
    type annProof

    val mkVar  : string * Region.Region -> term
    val mkPair : term * term -> term
    val mkFst  : term * Region.Region -> term
    val mkSnd  : term * Region.Region -> term
    val mkInl  : term * Region.Region -> term
    val mkInr  : term * Region.Region -> term
    val mkCase : term * bind * bind * Region.Region -> term
    val mkLam  : string * Region.Region * term -> term
    val mkApp  : term * term -> term
    val mkStar : Region.Region -> term
    val mkAbort: term * Region.Region -> term
    val mkAnn  : term * prop -> term

    val mkBind : string * Region.Region * term  -> bind

    val mkAnnHyp   : string * Region.Region * prop -> annHyp
    val mkAnnLine  : term * prop -> annAssertion
    val mkAnnFrame : annHyp * annProof -> annAssertion
    val mkAnnFinal : annAssertion -> annProof
    val mkAnnStep  : annAssertion * annProof -> annProof

    val getTerm           : term -> Term.Term
    val getAnnProof           : annProof -> AnnProof.Proof
    val getRegionTreeTerm : term -> Paths.RegTerm
    val getRegionTreeAnnProof : annProof -> Paths.RegAnnProof

end (* signature EXTSYN *)


structure ExtSyn :> EXTSYN =
struct
  
    type prop = Prop.Prop * Paths.RegProp

    type hyp  = Proofs.Hyp * Paths.RegHyp
    type assertion = Proofs.Assertion * Paths.RegAssertion
    type proof = Proofs.Proof * Paths.RegProof
 
    (* construction functions *)
    fun mkAtom    (s, r)           = (Prop.Atom (s), Paths.RegAtom (r))
    fun mkAnd     ((A, a), (B, b)) = (Prop.And (A, B), Paths.RegConn (a, b))
    fun mkOr      ((A, a), (B, b)) = (Prop.Or (A, B), Paths.RegConn (a, b))
    fun mkImplies ((A, a), (B, b)) = (Prop.Implies (A, B), Paths.RegConn (a, b))
    fun mkTrue    (r)              = (Prop.True, Paths.RegAtom (r))
    fun mkFalse   (r)              = (Prop.False, Paths.RegAtom (r))
    fun mkNot     ((A, a), r)      = (Prop.Not (A), Paths.RegPFix (a, r))
    fun mkEquiv   ((A, a), (B, b)) = (Prop.Equiv (A, B), Paths.RegConn (a, b))

    fun mkHyp     ((A, a))         = (Proofs.Hyp (A), Paths.RegHyp (a))
    fun mkLine    ((A, a))         = (Proofs.Line (A), Paths.RegLine (a))
    fun mkFrame   ((H, h), (P, p)) = (Proofs.Frame (H, P), Paths.RegFrame (h, p))
    fun mkFinal   ((E, e))         = (Proofs.Final (E), Paths.RegFinal (e)) 
    fun mkStep    ((E, e), (P, p)) = (Proofs.Step (E, P), Paths.RegStep (e, p))   
    (* selectors *)
    fun getProp  (A, a) = A
    fun getProof (P, p) = P
    fun getRegionTreeProp  (A, a) = a
    fun getRegionTreeProof (P, p) = p

    type term = Term.Term * Paths.RegTerm
    type bind = string * Region.Region * term
    
    type annHyp       = AnnProof.Hyp * Paths.RegAnnHyp
    type annAssertion = AnnProof.Assertion * Paths.RegAnnAssertion
    type annProof     = AnnProof.Proof * Paths.RegAnnProof

    fun mkVar   (x, r)           = (Term.Var (x), Paths.RegLeaf (r))
    fun mkPair  ((M, m), (N, n)) = (Term.Pair (M, N), Paths.RegBin (m, n))
    fun mkFst   ((M, m), r)      = (Term.Fst (M), Paths.RegUn (m, r))
    fun mkSnd   ((M, m), r)      = (Term.Snd (M), Paths.RegUn (m, r))
    fun mkInl   ((M, m), r)      = (Term.Inl (M), Paths.RegUn (m, r))
    fun mkInr   ((M, m), r)      = (Term.Inr (M), Paths.RegUn (m, r))
    fun mkCase  ((M, m), (x, r', (N, n)), (y, r'', (O, oo)), r) = (Term.Case (M, (x, N), (y, O)), Paths.RegTern (m, n, oo, r))
    fun mkLam   (x, r, (M, m))   = (Term.Lam (x, M), Paths.RegLam (r, m))
    fun mkApp   ((M, m), (N, n)) = (Term.App (M, N), Paths.RegBin (m, n))
    fun mkStar  (r)              = (Term.Star, Paths.RegLeaf (r))
    fun mkAbort ((M, m), r)      = (Term.Abort (M), Paths.RegUn (m, r))
    fun mkAnn   ((M, m), (A, a)) = (Term.Ann (M, A), Paths.RegAnn (m, a))

    fun mkBind  (x, r, (M, m))   = (x, r, (M, m))

    fun mkAnnHyp   (x, r, (A, a))   = (AnnProof.Hyp (x, A), Paths.RegAnnHyp (r, a))
    fun mkAnnLine  ((M, m), (A, a)) = (AnnProof.Line (M, A), Paths.RegAnnLine (m, a))
    fun mkAnnFrame ((H, h), (P, p)) = (AnnProof.Frame (H, P), Paths.RegAnnFrame (h, p))
    fun mkAnnFinal ((E, e))         = (AnnProof.Final (E), Paths.RegAnnFinal (e))
    fun mkAnnStep  ((E, e), (P, p)) = (AnnProof.Step (E, P), Paths.RegAnnStep (e, p))

    fun getTerm (M, m) = M
    fun getAnnProof (P, p) = P
    fun getRegionTreeTerm (M, m) = m
    fun getRegionTreeAnnProof (P, p) = p

end (* structure ExtSyn *)
