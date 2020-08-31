(* $Id: paths.sml,v 1.1 2000/10/17 22:23:11 abel Exp $ *)

signature PATHS =
sig

  (* Region trees *)
  (* corresponding to the data structures of tutch *)

  datatype RegProp =
      RegAtom of Region.Region                 (* P, T, F  *)  
    | RegConn of RegProp * RegProp      (* &, |, =>, <=> *)
    | RegPFix of RegProp * Region.Region       (* ~        *)

  datatype RegHyp = RegHyp of RegProp   (* A        *)
  datatype RegAssertion = 
      RegLine of RegProp                (* A        *) 
    | RegFrame of RegHyp * RegProof     (* [A; P]   *)
  and RegProof =
      RegFinal of RegAssertion          (* E        *)
    | RegStep of RegAssertion * RegProof(* E ; P    *)

  (* Paths *)
  datatype Path =
      Here                              (* #                            *)
    | Left of Path                      (* # &|=><=> A, [#; P], ~#      *) 
                                        (* (#,M), # M, (#:A),case # of..*)
    | Middle of Path                    (* case M of inl x => # | inr B *)
    | Right of Path                     (* A &|=><=> #, [A; #]          *)
                                        (* (M,#), M #, M:#, .inr x => #.*)
    | This of Path                      (* x : #, Line, Final           *)
    | Next of Path                      (* E ; #                        *)

  (* Reverse paths, for accumulation in descent through a structure *)
  datatype RevPath =
      top
    | left of RevPath
    | middle of RevPath
    | right of RevPath
    | this of RevPath
    | next of RevPath
    
  val rev : RevPath -> Path
  val rev' : RevPath * Path -> Path
  val toRegionProp  : Path * RegProp -> Region.Region
  val toRegionProof : Path * RegProof -> Region.Region


  datatype RegTerm =
      RegLeaf of Region.Region
    | RegUn of RegTerm * Region.Region
    | RegLam of Region.Region * RegTerm (* CAUTION: Region is NOT the whole region in this case, needs join!! *)
    | RegBin of RegTerm * RegTerm
    | RegTern of RegTerm * RegTerm * RegTerm * Region.Region   (* case, rec *)
    | RegTern' of RegTerm * RegTerm * RegTerm * Region.Region  (* if, NEEDS JOIN *)
    | RegAnn of RegTerm * RegProp

  datatype RegAnnHyp = RegAnnHyp of Region.Region * RegProp  (* x : A    *)
    (* CAUTION: Region is NOT the whole region in this case, needs join!! *)
  datatype RegAnnAssertion = 
      RegAnnLine of RegTerm * RegProp                 (* M : A    *) 
    | RegAnnFrame of RegAnnHyp * RegAnnProof          (* [x:A; P] *)
  and RegAnnProof =
      RegAnnFinal of RegAnnAssertion                  (* E        *)
    | RegAnnStep of RegAnnAssertion * RegAnnProof     (* E ; P    *)

  val treeToRegionTerm : RegTerm -> Region.Region

  val toRegionTerm : Path * RegTerm -> Region.Region
  val toRegionAnnProof : Path * RegAnnProof -> Region.Region

end; (* signature PATH *)


structure Paths :> PATHS =
struct

  (* Region trees *)
  (* corresponding to the data structures of tutch *)

  datatype RegProp =
      RegAtom of Region.Region                 (* P, T, F  *)  
    | RegConn of RegProp * RegProp      (* &, |, => *)
    | RegPFix of RegProp * Region.Region       (* ~        *)

  datatype RegHyp = RegHyp of RegProp   (* x : A    *)
  datatype RegAssertion = 
      RegLine of RegProp                (* A        *) 
    | RegFrame of RegHyp * RegProof     (* [x:A; P] *)
  and RegProof =
      RegFinal of RegAssertion          (* E        *)
    | RegStep of RegAssertion * RegProof(* E ; P    *)

  (* Paths *)
  datatype Path =
      Here                              (* #                            *)
    | Left of Path                      (* # &|=><=> A, [#; P], ~#      *) 
                                        (* (#,M), # M, (#:A),case # of..*)
    | Middle of Path                    (* case M of inl # | inr B      *)
    | Right of Path                     (* A &|=><=> #, [A; #]          *)
                                        (* (M,#), M #, M:#, ..inr # end *)
    | This of Path                      (* x : #, Line, Final, x => #   *)
    | Next of Path                      (* E ; #                        *)

  (* Reverse paths, for accumulation in descent through a structure *)
  datatype RevPath =
      top
    | left of RevPath
    | middle of RevPath
    | right of RevPath
    | this of RevPath
    | next of RevPath
    
  fun rev' (top, p) = p
    | rev' (left (rp),  p) = rev' (rp, Left (p))
    | rev' (middle (rp),p) = rev' (rp, Middle (p))
    | rev' (right (rp), p) = rev' (rp, Right (p))
    | rev' (this (rp),  p) = rev' (rp, This (p))
    | rev' (next (rp),  p) = rev' (rp, Next (p))
  fun rev (rp) = rev' (rp, Here) 
 
  (*  convert region trees into regions *)
  fun treeToRegionProp (RegAtom (r)) = r
    | treeToRegionProp (RegPFix (rt, r)) = r
    | treeToRegionProp (RegConn (rt1, rt2)) = 
        Region.join (treeToRegionProp (rt1), treeToRegionProp (rt2))

  fun treeToRegionHyp (RegHyp (rt)) = treeToRegionProp (rt)

  fun treeToRegionAssertion (RegLine (rt)) = treeToRegionProp (rt)
    | treeToRegionAssertion (RegFrame (rt1, rt2)) = 
        Region.join (treeToRegionHyp (rt1), treeToRegionProof (rt2))
  and treeToRegionProof (RegFinal (rt)) = treeToRegionAssertion (rt)
    | treeToRegionProof (RegStep (rt1, rt2)) = 
        Region.join (treeToRegionAssertion (rt1), treeToRegionProof (rt2))
  
  (*  calculate region for a path *)
  fun toRegionProp (Here,  rt) = treeToRegionProp (rt)
    | toRegionProp (Left (p), RegConn (rt1, rt2)) = toRegionProp (p, rt1)
    | toRegionProp (Left (p), RegPFix (rt1, r))   = toRegionProp (p, rt1) 
    | toRegionProp (Right(p), RegConn (rt1, rt2)) = toRegionProp (p, rt2)
  (* all others impossible *)

  fun toRegionHyp (Here, rt) = treeToRegionHyp (rt)
    | toRegionHyp (This(p), RegHyp (rt)) = toRegionProp (p, rt)
  (* all others impossible *)

  fun toRegionAssertion (Here, rt) = treeToRegionAssertion (rt)
    | toRegionAssertion (This (p), RegLine (rt)) = toRegionProp (p, rt) 
    | toRegionAssertion (Left (p), RegFrame (rt1, rt2)) = toRegionHyp (p, rt1)
    | toRegionAssertion (Right(p), RegFrame (rt1, rt2)) = toRegionProof (p, rt2)
  (* all others impossible *)
  and toRegionProof (Here, rt) = treeToRegionProof (rt)
    | toRegionProof (This (p), RegFinal (rt)) = toRegionAssertion (p, rt)
    | toRegionProof (This (p), RegStep (rt1, rt2)) = toRegionAssertion (p, rt1)
    | toRegionProof (Next (p), RegStep (rt1, rt2)) = toRegionProof (p, rt2)
  (* all others impossible *)


  datatype RegTerm =
      RegLeaf of Region.Region
    | RegUn of RegTerm * Region.Region
    | RegLam of Region.Region * RegTerm (* CAUTION: Region is NOT the whole region in this case, needs join!! *)
    | RegBin of RegTerm * RegTerm
    | RegTern of RegTerm * RegTerm * RegTerm * Region.Region   (* case, rec *)
    | RegTern' of RegTerm * RegTerm * RegTerm * Region.Region  (* if, NEEDS JOIN *)
    | RegAnn of RegTerm * RegProp


  datatype RegAnnHyp = RegAnnHyp of Region.Region * RegProp  (* x : A    *)
    (* CAUTION: Region is NOT the whole region in this case, needs join!! *)
  datatype RegAnnAssertion = 
      RegAnnLine of RegTerm * RegProp                 (* M : A    *) 
    | RegAnnFrame of RegAnnHyp * RegAnnProof          (* [x:A; P] *)
  and RegAnnProof =
      RegAnnFinal of RegAnnAssertion                  (* E        *)
    | RegAnnStep of RegAnnAssertion * RegAnnProof     (* E ; P    *)

  (*  convert region trees into regions *)
  fun treeToRegionTerm (RegLeaf (r)) = r
    | treeToRegionTerm (RegUn (rt, r)) =  r
    | treeToRegionTerm (RegLam (r, rt)) = Region.join (r, treeToRegionTerm (rt))
    | treeToRegionTerm (RegBin (rt1, rt2)) = Region.join (treeToRegionTerm (rt1), treeToRegionTerm (rt2))
    | treeToRegionTerm (RegTern (rt1, rt2, rt3, r)) = r
    | treeToRegionTerm (RegTern' (rt1, rt2, rt3, r)) = Region.join (r, treeToRegionTerm (rt3))
    | treeToRegionTerm (RegAnn (rt1, rt2)) = Region.join (treeToRegionTerm (rt1), treeToRegionProp (rt2))

  fun treeToRegionAnnHyp (RegAnnHyp (r, rt)) = Region.join (r, treeToRegionProp (rt))

  fun treeToRegionAnnAssertion (RegAnnLine (rt1, rt2)) = 
        Region.join (treeToRegionTerm (rt1), treeToRegionProp (rt2))
    | treeToRegionAnnAssertion (RegAnnFrame (rt1, rt2)) = 
        Region.join (treeToRegionAnnHyp (rt1), treeToRegionAnnProof (rt2))
  and treeToRegionAnnProof (RegAnnFinal (rt)) = treeToRegionAnnAssertion (rt)
    | treeToRegionAnnProof (RegAnnStep (rt1, rt2)) = 
        Region.join (treeToRegionAnnAssertion (rt1), treeToRegionAnnProof (rt2))

  fun toRegionTerm (Here, rt) = treeToRegionTerm (rt)

    | toRegionTerm (This (p), RegUn (rt, r)) = toRegionTerm (p, rt)

    | toRegionTerm (This (p), RegLam (r, rt)) = toRegionTerm (p, rt)

    | toRegionTerm (Left (p), RegBin (rt1, rt2)) = toRegionTerm (p, rt1)
    | toRegionTerm (Right (p), RegBin (rt1, rt2)) = toRegionTerm (p, rt2)

    | toRegionTerm (Left (p), RegTern (rt1, rt2, rt3, r)) = toRegionTerm (p, rt1)
    | toRegionTerm (Middle (p), RegTern (rt1, rt2, rt3, r)) = toRegionTerm (p, rt2)
    | toRegionTerm (Right (p), RegTern (rt1, rt2, rt3, r)) = toRegionTerm (p, rt3)
    | toRegionTerm (Left (p), RegTern' (rt1, rt2, rt3, r)) = toRegionTerm (p, rt1)
    | toRegionTerm (Middle (p), RegTern' (rt1, rt2, rt3, r)) = toRegionTerm (p, rt2)
    | toRegionTerm (Right (p), RegTern' (rt1, rt2, rt3, r)) = toRegionTerm (p, rt3)

    | toRegionTerm (Left (p), RegAnn (rt1, rt2)) = toRegionTerm (p, rt1)
    | toRegionTerm (Right(p), RegAnn (rt1, rt2)) = toRegionProp (p, rt2)


  fun toRegionAnnHyp (Here, rt) = treeToRegionAnnHyp (rt)
    | toRegionAnnHyp (This(p), RegAnnHyp (r, rt)) = toRegionProp (p, rt)
  (* all others impossible *)

  fun toRegionAnnAssertion (Here, rt) = treeToRegionAnnAssertion (rt)
    | toRegionAnnAssertion (Left (p), RegAnnLine (rt1, rt2)) = 
        toRegionTerm (p, rt1) 
    | toRegionAnnAssertion (Right (p), RegAnnLine (rt1, rt2)) = 
        toRegionProp (p, rt2) 
    | toRegionAnnAssertion (Left (p), RegAnnFrame (rt1, rt2)) = toRegionAnnHyp (p, rt1)
    | toRegionAnnAssertion (Right(p), RegAnnFrame (rt1, rt2)) = toRegionAnnProof (p, rt2)
  (* all others impossible *)
  and toRegionAnnProof (Here, rt) = treeToRegionAnnProof (rt)
    | toRegionAnnProof (This (p), RegAnnFinal (rt)) = toRegionAnnAssertion (p, rt)
    | toRegionAnnProof (This (p), RegAnnStep (rt1, rt2)) = toRegionAnnAssertion (p, rt1)
    | toRegionAnnProof (Next (p), RegAnnStep (rt1, rt2)) = toRegionAnnProof (p, rt2)
  (* all others impossible *)


end;  (* structure Paths *)
