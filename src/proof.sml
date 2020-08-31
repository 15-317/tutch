(* $Id: proof.sml,v 1.2 2002/10/24 19:25:48 abel Exp $ *)

(* Syntax for first-order/arithmetical proofs
 * used by syntax.sml ...
 *)

signature PROOF =
sig

  datatype Judgment =                   (* J ::=        *)
      HasType of Exp.Exp * Exp.Exp      (*       M : T  *)
    | IsTrue of Exp.Exp                 (*     | A true *)

  datatype Hyp =
      Var of string * Exp.Exp           (* H ::=  x:T     *)
    | Ass of Exp.Exp                    (*      | A       *)    

  datatype Hyps = 
      Last of Hyp	                (* Hs ::= H       *)
    | Ext of Hyp * Hyps                 (*      | H, Hs   *)

  datatype Assertion =			(* E ::=          *)
    Line of Judgment		        (*        J       *)
  | Frame of Hyps * Proof		(*      | [Hs; P] *)
  and Proof =    			(* P ::=          *)
    Final of Assertion			(*      | E       *)
  | Step of Assertion * Proof		(*      | E; P    *)


  val jEq : Val.env * Judgment * Judgment -> bool
(*  val jEq2 : int * Judgement * Val.Env * Judgement * Val.Env -> bool *)
  val hypJEq : Val.env * Hyps * Judgment * Hyps * Judgment -> bool

(*
  val hypsEq : Val.env * Hyps * Hyps -> bool
*)  

  val jToString : Judgment -> string
  
  val hypToString : Hyp -> string
  val hypsToString : Hyps -> string

  val hypToJ : Hyp -> Judgment

end (* signature PROOF *)


structure Proof :> PROOF =
struct

  datatype Judgment =                   (* J ::=        *)
      HasType of Exp.Exp * Exp.Exp      (*       M : T  *)
    | IsTrue of Exp.Exp                 (*     | A true *)

  datatype Hyp =
      Var of string * Exp.Exp           (* H ::=  x:T     *)
    | Ass of Exp.Exp                    (*      | A       *)    

  datatype Hyps = 
      Last of Hyp	                (* Hs ::= H       *)
    | Ext of Hyp * Hyps                 (*      | H, Hs   *)

  datatype Assertion =			(* E ::=          *)
    Line of Judgment		        (*        J       *)
  | Frame of Hyps * Proof		(*      | [Hs; P] *)
  and Proof =    			(* P ::=          *)
    Final of Assertion			(*      | E       *)
  | Step of Assertion * Proof		(*      | E; P    *)


  fun jEq (e, HasType (M, S), HasType (N, T)) = Val.eqExp' (e, M, N)
        andalso Val.eqExp' (e, S, T)
    | jEq (e, IsTrue (A), IsTrue (B)) = Val.eqExp' (e, A, B)
    | jEq (e, J, J') = false

  fun jEq2 (i, HasType (M, S), e, HasType (N, T), e') = 
        Val.conv (i, Val.Clos (M, e), Val.Clos (N, e'))
        andalso Val.conv (i, Val.Clos (S, e), Val.Clos (T, e'))
    | jEq2 (i, IsTrue (A), e, IsTrue (B), e') = 
        Val.conv (i, Val.Clos (A, e), Val.Clos (B, e'))
    | jEq2 (i, J, e, J', e') = false

  fun hypJEq ((i, e), Hs, A, Hs', A') =
      let 
	  fun eq (i, Last (Var (x, T)), e, 
		     Last (Var (x', T')), e') =
                Val.eqExp2 (i, T, e, T', e') andalso
                jEq2 (i+1, A, Cxt.Ext (x, Val.Gen (i), e), A', Cxt.Ext (x', Val.Gen (i), e'))
	    | eq (i, Last (Ass T), e, 
		     Last (Ass T'), e') =
                Val.eqExp2 (i, T, e, T', e') andalso
                jEq2 (i+1, A, e, A', e')
	    | eq (i, Ext (Var (x, T), Hs), e, 
		     Ext (Var (x', T'), Hs'), e') =
                Val.eqExp2 (i, T, e, T', e') andalso
                eq (i+1, Hs, Cxt.Ext (x, Val.Gen (i), e), Hs', Cxt.Ext (x', Val.Gen (i), e'))
	    | eq (i, Ext (Ass (T), Hs), e, 
		     Ext (Ass (T'), Hs'), e') =
                Val.eqExp2 (i, T, e, T', e') andalso
                eq (i, Hs, e, Hs', e')
	    | eq _ = false
      in
	  eq (i, Hs, e, Hs', e)
      end    

(*
  fun hypEq (e, Var (x, T), Var (x', T')) = x = x'
        andalso Val.eqExp' (e, T, T')
    | hypEq (e, Ass (A), Ass (A')) = Val.eqExp' (e, A, A')
    | hypEq (e, h, h' ) = false
  
  fun hypsEq (e, Last (H), Last (H')) = hypEq (e, H, H')
    | hypsEq (e, Ext (H, Hs), Ext (H', Hs')) = hypEq (e, H, H')
       andalso hypsEq (Hs, Hs')
    | hypsEq (e, h, h') = false
*)
  
  fun jToString (IsTrue (A)) = Exp.propToString (A)
    | jToString (HasType (M, T)) = Exp.termToString (M)^" : "^Exp.typeToString (T)

  fun hypToString (Var (x, T)) = x^": "^Exp.typeToString (T)
    | hypToString (Ass (A)) = Exp.propToString (A)

  fun hypsToString (Last (H)) = hypToString (H)
    | hypsToString (Ext (H, Hs)) = hypToString (H) ^", "^hypsToString (Hs)

  fun hypToJ (Var (x, T)) = HasType (Exp.Var (x), T)
    | hypToJ (Ass A) = IsTrue (A)

end (* structure Proof *)
