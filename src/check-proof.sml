(* $Id: check-proof.sml,v 1.8 2002/10/24 19:25:47 abel Exp $ *)

signature CHECKPROOF =
sig

(*
  datatype Result = 
      OK              (* proof is OK and complete *)
    | Invalid         (* proof has invalid step, eg ending with hyp. judg. *)
    | Unjustified     (* proof has unjustified lines *)
    | WrongGoal       (* proof does not match goal *)
*)

  val check : bool * Exp.Cxt * Syntax.Proof * Syntax.Exp -> Global.result
 (* Results:                                      *)      
 (* exitOK:           proof is OK and complete    *)
 (* exitProofInvalid: proof has invalid step, eg ending with hyp. judg. *)
 (* exitUnjustified:  proof has unjustified lines *)
 (* exitWrongGoal:    proof does not match goal   *)

end; (* signature CHECKPROOF *)

structure CheckProof :> CHECKPROOF =
struct

  datatype Judgment =                              (* J ::=        *)
      NonHyp of Proof.Judgment                     (*       |- A   *)
    | Hypothetical of Proof.Hyps * Proof.Judgment  (*     | Hs |- A *)

  datatype Judgments =                             (* Js ::=       *) 
      Empty                                        (*       .      *)
    | Ext of int * Judgment * Judgments            (*     | Js, J_i*)

  datatype Justification =
      Hyp of int
    | AndI of int * int
    | AndE1 of int 
    | AndE2 of int
    | OrI1 of int
    | OrI2 of int
    | OrE of int * int * int
    | ImpI of int
    | ImpE of int * int
    | TrueI 
    | FalseE of int
    | Class of int
    | AllI of int
    | AllE of int * int
    | ExI of int * int
    | ExE of int * int
    | NatE of int * int * int
    | ListE of int * int * int
    | EqNI0
    | EqNIS of int
    | EqNE0S of int
    | EqNES0 of int
    | EqNESS of int
    | LessNI0
    | LessNIS of int
    | LessNE0 of int
    | LessNES of int
    | EqLIN
    | EqLIC of int         (* xs = ys        |- x::xs = x::ys *)
    | EqLIC2 of int * int  (* x = y, xs = ys |- x::xs = y::ys *)
    | EqLENC of int
    | EqLECN of int
    | EqLECC of int        (* x::xs = y::ys  |- xs = ys *)
    | EqLECC2 of int       (* x::xs = y::ys  |- x = y *)
    | Lemma of string

  exception Error of string

  fun error (msg) = raise Error (msg)

  (* INFERENCE *)

  (*  lemma : Exp.Cxt * Exp.Exp -> Justification option  
  *)
  fun lemma (e, Cxt.Empty, B) = NONE
    | lemma (e, Cxt.Ext (x, A, Ls), B) = 
        if Val.eqExp' (e, A, B) then SOME (Lemma (x)) else lemma (e, Ls, B)

  (*  immed : Val.env * Judgments * Judgment -> int option
  *)
  fun immed (e, Empty, J) = NONE
    | immed (e, Ext (i, NonHyp (A), Js), J as NonHyp (B)) =
        if Proof.jEq (e, A, B) then SOME (i) else immed (e, Js, J)
    | immed (e, Ext (i, Hypothetical (As, G), Js),
	           J as Hypothetical (Bs, H)) =
	if Proof.hypJEq (e, As, G, Bs, H) then SOME (i) 
	else immed (e, Js, J)
    | immed (e, Ext (i, J', Js), J) = immed (e, Js, J)

  (*  hyp : Val.env * Judgments * Exp.Exp -> Justification option
  *)
  fun hyp (e, Js, A) = 
      case immed (e, Js, NonHyp (Proof.IsTrue (A))) of
	  NONE => NONE
	| SOME (i) => SOME (Hyp (i))

  (*  Val.eqSubst (e, C, M, x, A) = true  iff C == [M/x]A in env e *)
  (*  Val.eqSubstRec (e, C, M, x, A) = true  iff C(x) == [M(x)/x]A in env e *)
  (*  Val.eqSubstVar (e, C, y, x, A) = true  iff C(y) == [y/x]A in env e *)
   
  (*  intro : Val.env * Judgments * Exp.Exp -> Justification option
  *)
  fun intro (e, Js, Exp.Atom (P)) = NONE
    | intro (e, Js, Exp.Sigma (Range.Prop, A, B)) = 
       (case immed (e, Js, NonHyp (Proof.IsTrue A)) of
	    NONE => NONE
	  | SOME (i) => case immed (e, Js, NonHyp (Proof.IsTrue B)) of
		NONE => NONE
	      | SOME (j) => SOME (AndI (i, j))) 
    | intro (e, Js, Exp.Sum (A, B)) = 
       (case immed (e, Js, NonHyp (Proof.IsTrue A)) of
	    NONE => (case immed (e, Js, NonHyp (Proof.IsTrue B)) of
		NONE => NONE
	      | SOME (j) => SOME (OrI2 (j)))
	  | SOME (i) => SOME (OrI1 (i)))
    | intro (e, Js, Exp.Pi (Range.Prop, A, B)) = 
       (case immed (e, Js, Hypothetical (Proof.Last (Proof.Ass A), Proof.IsTrue (B))) of
	    NONE => NONE
	  | SOME (i) => SOME (ImpI (i)))
    | intro (e, Js, Exp.Unit) = SOME (TrueI)
    | intro (e, Js, Exp.Empty) = NONE
    | intro (e, Js, Exp.Pi (Range.Dep x, T, A)) = 
       (case immed (e, Js, Hypothetical (Proof.Last (Proof.Var (x, T)), Proof.IsTrue(A))) of
	    NONE => NONE
	  | SOME (i) => SOME (AllI (i)))
    | intro (e, Js, Exp.Sigma (Range.Dep x, T, A)) = 
        let 
	   fun exI (Empty) = NONE
	     | exI (Ext (i, NonHyp (Proof.HasType (M, S)), Js')) =
                 if Val.eqExp' (e, S, T) then 
		     let
			 fun exI' (Empty) = exI (Js')
			   | exI' (Ext (j, NonHyp (Proof.IsTrue (AM)), Js'')) =
			       if Val.eqSubst (e, AM, M, x, A) 
			       then SOME (ExI (i, j))
			       else exI' (Js'')
			   | exI' (Ext (j, J, Js'')) = exI' (Js'')
		     in 
			 exI' (Js) 
		     end
(*                  (case immed (e, Js, Exp.subst (M, x, A)) of
		       NONE => exI (Js')
		     | SOME (j) => SOME (ExI (i, j)))
*)		 else exI (Js')
	     | exI (Ext (i, J, Js')) = exI (Js')
        in 
	    exI (Js) 
        end
    | intro (e, Js, Exp.Not (A)) = intro (e, Js, Exp.notDef (A))
    | intro (e, Js, Exp.Equiv (A, B)) = intro (e, Js, Exp.equivDef (A, B))
    | intro ((n, e), Js, Exp.Equal (M, N)) = 
	let
	  fun eqProven (v, w) =
	    let 
	      fun eq (Empty) = NONE 
		| eq (Ext (i, NonHyp (Proof.IsTrue (
                         Exp.Equal (M', N'))), Js')) =
		    if Val.conv (n, Val.whnf (M', e), v) andalso 
                       Val.conv (n, Val.whnf (N', e), w) 
		    then SOME (i)
		    else eq (Js')
		| eq (Ext (i, J, Js')) = eq (Js')		  
	    in
	      eq (Js)
	    end
          val v = Val.whnf (M, e)
	  val w = Val.whnf (N, e)
	in
	  case (v, w) of
	      (Val.Zero, Val.Zero) => SOME (EqNI0)
	    | (Val.Succ (v'), Val.Succ (w')) =>
		(case eqProven (v', w') of
		     NONE => NONE
		   | SOME (i) => SOME (EqNIS (i)))
(* abel, 21.11.2001 *)
(*	        let 
		  fun eqS (Empty) = NONE 
		    | eqS (Ext (i, NonHyp (Proof.IsTrue (
                         Exp.Equal (M', N'))), Js')) =
		       if Val.conv (n, Val.whnf (M', e), v') andalso 
                          Val.conv (n, Val.whnf (N', e), w') 
		       then SOME (EqNIS (i))
		       else eqS (Js')
		    | eqS (Ext (i, J, Js')) = eqS (Js')
		in
		  eqS (Js)
                end
*)
	    | (Val.Nil, Val.Nil) => SOME (EqLIN)
	    | (Val.Cons (v1, v2), Val.Cons (w1, w2)) =>
		(case eqProven (v2, w2) of
		     NONE => NONE
		   | SOME (j) => 
		       if Val.conv (n, v1, w1) then SOME (EqLIC (j))
		       else (case eqProven (v1, w1) of
			   NONE => NONE
                         | SOME (i) => SOME (EqLIC2 (i, j))))	   
(*
              if Val.conv (n, v1, w1) then 
	        let 
		  fun eqC (Empty) = NONE 
		    | eqC (Ext (i, NonHyp (Proof.IsTrue (
                         Exp.Equal (M', N'))), Js')) =
		       if Val.conv (n, Val.whnf (M', e), v2) andalso 
                          Val.conv (n, Val.whnf (N', e), w2) 
		       then SOME (EqLIC (i))
		       else eqC (Js')
		    | eqC (Ext (i, J, Js')) = eqC (Js')
		in
		  eqC (Js)
                end
              else NONE 
*)
	    | (v', w') => NONE
	end
    | intro ((n, e), Js, Exp.Less (M, N)) =
       (case (Val.whnf (M, e), Val.whnf (N, e)) of
	    (Val.Zero, Val.Succ (w)) => SOME (LessNI0)
	  | (Val.Succ(v), Val.Succ (w)) =>  
	    let 
		fun lessS (Empty) = NONE 
		  | lessS (Ext (i, NonHyp (Proof.IsTrue (
                         Exp.Less (M', N'))), Js')) =
		       if Val.conv (n, Val.whnf (M', e), v) andalso 
                          Val.conv (n, Val.whnf (N', e), w)
		       then SOME (LessNIS (i))
		       else lessS (Js')
		  | lessS (Ext (i, J, Js')) = lessS (Js')
	    in
		lessS (Js)
	    end
          | (v, w) => NONE)
    | intro (e, Js, Exp.App (P, M)) = NONE

  (*  elim : bool * Val.env * Judgments * Exp.Exp -> Justification option
  *)
  fun elim (class, e as (n, e'), Js, C) =
      let
	fun elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Atom (P))), Js')) = elim' (Js')
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Sigma (Range.Prop, A, B))), Js')) =
	    if Val.eqExp' (e, A, C) then SOME (AndE1 (i))
	    else if Val.eqExp' (e, B, C) then SOME (AndE2 (i)) else elim' (Js')
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Sum (A, B))), Js')) = 
	     (case immed (e, Js, Hypothetical (Proof.Last (Proof.Ass A), 
                                             Proof.IsTrue (C))) of
		  NONE => elim' (Js')
		| SOME (j) => (case immed (e, Js, Hypothetical (Proof.Last (Proof.Ass B), Proof.IsTrue (C))) of
		      NONE => elim' (Js')
		    | SOME (k) => SOME (OrE (i, j, k))))
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Pi (Range.Prop, A, B))), Js')) =
	    if Val.eqExp' (e, B, C) then 
		(case immed (e, Js, NonHyp (Proof.IsTrue (A))) of
		    NONE => elim' (Js')
		  | SOME (j) => SOME (ImpE (i, j)))
	    else elim' (Js')
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Unit)), Js')) = elim' (Js')
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Empty)), Js')) = SOME (FalseE (i))
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Pi (Range.Dep x, T, A))), Js')) =
            let 
		fun allE (Empty) = elim' (Js')
		  | allE (Ext (j, NonHyp (Proof.HasType (M, S)), Js'')) =
                      if Val.eqExp' (e, S, T) andalso
(*                         Val.eqExp' (e, subst (M, x, A), C) *)
			  Val.eqSubst (e, C, M, x, A)
                      then SOME (AllE (i, j))
		      else allE (Js'')
		  | allE (Ext (j, J, Js'')) = allE (Js'')
	    in
		allE (Js)
	    end
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Sigma (Range.Dep x, T, A))), Js')) =
	    let 
		fun exE (Empty) = elim' (Js')
		  | exE (Ext (j, Hypothetical (Proof.Ext (Proof.Var (y, S), 
			                      Proof.Last (Proof.Ass Ay)), 
                                 Proof.IsTrue (C')), Js'')) =
                      if Val.eqExp' (e, C, C') andalso
                         Val.eqExp' (e, S, T) andalso
(*                         Val.eqExp' (Val.gen (y, e), Ay, subst (Exp.Var (y), x, A)) *)
			 Val.eqSubstVar (e, Ay, y, x, A)
                      then SOME (ExE (i, j))
		      else exE (Js'')
		  | exE (Ext (j, J, Js'')) = exE (Js'')
	    in 
	        exE (Js)
            end
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Not (A))), Js')) = 
	      elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.notDef (A))), Js'))
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Equiv (A, B))), Js')) =
	      elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.equivDef (A, B))), Js'))

	  | elim' (Ext (i, NonHyp (Proof.IsTrue (
                     Exp.Equal (M, N))), Js')) = 
	     (case (Val.whnf (M, e'), Val.whnf (N, e')) of
		  (Val.Zero, Val.Zero) => elim' (Js')
		| (Val.Zero, Val.Succ (w)) => SOME (EqNE0S (i))
		| (Val.Succ (v), Val.Zero) => SOME (EqNES0 (i))
		| (Val.Succ (v), Val.Succ (w)) =>
		     (case C of
			  Exp.Equal (M', N') =>
			      if Val.conv (n, Val.whnf (M', e'), v) andalso
				 Val.conv (n, Val.whnf (N', e'), w)
                              then SOME (EqNESS (i))
			      else elim' (Js')
			| _ => elim' (Js'))
		| (Val.Nil, Val.Nil) => elim' (Js')
		| (Val.Nil, Val.Cons (w1, w2)) => SOME (EqLENC (i))
		| (Val.Cons (v1, v2), Val.Nil) => SOME (EqLECN (i))
		| (Val.Cons (v1, v2), Val.Cons (w1, w2)) =>   
                     (* (eqLEcc)  v1::v2 = w1::w2 |- v2 = w2 
		        is an incomplete elimination rule, 
		        therefore I added the rule
		        (eqLEcc2) v1::v2 = w1::w2 |- v1 = w1 [21.11.2001, abel]
		      *)
		     (case C of
			  Exp.Equal (M', N') =>
			      let val v = Val.whnf (M', e')
				  val w = Val.whnf (N', e')
			      in
				  if (Val.conv (n, v, v2) andalso
				      Val.conv (n, w, w2))
				  then SOME (EqLECC (i))
 			          else if
				     (Val.conv (n, v, v1) andalso
				      Val.conv (n, w, w1))
				  then SOME (EqLECC2 (i))
				  else elim' (Js')
			      end
			| _ => elim' (Js'))
		| (v, w) => elim' (Js')) 
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (
                     Exp.Less (M, N))), Js')) = 
	     (case (Val.whnf (M, e'), Val.whnf (N, e')) of
		  (v, Val.Zero) => SOME (LessNE0 (i))
		| (Val.Zero, Val.Succ (w)) => elim' (Js')
		| (Val.Succ (v), Val.Succ (w)) =>
		     (case C of
			  Exp.Less (M', N') =>
			      if Val.conv (n, Val.whnf (M', e'), v) andalso
				 Val.conv (n, Val.whnf (N', e'), w)
                              then SOME (LessNES (i))
			      else elim' (Js')
			| _ => elim' (Js'))
		| (v, w) => elim' (Js')) 
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.App (P, M))), Js')) = elim' (Js')
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Pi (Range.Type, _, _))), Js')) =

	      elim' (Js')
	  | elim' (Ext (i, NonHyp (Proof.IsTrue (Exp.Sigma (Range.Type, _, _))), Js')) =
	      elim' (Js')

	  | elim' (Ext (i, NonHyp (Proof.HasType _), Js')) = elim' (Js')

	  | elim' (Ext (i, Hypothetical (Proof.Ext (Proof.Var (x, Exp.Nat),
                                         Proof.Last (Proof.Ass (Ax))),  
	  				             Proof.IsTrue (Asx)),
			Js')) = 
(*             if Val.eqExp' (Val.gen (x, e), 
			     Asx, 
                             subst (Exp.Succ (Exp.Var (x)), x, Ax)
*)
              if Val.eqSubstRec (e, Asx, Exp.Succ (Exp.Var (x)), x, Ax) then
		  let
		      fun natE (Empty) = elim' (Js')
			| natE (Ext (j, NonHyp (Proof.IsTrue A0), Js'')) =
			  if Val.eqSubst (e, A0, Exp.Zero, x, Ax) then
			    let
				fun natE' (Empty) = elim' (Js'')
				  | natE' (Ext (k, NonHyp (Proof.HasType (M, Exp.Nat)), Js''')) =
(*				      if Val.eqExp' (e, C, subst (M, x, Ax))
*)
				      if Val.eqSubst (e, C, M, x, Ax)
				      then SOME (NatE (k, j, i))
				      else natE' (Js''')
				  | natE' (Ext (k, J, Js''')) = natE' (Js''')
			    in
				natE' (Js)
			    end
			  else natE (Js'')
			| natE (Ext (j, J, Js'')) = natE (Js'')
		  in
		      natE (Js)
		  end
	      else elim' (Js')
	  | elim' (Ext (i, Hypothetical (Proof.Ext (Proof.Var (x, S),
                                         Proof.Ext (Proof.Var (xs, Exp.List T),
                                         Proof.Last (Proof.Ass (Axs)))),  
	  				             Proof.IsTrue (Axxs)),
			Js')) = 

              if Val.eqExp' (e, S, T) andalso
                 Val.eqSubstRec (Val.gen (x, e), Axxs, Exp.Cons (Exp.Var (x), Exp.Var (xs)), xs, Axs) then
		  let
		      fun listE (Empty) = elim' (Js')
			| listE (Ext (j, NonHyp (Proof.IsTrue Anil), Js'')) =
			  if Val.eqSubst (e, Anil, Exp.Nil, xs, Axs) then
			    let
				fun listE' (Empty) = elim' (Js'')
				  | listE' (Ext (k, NonHyp (Proof.HasType (M, Exp.List (T'))), Js''')) =

				      if Val.eqExp' (e, T, T') andalso
					 Val.eqSubst (e, C, M, xs, Axs)
				      then SOME (ListE (k, j, i))
				      else listE' (Js''')
				  | listE' (Ext (k, J, Js''')) = listE' (Js''')
			    in
				listE' (Js)
			    end
			  else listE (Js'')
			| listE (Ext (j, J, Js'')) = listE (Js'')
		  in
		      listE (Js)
		  end
	      else elim' (Js')

	    (* Classical proof C -> F |- F ==> C *)  
	  | elim' (Ext (i, Hypothetical (
                Proof.Last (Proof.Ass (notC)), 
                Proof.IsTrue (Exp.Empty)), 
              Js')) = 
	    if class andalso Val.eqExp' (e, Exp.Not (C), notC) 
		then SOME (Class (i)) 
            else elim' (Js')

	  | elim' (Ext (i, Hypothetical _, Js')) = elim' (Js')
	  | elim' (Empty) = NONE
      in
	elim' (Js)
      end

  (*  infer: bool * Exp.Cxt * Val.env * Judgments * Exp.Exp -> Justification option
  *)
  fun infer (class, Ls, e, Js, A) =
      (case hyp (e, Js, A) of
          SOME (just) => SOME (just)
	| NONE => case intro (e, Js, A) of
	      SOME (just) => SOME (just)
	    | NONE => case elim (class, e, Js, A) of
		  SOME (just) => SOME (just)
		| NONE => lemma (e, Ls, A))


  fun extEnv (Proof.Last (Proof.Var (x, T)), e) = Val.gen (x, e)
    | extEnv (Proof.Last (Proof.Ass (A)), e) = e
    | extEnv (Proof.Ext (Proof.Var (x, T), Hs), e) = extEnv (Hs, Val.gen (x, e))
    | extEnv (Proof.Ext (Proof.Ass (A), Hs), e) = extEnv (Hs, e)

  fun extCxt (Proof.Last (Proof.Var (x, T)), G) = Cxt.Ext (x, T, G)
    | extCxt (Proof.Last (Proof.Ass (A)), G) = G
    | extCxt (Proof.Ext (Proof.Var (x, T), Hs), G) = extCxt (Hs, Cxt.Ext (x, T, G))
    | extCxt (Proof.Ext (Proof.Ass (A), Hs), G) = extCxt (Hs, G)

  (*  PRINTING *)

  fun JsToString (Ext (i, NonHyp (J), Empty)) = Proof.jToString J
    | JsToString (Ext (i, Hypothetical (Hs, J), Empty)) = "(" ^ Proof.hypsToString Hs ^ " |- " ^ Proof.jToString J ^ ")"
    | JsToString (Ext (i, NonHyp (J), Js)) = JsToString (Js) ^ "; " ^ Proof.jToString J
    | JsToString (Ext (i, Hypothetical (Hs, J), Js)) = JsToString (Js) ^ "; " ^ "(" ^ Proof.hypsToString Hs ^ " |- " ^ Proof.jToString J ^ ")"
    | JsToString (Empty) = "."


  fun justToString' (Hyp (i)) = "Hyp " ^ Int.toString (i)
    | justToString' (AndI (i,j)) = "AndI " ^ Int.toString (i) ^ " " ^ Int.toString (j)
    | justToString' (AndE1 (i)) = "AndEL " ^ Int.toString (i)
    | justToString' (AndE2 (i)) = "AndER " ^ Int.toString (i)
    | justToString' (OrI1 (i)) = "OrIL " ^ Int.toString (i)
    | justToString' (OrI2 (i)) = "OrIR " ^ Int.toString (i)
    | justToString' (OrE (i,j,k)) = "OrE " ^ Int.toString (i) ^ " " ^ Int.toString (j) ^ " " ^ Int.toString (k) 
    | justToString' (ImpI (i)) = "ImpI " ^ Int.toString (i)
    | justToString' (ImpE (i,j)) = "ImpE " ^ Int.toString (i) ^ " " ^ Int.toString (j)
    | justToString' (TrueI) = "TrueI"
    | justToString' (FalseE (i)) = "FalseE " ^ Int.toString (i)
    | justToString' (Class (i)) = "Class " ^ Int.toString (i)

    | justToString' (AllI (i)) = "ForallI " ^ Int.toString (i)
    | justToString' (AllE (i, j)) = "ForallE " ^ Int.toString (i) ^ " " ^Int.toString (j)
    | justToString' (ExI (i, j)) = "ExistsI " ^ Int.toString (i) ^ " " ^ Int.toString (j)
    | justToString' (ExE (i, j)) = "ExistsE " ^ Int.toString (i) ^ " " ^ Int.toString (j)
    | justToString' (NatE (i, j, k)) = "NatE " ^ Int.toString (i) ^ " " ^ Int.toString (j) ^ " " ^ Int.toString (k)
    | justToString' (ListE (i, j, k)) = "ListE " ^ Int.toString (i) ^ " " ^ Int.toString (j) ^ " " ^ Int.toString (k)
    | justToString' (EqNI0) = "=NI0"
    | justToString' (EqNIS (i)) = "=NIs " ^ Int.toString (i)
    | justToString' (EqNE0S (i)) = "=NE0s " ^ Int.toString (i)
    | justToString' (EqNES0 (i)) = "=NEs0 " ^ Int.toString (i)
    | justToString' (EqNESS (i)) = "=NEss " ^ Int.toString (i)
    | justToString' (LessNI0) = "<NI0"
    | justToString' (LessNIS (i)) = "<NIs " ^ Int.toString (i)
    | justToString' (LessNE0 (i)) = "<NE0 " ^ Int.toString (i)
    | justToString' (LessNES (i)) = "<NEs " ^ Int.toString (i)
    | justToString' (EqLIN) = "=LIn"
    | justToString' (EqLIC (i)) = "=LIc " ^ Int.toString (i)
    | justToString' (EqLIC2 (i, j)) = "=LIc2 " ^ Int.toString (i) ^ " " ^ Int.toString (j)
    | justToString' (EqLENC (i)) = "=LEnc " ^ Int.toString (i)
    | justToString' (EqLECN (i)) = "=LEcn " ^ Int.toString (i)
    | justToString' (EqLECC (i)) = "=LEcc " ^ Int.toString (i)
    | justToString' (EqLECC2 (i)) = "=LEcc2 " ^ Int.toString (i)
    | justToString' (Lemma (x)) = "Lemma " ^ x
  fun justToString (just) = "by " ^ justToString' (just)


  val width = ref 50

(*
  fun printLine (no, nest, line, justification) =  print (
	"%{" ^ StringCvt.padLeft #" " 4 (Int.toString(no)) ^ " }% " ^
        StringCvt.padRight #" " (nest*2) "" ^
	StringCvt.padRight #" " (50 - nest*2) line ^
        (if justification= "" then "" else "% " ^ justification) ^ "\n")
*)
  fun printLine (no, nest, line, justification) =  print (
	StringCvt.padLeft #" " 3 (Int.toString(no)) ^ "  " ^
        StringCvt.padRight #" " (nest*2) "" ^
	StringCvt.padRight #" " (!width - nest*2) line ^ "  " ^
        justification ^ "\n")

  local
    val lineNo = ref 0          (* State: Line Number = Judgment Number *)
    fun nextNo() = !lineNo + 1
  in
    fun empty ()    = (lineNo := 0; Empty)
    fun ext (J, Js) = (lineNo := !lineNo+1; Ext (!lineNo, J, Js))
    fun extHyps (Hs, Js) = 
        let fun eh (Proof.Last (H), Js) = 
                  Ext (!lineNo, NonHyp (Proof.hypToJ (H)), Js)
	      | eh (Proof.Ext (H, Hs), Js) = 
                  eh (Hs, Ext (!lineNo, NonHyp (Proof.hypToJ (H)), Js))
	in (lineNo := !lineNo+1; eh (Hs, Js)) end

    (* val lineNo = (fn() => !lineNo+1) *)
    fun printLine' (nest, line, justification) =  
          printLine (nextNo(), nest, line, justification)
  end
  
  (*  printFirst : print the first line of a frame (hypothesis) *)
  fun printFirst (nest, Hs) = printLine' (nest, "[ " ^ Proof.hypsToString (Hs) ^ ";", "") 

  (*  printStep : print a line with justification
           last indicated whether it is the last line, which is closed with
           a ] instead of a ;
  *)
  fun printStep (nest, A, just, last) = printLine' (nest, 
        Exp.propPretty (A) ^ (if last then (if nest>0 then " ];" else "") else ";"), 
        justToString (just))

  fun printObj (nest, M, T, last) = printLine' (nest, 
        Exp.termPretty (M) ^ " : " ^ Exp.typePretty (T) ^ 
          (if last then (if nest>0 then " ];" else "") else ";"), 
        "")


  (*  printUnjust : print unjustified line *)
  fun printUnjust (nest, A, last) = printLine' (nest, 
        Exp.propPretty (A) ^ (if last then (if nest>0 then " ];" else "") else ";"), 
        "UNJUSTIFIED")


  (* PROVING LOOP *)   

  fun check (class, GlobG, Pp, Gg) = 
  (* invariants? -fp *)
  let val rt = Syntax.getRegTreeProof (Pp)
      val valid = ref true       (* proof valid? *)

      (* the following functions are local to check for two reasons:
         - to alter the flag "valid"
         - to access the region tree of the proof for error messages via "msg"
      *)
      fun msg (path, s) = Region.wrap (Path.toRegion (Path.rev (path), rt), s)
      fun regTree (path) = Path.toRegTree (Path.rev (path), rt)

      (*  step : int * (Exp.Cxt * Val.env) * 
                 Judgments * Assertion * Path.RevPath * bool 
              -> Judgment *)
      (*  step (nest, Js, E as Proof.Line(A), path, last) = NonHyp(A)
       *  step (nest, Js, E as Proof.Frame _, path, last) = Hypothetical _
       *    
       *  Parameters:
       *  - nest: nesting level (current depth in proof)
       *  - Js:   available judgments for this inference step
       *  - E:    Proof entry (Line or Frame) (current goal)
       *  - path: path to proof entry in Proof structure
       *  - last: flag indicating wether this is the final proof step
       *          (used only for printing)
       *  Result:
       *  a judgment representing the proven step---for addition to Js
       *
       *  Side effects: 
       *  - prints proof lines with justifications
       *  - if step is unjustified, the flag valid is set to false
       *)
      fun step (nest, (G, e), Js, Proof.Line (Proof.HasType (M, T)), path, last) = 
           ((CheckExp.check(G, Syntax.mkTerm (M, regTree (Path.left (path))),
			      Syntax.mkType (T, regTree (Path.right(path))));
            if Chatter.declDetails() then printObj (nest, M, T, last) else ();
	    NonHyp (Proof.HasType (M, T)))
	    handle CheckExp.Error (s) => raise Error s)
(*	    handle CheckExp.Error (s) => raise Error (msg (path, s ^ 
		    (if Chatter.errorDetails() then "\nIn line: " ^
                     Exp.propToString (A) else ""))))
*)
        | step (nest, (G, e), Js, Proof.Line (Proof.IsTrue (A)), path, last) = 
            ((case infer (class, GlobG, e, Js, A) of
                SOME just => (
		  if Chatter.declDetails() then printStep (nest, A, just, last) else ();
	          NonHyp (Proof.IsTrue (A)))
              | NONE => (
		  print (msg (path, 
		    "Unjustified line " ^ 
                    (if Chatter.errorDetails() then JsToString Js ^ "  |-  " ^ 
                      Exp.propToString (A) else "")
                    ^  "\n" ^ 
                    (if Chatter.actions() then 
                      "Assuming this line, checking remainder...\n" else "")));
	          valid := false; 
                  if Chatter.declDetails() then printUnjust (nest, A, last) else (); 
                  NonHyp (Proof.IsTrue (A))))
	    handle Val.Error (s) => raise Error (msg (path, s ^ 
		    (if Chatter.errorDetails() then "\nIn line: " ^
                     Exp.propToString (A) else ""))))
(*
		(print (msg (path, s ^ "\n" ^ 
                    (if Chatter.actions() then 
                      "Assuming this line, checking remainder...\n" else "")));
	          valid := false; 
                  if Chatter.declDetails() then printUnjust (nest, A, last) else (); 
                  NonHyp (Proof.IsTrue (A))))
*)
	| step (nest, (G, e), Js, Proof.Frame (Hs, P), path, last) = 
            (if Chatter.declDetails() then printFirst (nest, Hs) else ();
	    case check' (nest+1, (extCxt (Hs, G), extEnv (Hs, e)), 
                         extHyps (Hs, Js), P, Path.right (path)) of
	        NonHyp (B) => Hypothetical (Hs, B)
	      | Hypothetical _ => error (msg (path, "Hypothetical judgment at the end of a proof"))) 

      (*  check' : int * Val.env * Judgments * Proof * path -> Judgment *)
      (*  check' (nest, Js, P, path) = J
       *
       *  Parameters: 
       *  - nest, Js, path: same as for step
       *  - P: current proof 
       *
       *  Result: 
       *  a judgment representing the last line of P
       *
       *  check proves each step of P calling "step", accumulating the 
       *  resulting judgements in Js to make them available for proving
       *  the next steps.
       *)
      and check' (nest, (G, e), Js, Proof.Final (E), path)   = 
            step (nest, (G, e), Js, E, path, true)
	| check' (nest, (G, e), Js, Proof.Step (E, P), path) = 
            check' (nest, (G, e), ext (step (nest, (G, e), Js, E, Path.left (path), false), 
                               Js), 
                    P, Path.right (path))

      (*  check: runs check' and compares the result to the goal *) 

      val P = Syntax.getProof (Pp)
      val Goal = Syntax.getProp (Gg)

      fun returnOK () = 
	   (if Chatter.finalResults() then print "QED\n" else(); Global.exitOK)
      fun returnUnjustified () =
	   (if Chatter.finalResults() then print "Proof incomplete\n" else (); 
	    Global.exitUnjustified)   
      fun returnWrongGoal () =
	   (if Chatter.finalResults() then print (
                "Proof valid, but does not match the goal "
	        ^ Exp.propPretty Goal^ "\n") else ();
	    Global.exitWrongGoal)
      fun returnWrongGoalUnjust () =
           (if Chatter.finalResults() then print (
                "Proof incomplete and does not match the goal "
		^ Exp.propPretty Goal ^ "\n") else ();
	    Global.exitUnjustified)
      fun returnInvalid () =
	   (print (msg (Path.top, "Hypothetical judgment at the end of a proof\n"));
	    Global.exitProofInvalid)
      fun returnInvalid' () =
	   (print (msg (Path.top, "Judgment of form M : T at the end of the proof\n"));
	    Global.exitProofInvalid)

  in
      (* Heuristics to calculate the width of the longest line in the proof.
         We guess that we need one indentation (a 2 characters)
         per 8 characters of the goal formula.
         Thus we add 25% percent. For esthetic reasons (when you check several
         proofs after another), we make it a multiple of 8.
         Also the minimum should be 16.
       *)
      width := Real.ceil (Real.fromInt (String.size (Exp.propPretty (Goal))) 
                          * 1.25 / 8.0) * 8;
      if !width < 16 then width := 16 else ();
      case check' (0, (GlobG, Val.empty()), empty(), P, Path.top) of
	  NonHyp (Proof.IsTrue(A)) => 
	    if Val.eqExp' (Val.empty(), A, Goal) then
		if !valid then returnOK ()
		else returnUnjustified ()
	    else if !valid then returnWrongGoal ()
	    else returnWrongGoalUnjust ()
	| NonHyp (Proof.HasType _) => returnInvalid' () 
	| Hypothetical _ => returnInvalid ()
  end handle Error (msg) => (print (msg ^ "\n"); Global.exitProofInvalid)


end; (* structure CheckProof *)
