(* $Id: decl.sml,v 1.5 2002/10/24 19:25:48 abel Exp $ *)

signature DECL =
sig

(* prefer just boolean
  datatype ProofKind =
      Intuitionistic
    | Classical
 *)

  datatype Decl =			           (* D :: = *)
      Proof of bool *           (* true=classical / false=intuitionistic *)
               string * Syntax.Exp * Syntax.Proof  (* proof name : A = P *)
    | AnnProof of string * ExtSyn.prop * ExtSyn.annProof
      (* annotated proof name : A = begin ... end *)
    | Exp of string * Syntax.Exp * Syntax.Exp      (* val name : S = t *)
    | Term of string * Syntax.Exp * Syntax.Exp     (* term name : A = M *)

  (* add eventually: definitions, ... *)

  val check : Decl -> Global.result

  (* add eventually: environment, ... *)

end; (* signature DECL *)

structure Decl :> DECL =
struct

  datatype Decl =			           (* D :: = *)
      Proof of bool *           (* true=classical / false=intuitionistic *)
               string * Syntax.Exp * Syntax.Proof  (* proof name : A = P *)
    | AnnProof of string * ExtSyn.prop * ExtSyn.annProof
      (* annotated proof name : A = begin ... end *)
    | Exp of string * Syntax.Exp * Syntax.Exp      (* val name : S = t *)
    | Term of string * Syntax.Exp * Syntax.Exp     (* term name : A = M *)
   
  val  globG = ref (Cxt.Empty : Exp.Cxt)
  val  uvalG = ref (Cxt.Empty : UVal.Cxt) 
(*  val  globE = ref (Val.empty : Val.env) moved to Val *)
(*  val  globN = ref Names.empty *)

  (* add eventually: definitions, ... *)

  fun check (Proof (class, x, a, p)) = 
      let val A = Syntax.getProp(a)
          val _ = if Chatter.declDetails() then print ("\n") else ()
	  val _ = if Chatter.finalResults() then print ("Proving " ^ x ^ ": "^ Exp.propPretty (A) 
            ^ " ..." ^(if class then " (classically)" else "")^ "\n") else (); 
          val result = CheckProof.check (class, !globG, p, a)
	  val _ = if result = Global.exitOK 
                  then (globG := Cxt.Ext (x, A, !globG);
                        SpecCheck.tally (Spec.Proof (x, A)))
		  else false
      in result
      end
(* in Global.exitOK end *)
    | check (AnnProof (x, a, p)) = 
      let val A = ExtSyn.getProp(a)
          val _ = if Chatter.declDetails() then print ("\n") else ()
	  val _ = if Chatter.finalResults() then print ("Proving " ^ x ^ ": "^ Prop.prettyString (A) 
            ^ " annotated...\n") else (); 
          val result = CheckAnnProof.check (p, a)
	  val _ = if result = Global.exitOK then SpecCheck.tally (Spec.AnnProof (x, A))
		  else false
      in result
      end
    | check (Exp (x, s, m)) = 
      let val S = Syntax.getType (s)
          val M = Syntax.getTerm (m)
          val _ = if Chatter.declDetails() then print ("\n") else ()
	  val _ = if Chatter.finalResults() then print ("Checking value " ^ x ^ ": "^ Exp.typePretty (S) 
            ^ "\n") else (); 
          val result = CheckExp.checkExp (!globG, m, s)
	  val _ = if result = Global.exitOK then 
                   (globG := Cxt.Ext (x, S, !globG);
                    uvalG := Cxt.Ext (x, (Val.eval (S, Cxt.Empty), Univ.Type), 
                                          !uvalG); 
                    Val.extGlob (x, M);
(*                        globN := Names.add (i, x, !globN); 
			globE := (i, Cxt.Ext (x, Val.eval (M, e), e)) (* Def!! *) *)
(*
                    let 
			val (i, e) = !globE
		    in
                        uvalG := Cxt.Ext (x, (Val.eval (S, e), Univ.Type), 
                                          !uvalG); 

		    end;
*)
                    SpecCheck.tally (Spec.Exp (x, S)))
		  else false
      in result
      end
    | check (Term (x, s, m)) = 
      let val S = Syntax.getProp (s)
          val M = Syntax.getTerm (m)
          val _ = if Chatter.declDetails() then print ("\n") else ()
	  val _ = if Chatter.finalResults() then print ("Checking term " ^ x ^ ": "^ Exp.propPretty (S) 
            ^ "\n") else (); 
          val result = CheckDep.checkExp (!uvalG, m, s)
	  val _ = if result = Global.exitOK then 
                   (globG := Cxt.Ext (x, S, !globG);
                    uvalG := Cxt.Ext (x, (Val.eval (S, Cxt.Empty), Univ.Prop), 
                                          !uvalG); 
                    Val.extGlob (x, M);
                    SpecCheck.tally (Spec.Term (x, S)))
		  else false
      in result
      end

  (* add eventually: environment, ... *)

end; (* structure Decl *)
