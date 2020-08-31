(* $Id: check-ann-proof.sml,v 1.1 2000/10/17 22:23:07 abel Exp $ *)

signature CHECKANNPROOF = 
sig

  val check : ExtSyn.annProof * ExtSyn.prop -> Global.result


end (* signature CHECKANNPROOF *)


structure CheckAnnProof :> CHECKANNPROOF =
struct

  datatype Judgment =                         (* J ::=        *)
      NonHyp of Term.Term * Prop.Prop                     (*       |- A   *)
    | Hypothetical of AnnProof.Hyp * Term.Term * Prop.Prop   (*     | H |- A *)

  datatype Judgments =                        (* Js ::=       *) 
      Empty                                   (*       .      *)
    | Ext of Judgment * Judgments       (*     | Js, J_i*)

  type hypotheses = Prop.Prop Cxt.Cxt  


  exception Error of string

  fun error (msg) = raise Error (msg)

  (*  immed: Judgments * Judgment -> bool *)
  
  fun immed (Empty, J) = false
    | immed (Ext (NonHyp (M, A), Js), NonHyp (N, B)) = 
        if Prop.eq (A, B) andalso Term.eq (M, N) then true
	else immed (Js, NonHyp (N, B))
    | immed (Ext (Hypothetical (AnnProof.Hyp (x, A), M, C), Js),  
            J as (Hypothetical (AnnProof.Hyp (y, B), N, D))) =
        if Prop.eq (C, D) andalso Prop.eq (A, B) andalso x=y andalso Term.eq (M, N) then true
	else immed (Js, J)
    | immed (Ext (J', Js), J) = immed (Js, J)


  fun JToString (M, A) = Term.toString M ^ " : " ^ Prop.toString A

  fun JsToString (Ext (NonHyp (M, A), Empty)) = JToString (M, A)
    | JsToString (Ext (Hypothetical (AnnProof.Hyp (x, H), M, A), Empty)) = "(" ^ x ^": "^ Prop.toString H ^ " |- " ^ JToString (M, A) ^ ")"
    | JsToString (Ext (NonHyp (M, A), Js)) = JsToString (Js) ^ "; " ^ JToString (M, A)
    | JsToString (Ext (Hypothetical (AnnProof.Hyp (x, H), M, A), Js)) = JsToString (Js) ^ "; " ^ "(" ^ x ^": "^ Prop.toString H ^ " |- " ^ JToString (M, A) ^ ")"
    | JsToString (Empty) = "."


  datatype InferResult =
      Inf 
    | NotInf of string

  fun wrongProofTerm (M, A) = NotInf ("Proof term does not match proposition: " ^ JToString (M, A))

  (*  infer : Judgments * hypotheses * Term.Term * Prop.Prop -> InferResult
  *)

(* hypothesis rule *)

  fun infer (Js, G, J as (Term.Var (x), A)) = 
       (case Cxt.lookup (x, G) of 
	    NONE => NotInf ("Variable "^x^" not bound to a hypothesis")
	  | SOME B => if Prop.eq (A, B) then Inf
		      else NotInf ("Variable "^x^" labels proposition " ^ Prop.prettyString B ^", but used to proof proposition "^ Prop.prettyString A))

(* introduction rules *)

    | infer (Js, G, J as (Term.Pair (M, N), Prop.And (A, B))) = 
	if immed (Js, NonHyp (M, A)) andalso immed (Js, NonHyp (N, B)) then Inf
	else NotInf ("Invalid &-introduction. Proof of "^JToString (M, A)^" and " 
		     ^JToString (N, B) ^ " missing")
    | infer (Js, G, J as (Term.Pair (M, N), Prop.Equiv (A, B))) =
        infer (Js, G, (Term.Pair (M, N), Prop.equivDef (A, B)))
    | infer (Js, G, J as (Term.Pair (M, N), A)) = wrongProofTerm (J)

    | infer (Js, G, J as (Term.Inl (M), Prop.Or (A, B))) = 
	if immed (Js, NonHyp (M, A)) then Inf
        else NotInf ("Invalid |-introduction. Proof of "^JToString (M, A) ^" missing")
    | infer (Js, G, J as (Term.Inl (M), A)) = wrongProofTerm (J)
    | infer (Js, G, J as (Term.Inr (M), Prop.Or (A, B))) = 
	if immed (Js, NonHyp (M, B)) then Inf
        else NotInf ("Invalid |-introdution. Proof of "^JToString (M, B)^" missing")
    | infer (Js, G, J as (Term.Inr (M), A)) = wrongProofTerm (J)

    | infer (Js, G, J as (Term.Lam ((x, M)), Prop.Implies (A, B))) =
        if immed (Js, Hypothetical (AnnProof.Hyp (x, A), M, B)) then Inf
	else NotInf ("Invalid =>-introduction. Hypothetical proof "^x^": "^Prop.toString A
		     ^" |- "^JToString (M, B)^" missing")
    | infer (Js, G, J as (Term.Lam ((x, M)), Prop.Not (A))) =
        infer (Js, G, (Term.Lam ((x, M)), Prop.notDef (A)))
    | infer (Js, G, J as (Term.Lam ((x, M)), A)) = wrongProofTerm (J)

    | infer (Js, G, J as (Term.Star, Prop.True)) = Inf
    | infer (Js, G, J as (Term.Star, A)) = wrongProofTerm (J)

(* elimination rules *)

    | infer (Js, G, J as (Term.Fst (M), A)) = 
        let 
	    fun elim (Empty) = NotInf ("Not a valid elimination: No proof of " ^ Term.toString (M) ^ " : " ^ Prop.toString' (A) ^ " & _ found")  
	      | elim (Ext (NonHyp (N, Prop.And (B, C)), Js')) = 
                  if Prop.eq (A, B) andalso Term.eq (M, N) then Inf
		  else elim (Js')
	      | elim (Ext (NonHyp (N, Prop.Equiv (B, C)), Js')) =
                  elim (Ext (NonHyp (N, Prop.equivDef (B, C)), Js'))
	      | elim (Ext (_, Js')) = elim (Js')
	in 
	    elim (Js)
        end
    | infer (Js, G, J as (Term.Snd (M), A)) = 
        let 
	    fun elim (Empty) = NotInf ("Not a valid elimination: No proof of " ^ Term.toString (M) ^ " : _ & " ^ Prop.toString' (A) ^ " found")  
	      | elim (Ext (NonHyp (N, Prop.And (B, C)), Js')) = 
                  if Prop.eq (A, C) andalso Term.eq (M, N) then Inf
		  else elim (Js')
	      | elim (Ext (NonHyp (N, Prop.Equiv (B, C)), Js')) =
                  elim (Ext (NonHyp (N, Prop.equivDef (B, C)), Js'))
	      | elim (Ext (_, Js')) = elim (Js')
	in 
	    elim (Js)
        end
	     
    | infer (Js, G, J as (Term.Case (M, (x, N), (y, O)), C)) = 
	let 
	    fun elim (Empty) = NotInf ("Not a valid elimination: Term "^Term.toString (M) ^ " does not prove a suitable disjunction")
	      | elim (Ext (NonHyp (M', Prop.Or (A, B)), Js')) = 
                  if Term.eq (M, M') andalso
		     immed (Js, Hypothetical (AnnProof.Hyp (x, A), N, C)) andalso
		     immed (Js, Hypothetical (AnnProof.Hyp (y, B), O, C)) then Inf
		  else elim (Js')
	      | elim (Ext (_, Js')) = elim (Js')
	in 
	    elim (Js)
        end

    | infer (Js, G, J as (Term.App (M, N), C)) =
	let 
	    fun elim (Empty) = NotInf ("Not a valid elimination: No proof of " ^ Term.toString (M) ^ " : _ => " ^ Prop.toString' (C) ^ " found")
	      | elim (Ext (NonHyp (M', Prop.Implies (A, B)), Js')) =
		  if Term.eq (M, M') andalso Prop.eq (B, C) 
		      andalso immed (Js, NonHyp (N, A)) then Inf
		  else elim (Js')
	      | elim (Ext (NonHyp (M', Prop.Not (A)), Js')) =
		  elim (Ext (NonHyp (M', Prop.notDef (A)), Js'))
	      | elim (Ext (_, Js')) = elim (Js')
	in
	   elim (Js)
	end

    | infer (Js, G, J as (Term.Abort (M), C)) = 
	if immed (Js, NonHyp (M, Prop.False)) then Inf
        else NotInf ("Not a valid elimination: No proof of " ^ Term.toString (M) ^ " : _ => " ^ Prop.toString' (Prop.False) ^ " found")

(* annotation *)
    | infer (Js, G, J as (Term.Ann (M, A), C)) = 
        if Prop.eq (A, C) then infer (Js, G, (M, A)) 
        else NotInf ("Proposition annotation "^Prop.prettyString (A)^" of term "^Term.toString (M)^" does not match current goal "^Prop.prettyString (C))


  fun printLine (nest, term, prop) =  print (
        StringCvt.padRight #" " (nest*2) "" ^
	StringCvt.padRight #" " (60 - nest*2) term ^ " : " ^
        prop ^ "\n")

  (*  printFirst : print the first line of a frame (hypothesis) *)
  fun printFirst (nest, x, A) = printLine (nest, "[ " ^ x, Prop.prettyString (A) ^ ";") 

  (*  printStep : print a line 
           last indicated whether it is the last line, which is closed with
           a ] instead of a ;
  *)
  fun printStep (nest, M, A, last) = printLine (nest,
        Term.toString (M), 
        Prop.prettyString (A) ^ (if last then (if nest>0 then " ];" else "") else ";")) 
       

  val printUnjust = printStep

  (* PROVING LOOP *)   

  fun check (Pp, Gg) = 
  (* invariants? -fp *)
  let val rt = ExtSyn.getRegionTreeAnnProof (Pp)
      val valid = ref true       (* proof valid? *)

      (* the following functions are local to check for two reasons:
         - to alter the flag "valid"
         - to access the region tree of the proof for error messages via "msg"
      *)
      fun msg (path, s) = Region.wrap (Paths.toRegionAnnProof (Paths.rev (path), rt), s)

      (*  step : int * Judgments * Assertion * Paths.RevPath * bool 
              -> Judgment *)
      (*  step (nest, Js, E as AnnProof.Line(A), path, last) = NonHyp(A)
       *  step (nest, Js, E as AnnProof.Frame _, path, last) = Hypothetical _
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
      fun step (nest, Js, G, AnnProof.Line (M, A), path, last) = 
            ( (* print ("Checking line: " ^Term.toString (M)^" : "^
		    Prop.prettyString (A)^ " ...\n"); *)
              case infer (Js, G, (M, A)) of
                Inf => (
		  if Chatter.declDetails() then printStep (nest, M, A, last) else (); 
	          NonHyp (M, A))
              | NotInf (s) => (
		  print (msg (path, 
		    "Unjustified line " ^ 
                    (if Chatter.errorDetails() then JsToString Js ^ "  |-  " ^ 
                      JToString (M, A) else "")
                    ^  "\nReason: " ^ s ^ "\n" ^ 
                    (if Chatter.actions() then 
                      "Assuming this line, checking remainder...\n" else "")));
	          valid := false; 
                  if Chatter.declDetails() then printUnjust (nest, M, A, last) else (); 
                  NonHyp (M, A)))
	| step (nest, Js, G, AnnProof.Frame (AnnProof.Hyp (x, A), P), path, last) = 
           ( 
            if Chatter.declDetails() then printFirst (nest, x, A) else (); 
	    case check' (nest+1, Ext (NonHyp (Term.Var (x), A), Js), 
			 Cxt.Ext (x, A, G), P, Paths.right (path)) of
	        NonHyp (M, B) => Hypothetical (AnnProof.Hyp (x, A), M, B)
	      | Hypothetical _ => error (msg (path, "Hypothetical judgment at the end of a proof"))) 

      (*  check' : int * Judgments * Proof * path -> Judgment *)
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
      and check' (nest, Js, G, AnnProof.Final (E), path)   = 
            step (nest, Js, G, E, Paths.this (path), true)
	| check' (nest, Js, G, AnnProof.Step (E, P), path) = 
            check' (nest, Ext (step (nest, Js, G, E, Paths.this (path), false), 
                               Js), 
                    G, P, Paths.next (path))

      (*  check: runs check' and compares the result to the goal *) 

      val P = ExtSyn.getAnnProof (Pp)
      val Goal = ExtSyn.getProp (Gg)

      fun returnOK () = 
	   (if Chatter.finalResults() then print "QED\n" else(); Global.exitOK)
      fun returnUnjustified () =
	   (if Chatter.finalResults() then print "Proof incomplete\n" else (); 
	    Global.exitUnjustified)   
      fun returnWrongGoal () =
	   (if Chatter.finalResults() then print (
                "Proof valid, but does not match the goal "
	        ^ Prop.prettyString Goal^ "\n") else ();
	    Global.exitWrongGoal)
      fun returnWrongGoalUnjust () =
           (if Chatter.finalResults() then print (
                "Proof incomplete and does not match the goal "
		^ Prop.prettyString Goal ^ "\n") else ();
	    Global.exitUnjustified)
      fun returnInvalid () =
	   (print (msg (Paths.top, "Hypothetical judgment at the end of a proof\n"));
	    Global.exitProofInvalid)

  in
      case check' (0, Empty, Cxt.Empty, P, Paths.top) of
	  NonHyp (M, A) => 
	    if Prop.eq (A, Goal) then
		if !valid then returnOK ()
		else returnUnjustified ()
	    else if !valid then returnWrongGoal ()
	    else returnWrongGoalUnjust ()
	| Hypothetical _ => returnInvalid ()
  end handle Error (msg) => (print (msg ^ "\n"); Global.exitProofInvalid)

end (* structure CheckAnnProof *)