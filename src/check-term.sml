(* $Id: check-term.sml,v 1.1 2000/10/17 22:23:08 abel Exp $ *)

signature CHECKTERM = 
sig

  val check : ExtSyn.term * ExtSyn.prop -> Global.result

end (* signature CHECKTERM *)


structure CheckTerm =
struct

  exception Error of string

  fun error (msg) = raise Error (msg)

  fun check (Mm, Aa) =
  let 
      val rtm = ExtSyn.getRegionTreeTerm (Mm)
(*      val rta = ExtSyn.getRegionTreeProp (Aa) *)

      fun msg (path, s) = Region.wrap (Paths.toRegionTerm (Paths.rev (path), rtm), s)

      fun infer' (G, Term.Var (x), path) = 
	   (case Cxt.lookup (x, G) of 
		NONE => error (msg (path, "Variable "^x^" not bound in context"))
	      | SOME (B) => B)

	| infer' (G, Term.Fst (M), path) = 
	   (case infer' (G, M, Paths.this (path)) of
		Prop.And (A, B) => A
	      | Prop.Equiv (A, B) => Prop.Implies (A, B)
	      | A => error (msg (Paths.this (path), "Term "^Term.toString M^" should prove a conjunction, but proves "^Prop.toString (A))))

	| infer' (G, Term.Snd (M), path) = 
	   (case infer' (G, M, Paths.this (path)) of
		Prop.And (A, B) => B
	      | Prop.Equiv (A, B) => Prop.Implies (B, A)
	      | A => error (msg (Paths.this (path), "Term "^Term.toString M^" should prove a conjunction, but proves "^Prop.toString (A))))

	| infer' (G, Term.App (M, N), path) =
           (case infer' (G, M, Paths.left (path)) of
                Prop.Implies (A, B) => (check' (G, N, A, Paths.right (path)); B) 
	      | Prop.Not (A) =>  (check' (G, N, A, Paths.right (path)); Prop.False)
	      | A => error (msg (Paths.left (path), "Term "^Term.toString M^" should prove an implication, but proves "^Prop.toString (A)))) 

	| infer' (G, Term.Ann (M, A), path) = (check' (G, M, A, Paths.left (path)); A)

	| infer' (G, M, path) = error (msg (path, "Cannot infer the proposition of proof "^Term.toString (M)^", please insert annotation"))



      and check' (G, Term.Pair (M, N), Prop.And (A, B), path) =
           (check' (G, M, A, Paths.left (path));
	    check' (G, N, B, Paths.right (path)))
	| check' (G, Term.Pair (M, N), Prop.Equiv (A, B), path) =
           (check' (G, M, Prop.Implies (A, B), Paths.left (path));
	    check' (G, N, Prop.Implies (B, A), Paths.right (path)))
	| check' (G, tm as Term.Pair (M, N), A, path) =
	    error (msg (path, "Pair "^Term.toString tm^" does not prove a conjunction as expected, but "^Prop.toString A))

	| check' (G, Term.Inl (M), Prop.Or (A, B), path) =
            check' (G, M, A, Paths.this (path))
	| check' (G, tm as Term.Inl (M), A, path) =
	    error (msg (path, "Injection "^Term.toString tm^" does not prove a disjunction as expected, but "^Prop.toString A))

	| check' (G, Term.Inr (M), Prop.Or (A, B), path) =
            check' (G, M, B, Paths.this (path))
	| check' (G, tm as Term.Inr (M), A, path) =
	    error (msg (path, "Injection "^Term.toString tm^" does not prove a disjunction as expected, but "^Prop.toString A))
       
	| check' (G, Term.Case (M, (x, N), (y, O)), C, path) =
           (case infer' (G, M, Paths.left (path)) of
	       Prop.Or (A, B) => 
                (check' (Cxt.Ext (x, A, G), N, C, Paths.middle (path));
		 check' (Cxt.Ext (y, B, G), O, C, Paths.right (path)))
	     | A => error (msg (path, "Term "^Term.toString M^" does not prove a disjunction as expected, but "^Prop.toString A)))

	| check' (G, Term.Lam (x, M), Prop.Implies (A, B), path) =
	    check' (Cxt.Ext (x, A, G), M, B, Paths.this (path))
	| check' (G, Term.Lam (x, M), Prop.Not (A), path) =
	    check' (Cxt.Ext (x, A, G), M, Prop.False, Paths.this (path))
	| check' (G, tm as Term.Lam _, A, path) =
	    error (msg (path, "Abstraction "^Term.toString tm^" does not prove an implication as expected, but "^Prop.toString A))

	| check' (G, Term.Star, Prop.True, path) = ()
	| check' (G, tm as Term.Star, A, path) = 
	    error (msg (path, "The empty tuple "^Term.toString tm^" does not prove truth as expected, but "^Prop.toString A))

	| check' (G, Term.Abort (M), C, path) =
           (case infer' (G, M, Paths.this (path)) of
                Prop.False => ()
	      | A => error (msg (Paths.this (path), "The term "^Term.toString M^" does not prove falsehood as expected, but "^Prop.toString A)))

	| check' (G, Term.Ann (M, A), C, path) =
	    if Prop.eq (A, C) then check' (G, M, A, Paths.left (path))
	    else error (msg (Paths.right (path), "Annotated proposition "^Prop.toString A^" is different from expected proposition "^Prop.toString C))

	| check' (G, M, C, path) = 
	    let val A = infer' (G, M, path)
            in 
		if Prop.eq (A, C) then  ()
		else error (msg (path, "Term "^Term.toString M^" does not prove "^Prop.toString C^" as expected, but "^Prop.toString A))
	    end
      
      fun returnOK () = 
	   (if Chatter.finalResults() then print "QED\n" else(); Global.exitOK)
 
      val M = ExtSyn.getTerm (Mm)
      val A = ExtSyn.getProp (Aa)
  in
     check' (Cxt.Empty, M, A, Paths.top); 
     if Chatter.declDetails() then print ("|- "^Term.toString M^"\n : "^ Prop.prettyString A ^ "\n") else ();
     returnOK ()
  end handle Error (msg) => (print (msg ^ "\n"); Global.exitTermCheck)

end (* structure CheckTerm *)



