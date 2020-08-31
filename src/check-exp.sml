(* $Id: check-exp.sml,v 1.2 2000/10/24 21:09:53 abel Exp $ *)

signature CHECKEXP = 
sig

  val check : Exp.Cxt * Syntax.Exp * Syntax.Exp -> unit
  val checkExp : Exp.Cxt * Syntax.Exp * Syntax.Exp -> Global.result

end (* signature CHECKEXP *)


structure CheckExp =
struct

  exception Error of string

  fun error (msg) = raise Error (msg)

  fun Exp_eq (S, T) = S = T

  fun check (globG, Mm, Ss) =
  let 
      val rexp = Syntax.getRegTree (Mm)
(*      val rta = Syntax.getRegionTreeType (Ss) *)

      fun msg (path, s) = Region.wrap (Path.toRegion (Path.rev (path), rexp), s)

      (*  inferArg: *)
      fun inferArg (y, Gy, (G, C), Exp.Var (x), path) = 
           if x=y then
             let
		 fun lookup (Cxt.Empty) =  (case Cxt.lookup (x, C) of
			     NONE => error (msg (path, "Variable "^x^" not bound in context"))
			   | SOME _ =>  error (msg (path, "Primitive recursive function identifier "^x^" illegal in this position")))
		   | lookup (G' as Cxt.Ext (z, T, G'')) = 
                       if z=x then 
                         if Cxt.length (G') = Cxt.length (Gy) then (* contexts are equal ? *)
                           T
                         else error (msg (path, "Illegal argument "^x^" to primitive recursive function. Legal argument has been shadowed."))
		       else lookup G''
             in
                lookup G
	     end
	   else error (msg (path, "Illegal argument "^x^" to primitive recursive function. Legal argument is only "^y))
	| inferArg (y, Gy, (G, C), Exp.Ann (M, (T, U)), path) = 
            let val S = inferArg (y, Gy, (G, C), M, Path.left (path))
	    in
		if Exp_eq (S, T) then T
		else error (msg (Path.right (path), "Annotated type "^Exp.typeToString T^" is different from expected type "^Exp.typeToString S))
	    end
	| inferArg (y, Gy, (G, C), M, path) =
	    error (msg (path, "Illegal argument "^Exp.termToString M^" to primitive recursive function. Legal argument is only "^y))
                

      (*  infer' : Exp.Cxt * (string * Exp.Cxt * Exp.Type) Cxt.Cxt * Exp.Exp * Path.path  *)
      fun infer' ((G, C), Exp.Var (x), path) = 
	   (case Cxt.lookup (x, G) of 
		NONE => (case Cxt.lookup (x, C) of
			     NONE => error (msg (path, "Variable "^x^" not bound in context"))
			   | SOME _ =>  error (msg (path, "Primitive recursive function identifier "^x^" illegal in this position")))
	      | SOME (T) => T)

	| infer' ((G, C), Exp.Fst (M), path) = 
	   (case infer' ((G, C), M, Path.this (path)) of
		Exp.Sigma (Range.Type, S, T) => S
	      | S => error (msg (Path.this (path), "Value "^Exp.termToString M^" should be of product type, but is of type "^Exp.typeToString S)))

	| infer' ((G, C), Exp.Snd (M), path) = 
	   (case infer' ((G, C), M, Path.this (path)) of
		Exp.Sigma (Range.Type, S, T) => T
	      | S => error (msg (Path.this (path), "Value "^Exp.termToString M^" should be of product type, but is of type "^Exp.typeToString S)))

	| infer' ((G, C), Exp.App (N as (Exp.Var(f)), M), path) =
           (case Cxt.lookup (f, C) of
		SOME (G' as Cxt.Ext (x, _, _), S') => 
                  let val T = inferArg (x, G', (G, C), M, Path.left (path))
		  in
		      S'
		  end
	      | NONE => inferApp ((G, C), N, M, path)) 
	| infer' ((G, C), Exp.App (M, N), path) =
           inferApp ((G, C), M, N, path)  

	| infer' ((G, C), Exp.Ann (M, (S, U)), path) = (check' ((G, C), M, S, Path.left (path)); S)

	| infer' ((G, C), M, path) = error (msg (path, "Cannot infer the type of value "^Exp.termToString (M)^", please insert annotation"))

										     and inferApp ((G, C), M, N, path) =
           (case infer' ((G, C), M, Path.left (path)) of
                Exp.Pi (Range.Type, S, T) => (check' ((G, C), N, S, Path.right (path)); T) 
	      | S => error (msg (Path.left (path), "Value "^Exp.termToString M^" should be of arrow type, but is of type "^Exp.typeToString S))) 


      and check' ((G, C), Exp.Pair (M, N), Exp.Sigma (Range.Type, S, T), path) =
           (check' ((G, C), M, S, Path.left (path));
	    check' ((G, C), N, T, Path.right (path)))
	| check' ((G, C), exp as Exp.Pair (M, N), S, path) =
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of product type, but in this place something of type "^Exp.typeToString S^" is required."))

	| check' ((G, C), Exp.Inl (M), Exp.Sum (S, T), path) =
            check' ((G, C), M, S, Path.this (path))
	| check' ((G, C), exp as Exp.Inl (M), S, path) =
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of sum type, but in this place something of type "^Exp.typeToString S^" is required."))

	| check' ((G, C), Exp.Inr (M), Exp.Sum (S, T), path) =
            check' ((G, C), M, T, Path.this (path))
	| check' ((G, C), exp as Exp.Inr (M), S, path) =
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of sum type, but in this place something of type "^Exp.typeToString S^" is required."))
       
	| check' ((G, C), Exp.Case (M, (x, N), (y, O)), S', path) =
           (case infer' ((G, C), M, Path.left (path)) of
	       Exp.Sum (S, T) => 
                (check' ((Cxt.Ext (x, S, G), C), N, S', Path.middle (path));
		 check' ((Cxt.Ext (y, T, G), C), O, S', Path.right (path)))
	     | S => error (msg (path, "Value "^Exp.termToString M^" is not of sum type as expected, but "^Exp.typeToString S)))

	| check' ((G, C), Exp.Lam (x, M), Exp.Pi (Range.Type, S, T), path) =
	    check' ((Cxt.Ext (x, S, G), C), M, T, Path.this (path))
	| check' ((G, C), exp as Exp.Lam _, S, path) =
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of arrow type, but in this place something of type "^Exp.typeToString S^" is required."))

	| check' ((G, C), Exp.Star, Exp.Unit, path) = ()
	| check' ((G, C), exp as Exp.Star, S, path) = 
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of unit type, but in this place something of type "^Exp.typeToString S^" is required."))

	| check' ((G, C), Exp.Abort (M), S', path) =
           (case infer' ((G, C), M, Path.this (path)) of
                Exp.Empty => ()
	      | S => error (msg (Path.this (path), "The value "^Exp.termToString M^" is not of the empty type as expected, but "^Exp.typeToString S)))

	| check' ((G, C), Exp.Zero, Exp.Nat, path) = ()
	| check' ((G, C), exp as Exp.Zero, S, path) = 
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of type nat, but in this place something of type "^Exp.typeToString S^" is required."))

	| check' ((G, C), Exp.Succ (N), Exp.Nat, path) = 
            check' ((G, C), N, Exp.Nat, Path.this (path))
	| check' ((G, C), exp as (Exp.Succ (N)), S, path) = 
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of type nat, but in this place something of type "^Exp.typeToString S^" is required."))

	| check' ((G, C), exp as (Exp.RecNat (M, f, N, (x, L))), S, path) =
           (case infer' ((G, C), M, Path.left (path)) of
		Exp.Nat => (check' ((G, C), N, S, Path.middle (path));
                             let 
				 val G' = Cxt.Ext (x, Exp.Nat, G) 
				 val C' = Cxt.Ext (f, (G', S), C)    (* variable x lies on top of G' *)
                             in  
			         check' ((G', C'), L, S, Path.right (path))
			     end)
	      | T => error (msg (Path.left (path), "Head "^Exp.termToString exp^
                       " of primitive recursion is not of type nat as expected, but "^Exp.typeToString T)))

	| check' ((G, C), Exp.True, Exp.Bool, path) = ()
	| check' ((G, C), exp as Exp.True, S, path) = 	    
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of type bool, but in this place something of type "^Exp.typeToString S^" is required."))

	| check' ((G, C), Exp.False, Exp.Bool, path) = ()
	| check' ((G, C), exp as Exp.False, S, path) = 	    
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of type bool, but in this place something of type "^Exp.typeToString S^" is required."))

	| check' ((G, C), Exp.If (M, N, O), T, path) = 
           (case infer' ((G, C), M, Path.left (path)) of
                Exp.Bool => (check' ((G, C), N, T, Path.middle (path));
			      check' ((G, C), O, T, Path.right (path)))
	      | S => error (msg (path, "The value "^Exp.termToString M^" is not of type bool as expected, but "^Exp.typeToString S)))


	| check' ((G, C), Exp.Nil, Exp.List(S), path) = ()
	| check' ((G, C), exp as Exp.Nil, S, path) = 
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of list type, but in this place something of type "^Exp.typeToString S^" is required."))

	| check' ((G, C), Exp.Cons (M, N), Exp.List(S), path) = 
           (check' ((G, C), M, S, Path.left (path));
	    check' ((G, C), N, Exp.List(S), Path.right (path)))
	| check' ((G, C), exp as (Exp.Cons (M, N)), S, path) = 
	    error (msg (path, "Type mismatch: "^Exp.termToString exp^" is of list type, but in this place something of type "^Exp.typeToString S^" is required."))

	| check' ((G, C), exp as (Exp.RecList (M, f, N, (x, xs, L))), S, path) =
           (case infer' ((G, C), M, Path.left (path)) of
		Exp.List(T) => (check' ((G, C), N, S, Path.middle (path));
				 let
				     val G' = Cxt.Ext (xs, Exp.List (T), Cxt.Ext (x, T, G))
				     val C' = Cxt.Ext (f, (G', S), C)
				 in 
				     check' ((G', C'), L, S, Path.right (path))
				 end)
	      | T => error (msg (Path.left (path), "Head "^Exp.termToString exp^
                " of primitive recursion is not of list type as expected, but "^Exp.typeToString T)))

	| check' ((G, C), Exp.Ann (M, (S, U)), S', path) =
	    if Exp_eq (S, S') then check' ((G, C), M, S, Path.left (path))
	    else error (msg (Path.right (path), "Annotated type "^Exp.typeToString S^" is different from expected type "^Exp.typeToString S'))

	| check' ((G, C), M, S', path) = 
	    let val S = infer' ((G, C), M, path)
            in 
		if Exp_eq (S, S') then  ()
	        else error (msg (path, "Type mismatch: "^Exp.termToString M^" is of type "^Exp.typeToString S^", but in this place something of type "^Exp.typeToString S'^" is required."))
	    end
      
      val M = Syntax.getTerm (Mm)
      val S = Syntax.getType (Ss)

  in 
      check' ((globG, Cxt.Empty), M, S, Path.top)
  end

  fun checkExp (globG, Mm, Ss) =
    let
        val M = Syntax.getTerm (Mm)
	val S = Syntax.getType (Ss)
    in
       check (globG, Mm, Ss);
	if Chatter.declDetails() then print ("|- "^Exp.termPretty M^"\n : "
             ^ Exp.typePretty S ^ "\n") else ();
	if Chatter.finalResults() then print "OK\n" else(); 
        Global.exitOK
    end handle Error (msg) => (print (msg ^ "\n"); Global.exitTermCheck)

end (* structure CheckExp *)



