(* $Id: check-dep.sml,v 1.3 2000/10/27 17:08:14 abel Exp $ *)

signature CHECKDEP = 
sig

  val check : Exp.Cxt * Syntax.Exp * Syntax.Exp -> unit
  val checkExp : Exp.Cxt * Syntax.Exp * Syntax.Exp -> Global.result

end (* signature CHECKDEP *)


structure CheckDep =
struct

  datatype Type =
      Prod
    | Sum
    | Arrow
    | Unit
    | Empty
    | Pi
    | Sigma
    | Nat
    | Bool
    | List
    | EqNat
    | Less
    | EqList

  exception Error of string
  (* fun error (msg) = raise Error (msg) *)

  fun Exp_eq (e, S, T) = Val.eqExp' (e, S, T)

  fun check (globG, Mm, Ss) =
  let 
      val rexp = Syntax.getRegTree (Mm)
(*      val rta = Syntax.getRegionTreeType (Ss) *)

      fun msg (path, s) = Region.wrap (Path.toRegion (Path.rev (path), rexp), s)

      fun checkFails (path, t, f, e, (V, U)) = msg (path, "Type mismatch: "
            ^ Exp.termPretty (t) ^ (case (f, U) of
		(Prod, Univ.Type) => " is of product type"
	      | (Sum, Univ.Type) => " is of sum type"
	      | (Arrow, Univ.Type) => " is of arrow type"
	      | (Unit, Univ.Type) => " is of unit type"
	      | (Nat, U) => " is of type nat"
	      | (Bool, U) => " is of type bool"
	      | (List, U) => " is of list type"
	      | (Prod, Univ.Prop) => " proves a conjunction"
	      | (Sum, Univ.Prop) => " proves a disjunction"
	      | (Arrow, Univ.Prop) => " proves an implication"
	      | (Pi, Univ.Prop) => " proves a universal quantification"
	      | (Sigma, Univ.Prop) => " proves a existential quantification"  
	      | (Unit, Univ.Prop) =>  " proves truth"
	      | (EqNat, U) =>  " proves an equation between natural numbers"
	      | (EqList, U) =>  " proves an equation between lists"
	      | (Less, U) =>  " proves a less-than relation between natural numbers")
	    ^ ", but "^ (case U of
	        Univ.Type => "something of type "
	      | Univ.Prop => "a proof of ") ^ UVal.pretty (e, (V, U))
            ^ " is required")

      fun inferFails (path, t, f, e, (V, U)) = msg (path, "Type mismatch: "
            ^ Exp.termPretty (t) ^ (case U of
	        Univ.Type => " is of type "
	      | Univ.Prop => " proves ") ^ UVal.pretty (e, (V, U))
	    ^ ", but "^ (case U of
	        Univ.Type => "something of"
	      | Univ.Prop => "a proof of") ^ (case (f, U) of
		(Prod, Univ.Type) => " product type"
	      | (Sum, Univ.Type) => " sum type"
	      | (Arrow, Univ.Type) => " arrow type"
	      | (Empty, Univ.Type) => " empty type"
	      | (Nat, U) => " type nat"
	      | (Bool, U) => " type bool"
	      | (List, U) => " list type"
	      | (Prod, Univ.Prop) => " a conjunction"
	      | (Sum, Univ.Prop) => " a disjunction"
	      | (Arrow, Univ.Prop) => " an implication"
	      | (Pi, Univ.Prop) => " a universal quantification"
	      | (Sigma, Univ.Prop) => " an existential quantification"
	      | (Empty, Univ.Prop) =>  " falsehood"
	      | (EqNat, U) => " an equation between natural numbers"
	      | (EqList, U) => " an equation between lists"
	      | (Less, U) => " a less-than relation between natural numbers")
            ^ " is required in this place")
(*
      fun checkFails (path, t, f, e, (V, U)) = msg (path, "Type mismatch: "
            ^ Exp.termPretty (t) ^ (case (f, U) of
		(Prod, Univ.Type) => " is of product type"
	      | (Sum, Univ.Type) => " is of sum type"
	      | (Arrow, Univ.Type) => " is of arrow type"
	      | (Unit, Univ.Type) => " is of unit type"
	      | (Nat, Univ.Type) => " is of type nat"
	      | (Bool, Univ.Type) => " is of type bool"
	      | (List, Univ.Type) => " is of list type"
	      | (Prod, Univ.Prop) => " proves a conjunction"
	      | (Sum, Univ.Prop) => " proves a disjunction"
	      | (Arrow, Univ.Prop) => " proves an implication"
	      | (Pi, Univ.Prop) => " proves a universal quantification"
	      | (Sigma, Univ.Prop) => " proves a existential quantification"  
	      | (Unit, Univ.Prop) =>  " proves truth"
	      | (EqNat, Univ.Prop) =>  " proves an equation between natural numbers"
	      | (EqList, Univ.Prop) =>  " proves an equation between lists"
	      | (Less, Univ.Prop) =>  " proves a less-than relation between natural numbers")
	    ^ ", but "^ (case U of
	        Univ.Type => "something of type "
	      | Univ.Prop => "a proof of ") ^ UVal.pretty (e, (V, U))
            ^ " is required")

      fun inferFails (path, t, f, e, (V, U)) = msg (path, "Type mismatch: "
            ^ Exp.termPretty (t) ^ (case U of
	        Univ.Type => " is of type "
	      | Univ.Prop => " proves ") ^ UVal.pretty (e, (V, U))
	    ^ ", but "^ (case U of
	        Univ.Type => "something of"
	      | Univ.Prop => "a proof of") ^ (case (f, U) of
		(Prod, Univ.Type) => " product type"
	      | (Sum, Univ.Type) => " sum type"
	      | (Arrow, Univ.Type) => " arrow type"
	      | (Empty, Univ.Type) => " empty type"
	      | (Nat, Univ.Type) => " type nat"
	      | (Bool, Univ.Type) => " type bool"
	      | (List, Univ.Type) => " list type"
	      | (Prod, Univ.Prop) => " a conjunction"
	      | (Sum, Univ.Prop) => " a disjunction"
	      | (Arrow, Univ.Prop) => " an implication"
	      | (Pi, Univ.Prop) => " a universal quantification"
	      | (Sigma, Univ.Prop) => " an existential quantification"
	      | (Empty, Univ.Prop) =>  " falsehood"
	      | (EqNat, Univ.Prop) => " an equation between natural numbers"
	      | (EqList, Univ.Prop) => " an equation between lists"
	      | (Less, Univ.Prop) => " a less-than relation between natural numbers")
            ^ " is required in this place")
*)

(*     
      fun lookup (x, Cxt.Empty) = 
	   (case Cxt.lookup (x, globG) of
		NONE => NONE
	      | SOME (A) => SOME (Val.Clos (A, Cxt.Empty)))
	| lookup (x, Cxt.Ext (y, VU, G)) = if x=y then VU else lookup (x, G)
*)
      val lookup = Cxt.lookup


      (*  inferArg: *)
      fun inferArg (y, Gy, (e, G, C), Exp.Var (x), path) = 
           if x=y then
             let
		 fun lookup' (Cxt.Empty) =  (case Cxt.lookup (x, C) of
			     NONE => raise Error (msg (path, "Variable "^x^" not bound in context"))
			   | SOME _ =>  raise Error (msg (path, "Primitive recursive function identifier "^x^" illegal in this position")))
		   | lookup' (G' as Cxt.Ext (z, VU, G'')) = 
                       if z=x then 
                         if Cxt.length (G') = Cxt.length (Gy) then (* contexts are equal ? *)
                           VU
                         else raise Error (msg (path, "Illegal argument "^x^" to primitive recursive function. Legal argument has been shadowed."))
		       else lookup' G''
             in
                lookup' G
	     end
	   else raise Error (msg (path, "Illegal argument "^x^" to primitive recursive function. Legal argument is only "^y))
	| inferArg (y, Gy, (e as (i, e'), G, C), Exp.Ann (M, SU), path) = 
            let val VU = inferArg (y, Gy, (e, G, C), M, Path.left (path))
	    in
		if UVal.eqExpVal (e, SU, VU) then UVal.whnf (SU, e')
		else raise Error (msg (Path.right (path), "Expression "
		  ^Exp.termPretty M^" is of type "^UVal.pretty (e, VU)^
                   ", which conflicts with the annotation "^UExp.toString SU))
	    end
	| inferArg (y, Gy, (e, G, C), M, path) =
	    raise Error (msg (path, "Illegal argument "^Exp.termToString M^" to primitive recursive function. Legal argument is only "^y))
                

      (*  infer' : (Val.Env * Exp.Cxt * (string * Exp.Cxt * Exp.Type) Cxt.Cxt) 
                   * Exp.Exp * Path.path -> UVal.UVal
      *)
      fun infer' ((e, G, C), Exp.Var (x), path) = 
	   (case lookup (x, G) of 
		NONE => (case Cxt.lookup (x, C) of
			     NONE => raise Error (msg (path, "Variable "^x^" not bound in context"))
			   | SOME _ =>  raise Error (msg (path, "Primitive recursive function identifier "^x^" illegal in this position")))
	      | SOME (T) => T)

	| infer' ((e, G, C), Exp.Fst (M), path) = 
	   (case infer' ((e, G, C), M, Path.this (path)) of
	       (Val.Clos (Exp.Sigma (Range.Dep (x), S, T), e'), U) => (Val.whnf (S, e'), Univ.Type)
	      |(Val.Clos (Exp.Sigma (Range.Type, S, T), e'), U) => (Val.whnf (S, e'), Univ.Type)
	      |(Val.Clos (Exp.Sigma (Range.Prop, S, T), e'), U) => (Val.whnf (S, e'), Univ.Prop)
	      | SU => raise Error (inferFails (Path.this (path), M, Prod, e, SU))) 

	| infer' (((i, e), G, C), Exp.Snd (M), path) = 
	   (case infer' (((i, e), G, C), M, Path.this (path)) of
	       (Val.Clos (Exp.Sigma (Range.Dep (x), S, T), e'), U) => 
                 (Val.whnf (T, Cxt.Ext (x, Val.eval (Exp.Fst (M), e), e')), U)
	      |(Val.Clos (Exp.Sigma (R, S, T), e'), U) => (Val.whnf (T, e'), U)
	      | SU => raise Error (inferFails (Path.this (path), M, Prod, (i, e), SU))) 

	| infer' ((e, G, C), Exp.App (N as (Exp.Var(f)), M), path) =
           (case Cxt.lookup (f, C) of
		SOME (G' as Cxt.Ext (x, _, _), SU') => 
                  let val T = inferArg (x, G', (e, G, C), M, Path.left (path))
		  in
		      SU'
		  end
	      | NONE => inferApp ((e, G, C), N, M, path)) 
	| infer' ((e, G, C), Exp.App (M, N), path) =
           inferApp ((e, G, C), M, N, path)  

	| infer' ((e, G, C), exp as (Exp.EqESS (M)), path) =
            let fun msg(SU) = inferFails (Path.this (path), M, EqNat, e, SU)
            in
              case infer' ((e, G, C), M, Path.this(path)) of
                  SU as (Val.Equal (v, w), Univ.Prop) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
                       (Val.Succ (v'), Val.Succ (w')) => (Val.Equal (v', w'), Univ.Prop)
		     | (v', w') => raise Error (msg (SU)))
		| SU => raise Error (msg (SU))
	    end
	| infer' ((e, G, C), exp as (Exp.LessES (M)), path) =
            let fun msg(SU) = inferFails (Path.this (path), M, Less, e, SU)
            in
              case infer' ((e, G, C), M, Path.this(path)) of
                  SU as (Val.Less (v, w), Univ.Prop) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
                       (Val.Succ (v'), Val.Succ (w')) => (Val.Less (v', w'), Univ.Prop)
		     | (v', w') => raise Error (msg (SU)))
		| SU => raise Error (msg (SU))
	    end
	| infer' ((e, G, C), exp as (Exp.EqECC (M)), path) =
            let fun msg(SU) = inferFails (Path.this (path), M, EqList, e, SU)
            in
              case infer' ((e, G, C), M, Path.this(path)) of
                  SU as (Val.Equal (v, w), Univ.Prop) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
                       (Val.Cons (v1, v2), Val.Cons (w1, w2)) => 
			   (Val.Equal (v2, w2), Univ.Prop)
		     | (v', w') => raise Error (msg (SU)))
		| SU => raise Error (msg (SU))
	    end

	| infer' ((e as (i, e'), G, C), Exp.Ann (M, (S, U)), path) = 
            let 
		val VU = (Val.whnf (S, e'), U)
	    in
		check' ((e, G, C), M, VU, Path.left (path));
		VU
	    end

	| infer' ((e, G, C), M, path) = raise Error (msg (path, "Cannot infer the type of value "^Exp.termToString (M)^", please insert annotation"))

      and inferApp (((i, e), G, C), M, N, path) =
           (case infer' (((i, e), G, C), M, Path.left (path)) of
               (Val.Clos (Exp.Pi (Range.Dep (x), S, T), e'), U) => 
                 (check' (((i, e), G, C), N, (Val.whnf (S, e'), Univ.Type), Path.right (path)); 
                  (Val.whnf (T, Cxt.Ext (x, Val.eval (N, e), e')), U))
              |(Val.Clos (Exp.Pi (Range.Prop, S, T), e'), U) => 
                 (check' (((i, e), G, C), N, (Val.whnf (S, e'), Univ.Prop), Path.right (path)); 
                  (Val.whnf (T, e'), U))
              |(Val.Clos (Exp.Pi (Range.Type, S, T), e'), U) => 
                 (check' (((i, e), G, C), N, (Val.whnf (S, e'), Univ.Type), Path.right (path)); 
                  (Val.whnf (T, e'), U))
	      | SU => raise Error (inferFails (Path.left (path), M, Arrow, (i, e), SU)))

      (*  check' : ((int * Val.Env) * Exp.Cxt * _ Cxt) * 
                   Exp.Exp * Val.Val * Path.path -> unit
      *)
      and check' (((i, e), G, C), Exp.Pair (M, N), (Val.Clos (Exp.Sigma (Range.Dep (x), S, T), e'), U), path) =
           (check' (((i, e), G, C), M, (Val.whnf (S, e'), Univ.Type), Path.left (path));
	    check' (((i, e), G, C), N, (Val.whnf (T, Cxt.Ext (x, Val.eval (M, e), e')), U), Path.right (path)))
	| check' ((e, G, C), Exp.Pair (M, N), (Val.Clos (Exp.Sigma (Range.Type, S, T), e'), U), path) =
           (check' ((e, G, C), M, (Val.whnf (S, e'), Univ.Type), Path.left (path));
	    check' ((e, G, C), N, (Val.whnf (T, e'), U), Path.right (path)))
	| check' ((e, G, C), Exp.Pair (M, N), (Val.Clos (Exp.Sigma (Range.Prop, S, T), e'), U), path) =
           (check' ((e, G, C), M, (Val.whnf (S, e'), Univ.Prop), Path.left (path));
	    check' ((e, G, C), N, (Val.whnf (T, e'), U), Path.right (path)))
	| check' (((i, e), G, C), exp as Exp.Pair (M, N), SU, path) =
            raise Error (checkFails (path, exp, Prod, (i, e), SU))

	| check' ((e, G, C), Exp.Inl (M), (Val.Clos (Exp.Sum (S, T), e'), U), path) =
            check' ((e, G, C), M, (Val.whnf (S, e'), U), Path.this (path))
	| check' (((i, e), G, C), exp as Exp.Inl (M), SU, path) =
            raise Error (checkFails (path, exp, Sum, (i, e), SU))

	| check' ((e, G, C), Exp.Inr (M), (Val.Clos (Exp.Sum (S, T), e'), U), path) =
            check' ((e, G, C), M, (Val.whnf (T, e'), U), Path.this (path))
	| check' (((i, e), G, C), exp as Exp.Inr (M), SU, path) =
            raise Error (checkFails (path, exp, Sum, (i, e), SU))
       
	| check' ((e, G, C), Exp.Case (M, (x, N), (y, O)), SU', path) =
           (case infer' ((e, G, C), M, Path.left (path)) of
	       (Val.Clos (Exp.Sum (S, T), e'), U) => 
                (check' ((Val.gen (x, e), 
                          Cxt.Ext (x, (Val.eval (S, e'), U), G), 
                          C), N, SU', Path.middle (path));
		 check' ((Val.gen (y, e), 
                          Cxt.Ext (y, (Val.eval (T, e'), U), G), 
			  C), O, SU', Path.right (path)))
	     | (S, U) => raise Error (inferFails (Path.left (path), M, Sum, e, (S, U))))

	| check' (((i, e), G, C), Exp.Lam (x, M), (Val.Clos (Exp.Pi (Range.Dep (y), S, T), e'), U), path) =
	    check' ((Val.gen (x, (i, e)), 
		     Cxt.Ext (x, (Val.eval (S, e'), Univ.Type), G), 
		     C), M, (Val.whnf (T, Cxt.Ext (y, Val.Gen (i), e')), U), Path.this (path))
	| check' (((i, e), G, C), Exp.Lam (x, M), (Val.Clos (Exp.Pi (Range.Type, S, T), e'), U), path) =
	    check' ((Val.gen (x, (i, e)), 
		     Cxt.Ext (x, (Val.eval (S, e'), Univ.Type), G), 
		     C), M, (Val.whnf (T, e'), U), Path.this (path))
	| check' (((i, e), G, C), Exp.Lam (x, M), (Val.Clos (Exp.Pi (Range.Prop, S, T), e'), U), path) =
	    check' ((Val.gen (x, (i, e)), 
		     Cxt.Ext (x, (Val.eval (S, e'), Univ.Prop), G), 
		     C), M, (Val.whnf (T, e'), U), Path.this (path))
	| check' (((i, e), G, C), exp as Exp.Lam _, SU, path) =
            raise Error (checkFails (path, exp, Arrow, (i, e), SU))

	| check' ((e, G, C), Exp.Star, (Val.Clos (Exp.Unit, e'), U), path) = ()
	| check' (((i, e), G, C), exp as Exp.Star, SU, path) = 
            raise Error (checkFails (path, exp, Unit, (i, e), SU))

	| check' ((e, G, C), Exp.Abort (M), US', path) =
           (case infer' ((e, G, C), M, Path.this (path)) of
                (Val.Clos (Exp.Empty, e'), U) => ()
	      | (S, U) => raise Error (inferFails (Path.this (path), M, Empty, e, (S, U))))


	| check' ((e, G, C), Exp.Zero, (Val.Clos (Exp.Nat, e'), Univ.Type), path) = ()
	| check' (((i, e), G, C), exp as Exp.Zero, SU, path) = 
            raise Error (checkFails (path, exp, Nat, (i, e), SU))

	| check' ((e, G, C), Exp.Succ (N), SU as (Val.Clos (Exp.Nat, e'), Univ.Type), path) = 
            check' ((e, G, C), N, SU, Path.this (path))
	| check' (((i, e), G, C), exp as (Exp.Succ (N)), SU, path) = 
            raise Error (checkFails (path, exp, Nat, (i, e), SU))

          (* induction (dependent recursion) only over variable *)
	| check' (((i, e), G, C), exp as (Exp.RecNat (Exp.Var(n), f, N, (x, L))), (S, U), path) =
	   (check' (((i, e), G, C), Exp.Var (n), UVal.nat, Path.left (path));
            let 
                val (SOME j) = Val.genInEnv (n, e)
            in
	        check' (((i, e), G, C), N, (Val.instVal (j, Val.Zero, S), U), Path.middle (path));
	    let   
                val e' = Cxt.Ext (x, Val.Gen(i), e)
                val Sx = Val.instVal (j, Val.Gen (i), S)
		val S' = Val.instVal (j, Val.Succ (Val.Gen (i)), S)
		val G' = Cxt.Ext (x, UVal.nat, G) 
		val C' = Cxt.Ext (f, (G', (Sx, U)), C)    (* variable x lies on top of G' *)
	    in  
		check' (((i+1, e'), G', C'), L, (S', U), Path.right (path))
	    end end)
(*
	   (check' (((i, e), G, C), Exp.Var (n), UVal.nat, Path.left (path));
            let 
                val (SOME j) = Val.genInEnv (n, e)
		val e0 = Val.inst (n, Val.Zero, e)
            in
	        check' (((i, e0), G, C), N, (Val.instVal (n, Val.Zero, e, S), U), Path.middle (path))
            end;
	    let   
		val es = Val.inst (n, Val.Succ (Val.Gen (i)), e)
                val ee = Cxt.Ext (x, Val.Gen(i), es)
		val G' = Cxt.Ext (x, UVal.nat, G) 
		val C' = Cxt.Ext (f, (G', SU), C)    (* variable x lies on top of G' *)
	    in  
		check' (((i+1, es), G', C'), L, SU, Path.right (path))
	    end)
*)
          (* (independent) recursion over any term M *)
	| check' ((e, G, C), exp as (Exp.RecNat (M, f, N, (x, L))), SU, path) =
           (case infer' ((e, G, C), M, Path.left (path)) of
		(Val.Clos (Exp.Nat, _), Univ.Type) => 
                  (check' ((e, G, C), N, SU, Path.middle (path));
		   let
		       val e' = Val.gen (x, e) 
		       val G' = Cxt.Ext (x, UVal.nat, G) 
		       val C' = Cxt.Ext (f, (G', SU), C)    (* variable x lies on top of G' *)
		   in  
		       check' ((e', G', C'), L, SU, Path.right (path))
		   end)
	      | TU => raise Error (inferFails (path, exp, Nat, e, TU)))

	| check' ((e, G, C), Exp.True, (Val.Clos (Exp.Bool, _), Univ.Type), path) = ()
	| check' (((i, e), G, C), exp as Exp.True, SU, path) = 	    
            raise Error (checkFails (path, exp, Bool, (i, e), SU))

	| check' ((e, G, C), Exp.False, (Val.Clos (Exp.Bool, _), Univ.Type), path) = ()
	| check' (((i, e), G, C), exp as Exp.False, SU, path) = 	    
            raise Error (checkFails (path, exp, Bool, (i, e), SU))

	| check' ((e, G, C), Exp.If (M, N, O), TU, path) = 
           (case infer' ((e, G, C), M, Path.left (path)) of
               (Val.Clos (Exp.Bool, _), Univ.Type) => 
                  (check' ((e, G, C), N, TU, Path.middle (path));
		   check' ((e, G, C), O, TU, Path.right (path)))
	      | SU => raise Error (inferFails (path, M, Bool, e, SU)))


	| check' ((e, G, C), Exp.Nil, (Val.Clos (Exp.List(S), e'), Univ.Type), path) = ()
	| check' (((i, e), G, C), exp as Exp.Nil, SU, path) = 
            raise Error (checkFails (path, exp, List, (i, e), SU))


	| check' ((e, G, C), Exp.Cons (M, N), SU as (Val.Clos (Exp.List(S), e'), Univ.Type), path) = 
           (check' ((e, G, C), M, (Val.whnf (S, e'), Univ.Type), Path.left (path));
	    check' ((e, G, C), N, SU, Path.right (path)))
	| check' (((i, e), G, C), exp as (Exp.Cons (M, N)), SU, path) = 
            raise Error (checkFails (path, exp, List, (i, e), SU))

          (* induction (dependent recursion) only over variable *)
	| check' (((i, e), G, C), exp as (Exp.RecList (Exp.Var(n), f, N, (x, xs, L))), (S, U), path) =
	   (case infer' (((i, e), G, C), Exp.Var (n), Path.left (path)) of
              LU as (Val.Clos (Exp.List (T), eL), Univ.Type) =>
		  let 
		      val (SOME j) = Val.genInEnv (n, e)
		  in
		      check' (((i, e), G, C), N, (Val.instVal (j, Val.Nil, S), U), Path.middle (path));
		  let   
		      val e' = Cxt.Ext (xs, Val.Gen (i+1), Cxt.Ext (x, Val.Gen(i), e))
		      val Sxs = Val.instVal (j, Val.Gen (i+1), S)
		      val S' = Val.instVal (j, Val.Cons (Val.Gen (i), Val.Gen (i+1)), S)
		      val G' = Cxt.Ext (xs, LU, Cxt.Ext (x, (Val.eval (T, eL), Univ.Type), G))
		      val C' = Cxt.Ext (f, (G', (Sxs, U)), C)    (* variable x lies on top of G' *)
		  in  
		      check' (((i+1, e'), G', C'), L, (S', U), Path.right (path))
		  end end
	    | LU => raise Error (inferFails (Path.left (path), exp, List, (i, e), LU)))
          (* (non-dependent) recursion only over variable *)
	| check' (((i, e), G, C), exp as (Exp.RecList (M, f, N, (x, xs, L))), (S, U), path) =
	   (case infer' (((i, e), G, C), M, Path.left (path)) of
              LU as (Val.Clos (Exp.List (T), eL), Univ.Type) =>
		  (check' (((i, e), G, C), N, (S, U), Path.middle (path));
		  let   
		      val e' = Cxt.Ext (xs, Val.Gen (i+1), Cxt.Ext (x, Val.Gen(i), e))
		      val G' = Cxt.Ext (xs, LU, Cxt.Ext (x, (Val.eval (T, eL), Univ.Type), G))
		      val C' = Cxt.Ext (f, (G', (S, U)), C)    (* variable x lies on top of G' *)
		  in  
		      check' (((i+1, e'), G', C'), L, (S, U), Path.right (path))
		  end)
	    | LU => raise Error (inferFails (Path.left (path), exp, List, (i, e), LU))) 
(*
	| check' ((e, G, C), exp as (Exp.RecList (M, f, N, (x, xs, L))), S, path) =
           (case infer' ((e, G, C), M, Path.left (path)) of
		Exp.List(T) => (check' ((e, G, C), N, S, Path.middle (path));
				 let
				     val e' = Val.gen (xs, Val.gen (x, e))
				     val G' = Cxt.Ext (xs, Exp.List (T), Cxt.Ext (x, T, G))
				     val C' = Cxt.Ext (f, (G', S), C)
				 in 
				     check' ((e', G', C'), L, S, Path.right (path))
				 end)
	      | T => error (msg (Path.left (path), "Head "^Exp.termToString exp^
                " of primitive recursion is not of list type as expected, but "^Exp.typeToString T)))
*)
        | check' (((i, e), G, C), Exp.LetPair (M, (x, y, N)), SU', path) =
	   (case infer' (((i, e), G, C), M, Path.left (path)) of
	       (Val.Clos (Exp.Sigma (Range.Dep (z), A, B), e'), U) =>
                   let
		       val exy = Val.gen (y, Val.gen (x, (i, e)))
		       val G' = Cxt.Ext (y, (Val.eval (B, Cxt.Ext (z, Val.Gen (i), e')), U), 
				Cxt.Ext (x, (Val.eval (A, e'), Univ.Type), G))	 
		   in
		       check' ((exy, G', C), N, SU', Path.right (path))
		   end
	     | (Val.Clos (Exp.Sigma (Range.Type, A, B), e'), U) => 
                   let
		       val exy = Val.gen (y, Val.gen (x, (i, e)))
		       val G' = Cxt.Ext (y, (Val.eval (B, e'), U),
				Cxt.Ext (x, (Val.eval (A, e'), Univ.Type), G))
		   in
		       check' ((exy, G', C), N, SU', Path.right (path))
		   end
	     | (Val.Clos (Exp.Sigma (Range.Prop, A, B), e'), U) => 
                   let
		       val exy = Val.gen (y, Val.gen (x, (i, e)))
		       val G' = Cxt.Ext (y, (Val.eval (B, e'), U),
				Cxt.Ext (x, (Val.eval (A, e'), Univ.Prop), G))	 
		   in
		       check' ((exy, G', C), N, SU', Path.right (path))
		   end
	     | SU => raise Error (inferFails (Path.left (path), M, Sigma, (i, e), SU)))  

        | check' ((e as (i, _), G, C), exp as (Exp.Eq0), SU' as (Val.Equal (v, w), Univ.Prop), path) =
           (case (Val.rwhnf (v), Val.rwhnf (w)) of
              (Val.Zero, Val.Zero) => ()
	    | (v', w') => raise Error (checkFails (path, exp, EqNat, e, SU')))
        | check' ((e, G, C), exp as (Exp.Eq0), SU', path) = 
            raise Error (checkFails (path, exp, EqNat, e, SU'))

        | check' ((e as (i, _), G, C), exp as (Exp.EqS M), SU' as (Val.Equal (v, w), Univ.Prop), path) =
           (case (Val.rwhnf (v), Val.rwhnf (w)) of
             (Val.Succ (v'), Val.Succ (w')) => check' ((e, G, C), M, (Val.Equal (v', w'), Univ.Prop), Path.this (path))
	   | (v', w') => raise Error (checkFails (path, exp, EqNat, e, SU')))
        | check' ((e as (i, _), G, C), exp as (Exp.EqS M), SU', path) =
            raise Error (checkFails (path, exp, EqNat, e, SU'))

	| check' ((e, G, C), exp as (Exp.EqE0S (M)), US', path) =
           (case infer' ((e, G, C), M, Path.this (path)) of
		US as (Val.Equal (v, w), U) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
			(Val.Zero, Val.Succ(w')) => ()
		      | (v', w') => raise Error (inferFails (Path.this (path), M, EqNat, e, US)))
	      | US => raise Error (inferFails (Path.this (path), M, EqNat, e, US)))
	| check' ((e, G, C), exp as (Exp.EqES0 (M)), US', path) =
           (case infer' ((e, G, C), M, Path.this (path)) of
		US as (Val.Equal (v, w), U) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
			(Val.Succ(w'), Val.Zero) => ()
		      | (v', w') => raise Error (inferFails (Path.this (path), M, EqNat, e, US)))
	      | US => raise Error (inferFails (Path.this (path), M, EqNat, e, US)))
	| check' (((i, e), G, C), exp as (Exp.EqESS (M)), US', path) =
           (case infer' (((i, e), G, C), M, Path.this (path)) of
		US as (Val.Equal (v, w), U) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
			(Val.Succ (v'), Val.Succ(w')) => 
                          if UVal.conv (i, (Val.Equal (v', w'), U), US') then ()
			  else raise Error (inferFails (Path.this (path), M, EqNat, (i, e), US))
		      | (v', w') => raise Error (inferFails (Path.this (path), M, EqNat, (i, e), US)))
	      | US => raise Error (inferFails (Path.this (path), M, EqNat, (i, e), US)))


        | check' ((e as (i, _), G, C), exp as (Exp.Less0), SU' as (Val.Less (v, w), Univ.Prop), path) =
           (case (Val.rwhnf (v), Val.rwhnf (w)) of
              (Val.Zero, w') => ()
	    | (v', w') => raise Error (checkFails (path, exp, Less, e, SU')))
        | check' ((e, G, C), exp as (Exp.Less0), SU', path) = 
            raise Error (checkFails (path, exp, Less, e, SU'))

        | check' ((e as (i, _), G, C), exp as (Exp.LessS M), SU' as (Val.Less (v, w), Univ.Prop), path) =
           (case (Val.rwhnf (v), Val.rwhnf (w)) of
             (Val.Succ (v'), Val.Succ (w')) => check' ((e, G, C), M, (Val.Less (v', w'), Univ.Prop), Path.this (path))
	   | (v', w') => raise Error (checkFails (path, exp, Less, e, SU')))
        | check' ((e as (i, _), G, C), exp as (Exp.LessS M), SU', path) =
            raise Error (checkFails (path, exp, Less, e, SU'))

	| check' ((e, G, C), exp as (Exp.LessE0 (M)), US', path) =
           (case infer' ((e, G, C), M, Path.this (path)) of
		US as (Val.Less (v, w), U) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
			(w', Val.Zero) => ()
		      | (v', w') => raise Error (inferFails (Path.this (path), M, Less, e, US)))
	      | US => raise Error (inferFails (Path.this (path), M, EqNat, e, US)))
	| check' (((i, e), G, C), exp as (Exp.LessES (M)), US', path) =
           (case infer' (((i, e), G, C), M, Path.this (path)) of
		US as (Val.Less (v, w), U) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
			(Val.Succ (v'), Val.Succ(w')) => 
                          if UVal.conv (i, (Val.Less (v', w'), U), US')
			      then ()
			  else raise Error (inferFails (Path.this (path), M, Less, (i, e), US))
		      | (v', w') => raise Error (inferFails (Path.this (path), M, Less, (i, e), US)))
	      | US => raise Error (inferFails (Path.this (path), M, Less, (i, e), US)))


        | check' ((e as (i, _), G, C), exp as (Exp.EqN), SU' as (Val.Equal (v, w), Univ.Prop), path) =
           (case (Val.rwhnf (v), Val.rwhnf (w)) of
              (Val.Nil, Val.Nil) => ()
	    | (v', w') => raise Error (checkFails (path, exp, EqList, e, SU')))
        | check' ((e, G, C), exp as (Exp.EqN), SU', path) = 
            raise Error (checkFails (path, exp, EqList, e, SU'))

        | check' ((e as (i, _), G, C), exp as (Exp.EqC M), SU' as (Val.Equal (v, w), Univ.Prop), path) =
           (case (Val.rwhnf (v), Val.rwhnf (w)) of
             (Val.Cons (v1, v2), Val.Cons (w1, w2)) => 
		 if Val.conv (i, v1, w1) then
		     check' ((e, G, C), M, (Val.Equal (v2, w2), Univ.Prop), Path.this (path))
		 else raise Error (msg (path, Exp.termPretty (exp)^" does not prove "^UVal.pretty (e, SU')^", since heads of lists in equation are not identical"))
	   | (v', w') => raise Error (checkFails (path, exp, EqList, e, SU')))
        | check' ((e as (i, _), G, C), exp as (Exp.EqC M), SU', path) =
            raise Error (checkFails (path, exp, EqList, e, SU'))

	| check' ((e, G, C), exp as (Exp.EqENC (M)), US', path) =
           (case infer' ((e, G, C), M, Path.this (path)) of
		US as (Val.Equal (v, w), U) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
			(Val.Nil, Val.Cons(w1, w2)) => ()
		      | (v', w') => raise Error (inferFails (Path.this (path), M, EqList, e, US)))
	      | US => raise Error (inferFails (Path.this (path), M, EqList, e, US)))
	| check' ((e, G, C), exp as (Exp.EqECN (M)), US', path) =
           (case infer' ((e, G, C), M, Path.this (path)) of
		US as (Val.Equal (v, w), U) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
			(Val.Cons(w1, w2), Val.Nil) => ()
		      | (v', w') => raise Error (inferFails (Path.this (path), M, EqList, e, US)))
	      | US => raise Error (inferFails (Path.this (path), M, EqList, e, US)))
	| check' (((i, e), G, C), exp as (Exp.EqECC (M)), US', path) =
           (case infer' (((i, e), G, C), M, Path.this (path)) of
		US as (Val.Equal (v, w), U) => 
                   (case (Val.rwhnf (v), Val.rwhnf (w)) of
			(Val.Cons (v1, v2), Val.Cons(w1, w2)) => 
                          if UVal.conv (i, (Val.Equal (v2, w2), U), US') then ()
			  else raise Error (inferFails (Path.this (path), M, EqList, (i, e), US))
		      | (v', w') => raise Error (inferFails (Path.this (path), M, EqList, (i, e), US)))
	      | US => raise Error (inferFails (Path.this (path), M, EqList, (i, e), US)))


	| check' ((e as (i, e'), G, C), Exp.Ann (M, (S, U)), SU', path) =
	    if UVal.eqExpVal (e, (S, U), SU') then 
		check' ((e, G, C), M, (Val.whnf (S, e'), U), Path.left (path))
	    else raise Error (msg (Path.right (path), "Annotated type "^UExp.toString (S, U)^" is different from expected type "^UVal.pretty (e, SU')))

	| check' (((i, e), G, C), M, SU', path) = 
	    let val SU = infer' (((i, e), G, C), M, path)
            in 
		if UVal.conv (i, SU, SU') then  ()
	        else raise Error (msg (path, "Type mismatch: "^Exp.termToString M^
                  " is of type "^UVal.pretty ((i, e), SU)^
                  ", but in this place something of type "^UVal.pretty ((i, e), SU')^
                  " is required."))
	    end
      
      val M = Syntax.getTerm (Mm)
      val (S, U) = Syntax.getUExp (Ss)

  in 
      check' ((Val.empty(), globG, Cxt.Empty), M, 
              (Val.whnf (S, Cxt.Empty), U), Path.top)
  end

  fun checkExp (globG, Mm, Ss) =
    let
        val M = Syntax.getTerm (Mm)
        val (S, U) = Syntax.getUExp (Ss)

    in
       check (globG, Mm, Ss);
	if Chatter.declDetails() then print ("|- "^Exp.termPretty M^"\n : "
             ^ UExp.pretty (S, U) ^ "\n") else ();
	if Chatter.finalResults() then print (case U of
            Univ.Type => "OK\n"
	  | Univ.Prop => "QED\n") 
        else(); 
        Global.exitOK
    end handle Error (msg) => (print (msg ^ "\n"); Global.exitTermCheck)
         | Val.Error (msg) =>  (print (msg ^ "\n"); Global.exitTermCheck) 

end (* structure CheckDep *)



