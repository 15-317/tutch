(* $Id: val.sml,v 1.4 2000/10/27 17:08:14 abel Exp $ *)

signature VAL =
sig

  datatype Val =
      Gen of int         (* v_i,  generic value for stepping under lambda *)
    | Def of int*Val     (* notational definition/let, only expanded by need *)
    | Clos of Exp.Exp * Env                 (* <t;e> *)

    | Pair of Val * Val
    | Fst of Val                        (* fst v *)
    | Snd of Val                        (* snd v *)
    | Inl of Val
    | Inr of Val
    | Case of Val * (string * Exp.Exp) * (string * Exp.Exp) * Env
    | Lam of string * Exp.Exp * Env 
    | App of Val * Val                  (* v w   *)
    | Star 
    | Abort of Val
    | Zero
    | Succ of Val
    | RecNat of Val * string * Exp.Exp * (string * Exp.Exp) * Env
                         (* <rec v of f 0 => s | f (s x') => t end;e> *)
    | True
    | False
    | If of Val * Exp.Exp * Exp.Exp * Env       (* <if v then s else t;e> *)
    | Nil
    | Cons of Val * Val
    | RecList of Val * string * Exp.Exp * (string * string * Exp.Exp) * Env
                         (* <rec v of f nil => s | f (x :: xs) => t end;e> *)

    | LetPair of Val * (string * string * Exp.Exp) * Env
                         (* <let (x,y) = v in t;e> *)
(*
    | Eq0
    | EqS of Val
    | EqE0S of Val
    | EqES0 of Val
    | EqESS of Val
    | Less0
    | LessS of Val
    | LessE0 of Val
    | LessES of Val
    | EqN 
    | EqC of Val
    | EqENC of Val
    | EqECN of Val
    | EqECC of Val
*)
    | Equal of Val * Val
    | Less of Val * Val
 
  withtype Env = Val Cxt.Cxt

  (* Environments *)
  type env = int * Env

  val empty : unit -> env
  val ext : string * Val * env -> env
  val gen : string * env -> env

  val extGlob : string * Exp.Exp -> unit
  val lookup : string * Env -> Val option (* lookup in given and global env *)

(*
  val inst : string * Val * Env -> Env
*)
  val genInEnv : string * Env -> int option
  val instVal : int * Val * Val -> Val
  val instEnv : int * Val * Env -> Env
(* not public
  val toNames : Env -> Names.names
*)
  (* Evaluation:  evaluate as much as possible, not under lambda *)
  val eval : Exp.Exp * Env -> Val
  val reval : Val -> Val    

  (* Weak Head Normal Form:  evaluate as less as possible, whnf *)
  val whnf : Exp.Exp * Env -> Val
  val rwhnf : Val -> Val    

  (* Equality on Val and Exp.Exp *)
  val conv : int * Val * Val -> bool     (* test two whnfs for equality *)
(*
  val eqExp  : Exp.Exp * Exp.Exp -> bool
*)
  val eqExp' : env * Exp.Exp * Exp.Exp -> bool
  val eqExp2 : int * Exp.Exp * Env * Exp.Exp * Env -> bool
  val eqExpVal : env * Exp.Exp * Val -> bool
  (* val eq: Val * Val -> bool  always need number of next generic value*)
(*
  val split : int * Val * Val -> (Val -> Val) Stream.stream  (* reverse substitution *)
*)
  (*  eqSubst (e, C, M, x, A) = true  iff C == [M/x]A in env e *)
  val eqSubst: env * Exp.Exp * Exp.Exp * string * Exp.Exp -> bool

  (*  eqSubstRec (e, C, M, x, A) = true  iff C(x) == [M(x)/x]A in env e *)
  val eqSubstRec: env * Exp.Exp * Exp.Exp * string * Exp.Exp -> bool

  (*  eqSubstVar (e, C, y, x, A) = true  iff C(y) == [y/x]A in env e *)
  val eqSubstVar: env * Exp.Exp * string * string * Exp.Exp -> bool

  (*  env carries all names *)
  val toExp : env * Val -> Exp.Exp
  val toString : env * Val -> string

  (*  some type constants *)
  val nat : Val
  val bool : Val
  

  exception Error of string

end (* signature VAL *)


structure Val :> VAL =
struct

  datatype Val =
      Gen of int         (* v_i,  generic value for stepping under lambda *)
    | Def of int*Val     (* notational definition/let, only expanded by need *)
    | Clos of Exp.Exp * Env                 (* <t;e> *)

    | Pair of Val * Val
    | Fst of Val                        (* fst v *)
    | Snd of Val                        (* snd v *)
    | Inl of Val
    | Inr of Val
    | Case of Val * (string * Exp.Exp) * (string * Exp.Exp) * Env
    | Lam of string * Exp.Exp * Env 
    | App of Val * Val                  (* v w   *)
    | Star 
    | Abort of Val
    | Zero
    | Succ of Val
    | RecNat of Val * string * Exp.Exp * (string * Exp.Exp) * Env
                         (* <rec v of f 0 => s | f (s x') => t end;e> *)
    | True
    | False
    | If of Val * Exp.Exp * Exp.Exp * Env       (* <if v then s else t;e> *)
    | Nil
    | Cons of Val * Val
    | RecList of Val * string * Exp.Exp * (string * string * Exp.Exp) * Env
                         (* <rec v of f nil => s | f (x :: xs) => t end;e> *)

    | LetPair of Val * (string * string * Exp.Exp) * Env
                         (* <let (x,y) = v in t;e> *)
(*  
    | Eq0
    | EqS of Val
    | EqE0S of Val
    | EqES0 of Val
    | EqESS of Val
    | Less0
    | LessS of Val
    | LessE0 of Val
    | LessES of Val
    | EqN 
    | EqC of Val
    | EqENC of Val
    | EqECN of Val
    | EqECC of Val
*)
    | Equal of Val * Val
    | Less of Val * Val
 
  withtype Env = Val Cxt.Cxt

  exception Error of string


  type env = int * Env

  val globE = ref (0, Cxt.Empty: Env)
  val globN = ref Names.empty

  fun empty() =  (* new local env *)
      let 
	  val (i, e) = !globE 
      in 
	  (i, Cxt.Empty)
      end
  fun ext (x, v, (i, e)) = (i, Cxt.Ext (x, v, e))
  fun gen (x, (i, e)) = (i+1, Cxt.Ext (x, Gen (i), e))

  fun lookup (x, e) = (case Cxt.lookup (x, e) of
           NONE => Cxt.lookup (x, #2(!globE))
	 | SOME (v) => SOME (v)) 


  (* handling of Def not yet fully clear *)

  fun evFst (Pair (v, w)) = v
    | evFst (v) = Fst v 
  fun evSnd (Pair (v, w)) = w
    | evSnd (v) = Snd v 

  fun evCase (Inl (v), (x, s), (y, t), e) = eval (s, Cxt.Ext (x, v, e))
    | evCase (Inr (v), (x, s), (y, t), e) = eval (t, Cxt.Ext (y, v, e))
    | evCase (v, (x, s), (y, t), e) = Case (v, (x, s), (y, t), e)

  and evApp (Lam (x, t, e), v) = eval (t, Cxt.Ext (x, v, e))
    | evApp (w, v) = App (w, v)

  and evRecNat (v, f, s, (x, t), e) =
  let val fx = f^" "^x
      val t' = Exp.convRec (f, x, t)
      fun rNat (Zero) = eval (s, e)
	| rNat (Succ(v)) = 
            let val r = rNat (v)
	    in eval (t', Cxt.Ext (fx, r, Cxt.Ext (x, v, e))) end
        | rNat (v) = RecNat (v, f, s, (x, t), e)
  in rNat (v) end

  and evIf (True, s, t, e) = eval (s, e)
    | evIf (False, s, t, e) = eval (t, e)
    | evIf (v, s, t, e) = If (v, s, t, e)

  and evRecList (v, f, s, (x, xs, t), e) =
  let val fxs = f^" "^xs
      val t' = Exp.convRec (f, xs, t)
      fun rList (Nil) = eval (s, e)
	| rList (Cons (v, vs)) = 
            let val r = rList (vs)
	    in eval (t', Cxt.Ext (fxs, r, Cxt.Ext (xs, vs, Cxt.Ext (x, v, e)))) end
        | rList (v) = RecList (v, f, s, (x, xs, t), e)
  in rList (v) end
  
  and evLetPair (Pair (v, w), (x, y, t), e) = 
        eval (t, Cxt.Ext (y, w, Cxt.Ext (x, v, e)))
    | evLetPair (v, (x, y, t), e) = LetPair (v, (x, y, t), e) 

  and eval (Exp.Var(x), e) = (case lookup (x, e) of
          NONE => raise Error ("Variable "^x^" not bound in context")
	| SOME (v) => v) 
    | eval (Exp.Pair (s, t), e) = Pair (eval (s, e), eval (t, e))
    | eval (Exp.Fst (t), e) = evFst (eval (t, e))
    | eval (Exp.Snd (t), e) = evSnd (eval (t, e))
    | eval (Exp.Inl (t), e) = Inl (eval (t, e))
    | eval (Exp.Inr (t), e) = Inr (eval (t, e))
    | eval (Exp.Case (r, (x, s), (y, t)), e) = 
        evCase (eval (r, e), (x, s), (y, t), e)
    | eval (Exp.Lam (x, t), e) = Lam (x, t, e)
    | eval (Exp.App (s, t), e) = evApp (eval (s, e), eval (t, e))
    | eval (Exp.Star, e) = Star
    | eval (Exp.Abort (t), e) = Abort (Clos (t, e)) (* never evaluate abort *)
    | eval (Exp.Zero, e) = Zero
    | eval (Exp.Succ (t), e) = Succ (eval (t, e))
    | eval (Exp.RecNat (r, f, s, (x, t)), e) = 
        evRecNat (eval (r, e), f, s, (x, t), e)
    | eval (Exp.True, e) = True
    | eval (Exp.False, e) = False
    | eval (Exp.If (r, s, t), e) = evIf (eval (r, e), s, t, e)
    | eval (Exp.Nil, e) = Nil
    | eval (Exp.Cons (s, t), e) = Cons (eval (s, e), eval (t, e))
    | eval (Exp.RecList (r, f, s, (x, xs, t)), e) = 
        evRecList (eval (r, e), f, s, (x, xs, t), e)
    | eval (Exp.LetPair (s, (x, y, t)), e) = evLetPair (eval (s, e), (x, y, t), e)
    | eval (Exp.Equal (s, t), e) = Equal (eval (s, e), eval (t, e))
    | eval (Exp.Less (s, t), e) = Less (eval (s, e), eval (t, e))
    | eval (Exp.Not (A), e) = eval (Exp.notDef (A), e)
    | eval (Exp.Equiv (A, B), e) = eval (Exp.equivDef (A, B), e)
    | eval (A, e) = Clos (A, e)

  fun reval (Clos (t,e)) = eval (t, e)
    | reval (Gen (i))    = Gen(i)
    | reval (Def (i, v)) = reval v

    | reval (Pair (v, w)) = Pair (reval v, reval w)
    | reval (Fst (v))   = evFst (reval v)
    | reval (Snd (v))   = evSnd (reval v)
    | reval (Inl (v))   = Inl (reval v)
    | reval (Inr (v))   = Inr (reval v)
    | reval (Case (v, (x, s), (y, t), e)) = evCase (reval v, (x, s), (y, t), e)
    | reval (Lam (x, t, e)) = Lam (x, t, e)
    | reval (App (v, w)) = evApp (reval v, reval w) 
    | reval (Star) = Star
    | reval (Abort v) = Abort (v)
    | reval (Zero) = Zero
    | reval (Succ (v)) = Succ (reval v)
    | reval (RecNat (v, f, s, (x, t), e)) = evRecNat (reval v, f, s, (x, t), e)
    | reval (True) = True
    | reval (False) = False
    | reval (If (v, s, t, e)) = evIf (reval v, s, t, e)
    | reval (Nil) = Nil
    | reval (Cons (v, vs)) = Cons (reval v, reval vs)
    | reval (RecList (v, f, s, (x, xs, t), e)) = 
        evRecList (reval v, f, s, (x, xs, t), e)
    | reval (LetPair (v, (x, y, t), e)) = evLetPair (reval v, (x, y, t), e)
    | reval (Equal (v, w)) = Equal (reval v, reval w)
    | reval (Less (v, w)) = Less (reval v, reval w)

  fun whnfFst (Pair (v, w)) = rwhnf v
    | whnfFst (Def (i, v)) = whnfFst (v)
    | whnfFst (v) = Fst v 
  and whnfSnd (Pair (v, w)) = rwhnf w
    | whnfSnd (Def (i, v)) = whnfSnd (v)
    | whnfSnd (v) = Snd v 

  and whnfCase (Inl (v), (x, s), (y, t), e) = whnf (s, Cxt.Ext (x, reval v, e))
    | whnfCase (Inr (v), (x, s), (y, t), e) = whnf (t, Cxt.Ext (y, reval v, e))
    | whnfCase (Def (i, v), (x, s), (y, t), e) = whnfCase (v, (x, s), (y, t), e)
    | whnfCase (v, (x, s), (y, t), e) = Case (v, (x, s), (y, t), e)


  (* evaluate arguments fully before putting them in environment *)
  and whnfApp (Lam (x, t, e'), s, e) = 
        whnf (t, Cxt.Ext (x, eval (s, e), e'))  
    | whnfApp (Def (i, v), t, e) = whnfApp (v, t, e)
    | whnfApp (w, t, e) = App (w, Clos (t, e))

  and whnfApp' (Lam (x, t, e'), w) = 
        whnf (t, Cxt.Ext (x, reval w, e'))  
    | whnfApp' (Def (i, v), w) = whnfApp' (v, w)
    | whnfApp' (v, w) = App (v, w)

  and whnfRecNat (v, f, s, (x, t), e) =
  let val fx = f^" "^x
      val t' = Exp.convRec (f, x, t)
      fun rNat (Zero) = whnf (s, e)
	| rNat (Succ (n)) = 
	    let val v = reval (n)
                val r = evRecNat (v, f, s, (x, t), e)
	    in whnf (t', Cxt.Ext (fx, r, Cxt.Ext (x, v, e))) end
	| rNat (Def (i, v)) = rNat (rwhnf v)
        | rNat (v) = RecNat (v, f, s, (x, t), e)
  in rNat (v) end

  and whnfIf (True, s, t, e) = whnf (s, e)
    | whnfIf (False, s, t, e) = whnf (t, e)
    | whnfIf (Def (i, v), s, t, e) = whnfIf (v, s, t, e)
    | whnfIf (v, s, t, e) = If (v, s, t, e)

  and whnfRecList (v, f, s, (x, xs, t), e) =
  let val fxs = f^" "^xs
      val t' = Exp.convRec (f, xs, t)
      fun rList (Nil) = whnf (s, e)
	| rList (Cons (w, ws)) = 
            let val v = reval w
		val vs = reval ws
                val r = evRecList (vs, f, s, (x, xs, t), e)
	    in whnf (t', Cxt.Ext (fxs, r, Cxt.Ext (xs, vs, Cxt.Ext (x, v, e)))) end
	| rList (Def (i, v)) = rList (v)
        | rList (v) = RecList (v, f, s, (x, xs, t), e)
  in rList (v) end

  and whnfLetPair (Pair (v, w), (x, y, t), e) = 
        whnf (t, Cxt.Ext (y, reval (w), Cxt.Ext (x, reval (v), e)))
    | whnfLetPair (v, (x, y, t), e) = LetPair (v, (x, y, t), e) 

  (* handle only destructors *)

  and whnf (Exp.Var(x), e) = (case lookup (x, e) of
          NONE => raise Error ("Variable "^x^" not bound in context")
	| SOME (v) => v) 
    | whnf (Exp.Pair (s, t), e) = Pair (Clos (s, e), Clos (t, e))
    | whnf (Exp.Fst (t), e) = whnfFst (whnf (t, e))
    | whnf (Exp.Snd (t), e) = whnfSnd (whnf (t, e))
    | whnf (Exp.Inl (t), e) = Inl (Clos (t, e))
    | whnf (Exp.Inr (t), e) = Inr (Clos (t, e))
    | whnf (Exp.Case (r, (x, s), (y, t)), e) = 
        whnfCase (whnf (r, e), (x, s), (y, t), e)
    | whnf (Exp.Lam (x, t), e) = Lam (x, t, e)
    | whnf (Exp.App (s, t), e) = whnfApp (whnf (s, e), t, e)
    | whnf (Exp.Star, e) = Star
    | whnf (Exp.Abort (t), e) = Abort (Clos (t, e)) (* never evaluate abort *)
    | whnf (Exp.Zero, e) = Zero
    | whnf (Exp.Succ (t), e) = Succ (Clos (t, e))
    | whnf (Exp.RecNat (r, f, s, (x, t)), e) = 
        whnfRecNat (whnf (r, e), f, s, (x, t), e)
    | whnf (Exp.True, e) = True
    | whnf (Exp.False, e) = False
    | whnf (Exp.If (r, s, t), e) = whnfIf (whnf (r, e), s, t, e)
    | whnf (Exp.Nil, e) = Nil
    | whnf (Exp.Cons (s, t), e) = Cons (Clos (s, e), Clos (t, e))
    | whnf (Exp.RecList (r, f, s, (x, xs, t)), e) = 
        whnfRecList (whnf (r, e), f, s, (x, xs, t), e)
    | whnf (Exp.LetPair (s, (x, y, t)), e) = whnfLetPair (whnf (s, e), (x, y, t), e)
    | whnf (Exp.Equal (s, t), e) = Equal (Clos (s, e), Clos (t, e))
    | whnf (Exp.Less (s, t), e) = Less (Clos (s, e), Clos (t, e))
    | whnf (Exp.Not (A), e) = whnf (Exp.notDef (A), e)
    | whnf (Exp.Equiv (A, B), e) = whnf (Exp.equivDef (A, B), e)
    | whnf (T, e) = Clos (T, e)

  and rwhnf (Clos (t,e)) = whnf (t, e)
    | rwhnf (Gen (i))    = Gen (i)
    | rwhnf (Def (i, v)) = Def (i, v)

    | rwhnf (Pair (v, w)) = Pair (v, w)
    | rwhnf (Fst (v))   = whnfFst (rwhnf v)
    | rwhnf (Snd (v))   = whnfSnd (rwhnf v)
    | rwhnf (Inl (v))   = Inl (v)
    | rwhnf (Inr (v))   = Inr (v)
    | rwhnf (Case (v, (x, s), (y, t), e)) = whnfCase (rwhnf v, (x, s), (y, t), e)
    | rwhnf (Lam (x, t, e)) = Lam (x, t, e)
    | rwhnf (App (v, w)) = whnfApp' (rwhnf v, rwhnf w) 
    | rwhnf (Star) = Star
    | rwhnf (Abort (v)) = Abort (v)
    | rwhnf (Zero) = Zero
    | rwhnf (Succ (v)) = Succ (v)
    | rwhnf (RecNat (v, f, s, (x, t), e)) = whnfRecNat (rwhnf v, f, s, (x, t), e)
    | rwhnf (True) = True
    | rwhnf (False) = False
    | rwhnf (If (v, s, t, e)) = whnfIf (rwhnf v, s, t, e)
    | rwhnf (Nil) = Nil
    | rwhnf (Cons (v, vs)) = Cons (v, vs)
    | rwhnf (RecList (v, f, s, (x, xs, t), e)) = 
        whnfRecList (rwhnf v, f, s, (x, xs, t), e)
    | rwhnf (LetPair (v, (x, y, t), e)) = whnfLetPair (rwhnf v, (x, y, t), e)
    | rwhnf (Equal (v, w)) = Equal (v, w)
    | rwhnf (Less (v, w)) = Less (v, w)


  and instVal (i, u, Clos (t,e)) = eval (t, instEnv (i, u, e))
    | instVal (i, u, Gen (j))    = Gen(j)
    | instVal (i, u, Def (j, v)) = instVal (i, u, v)

    | instVal (i, u, Pair (v, w)) = Pair (instVal (i, u, v), instVal (i, u, w))
    | instVal (i, u, Fst (v))   = evFst (instVal (i, u, v))
    | instVal (i, u, Snd (v))   = evSnd (instVal (i, u, v))
    | instVal (i, u, Inl (v))   = Inl (instVal (i, u, v))
    | instVal (i, u, Inr (v))   = Inr (instVal (i, u, v))
    | instVal (i, u, Case (v, (x, s), (y, t), e)) = evCase (instVal (i, u, v), (x, s), (y, t), instEnv (i, u, e))
    | instVal (i, u, Lam (x, t, e)) = Lam (x, t, instEnv (i, u, e))
    | instVal (i, u, App (v, w)) = evApp (instVal (i, u, v), instVal (i, u, w)) 
    | instVal (i, u, Star) = Star
    | instVal (i, u, Abort v) = Abort (v)
    | instVal (i, u, Zero) = Zero
    | instVal (i, u, Succ (v)) = Succ (instVal (i, u, v))
    | instVal (i, u, RecNat (v, f, s, (x, t), e)) = evRecNat (instVal (i, u, v), f, s, (x, t), instEnv (i, u, e))
    | instVal (i, u, True) = True
    | instVal (i, u, False) = False
    | instVal (i, u, If (v, s, t, e)) = evIf (instVal (i, u, v), s, t, instEnv (i, u, e))
    | instVal (i, u, Nil) = Nil
    | instVal (i, u, Cons (v, vs)) = Cons (instVal (i, u, v), instVal (i, u, vs))
    | instVal (i, u, RecList (v, f, s, (x, xs, t), e)) = 
        evRecList (instVal (i, u, v), f, s, (x, xs, t), instEnv (i, u, e))
    | instVal (i, u, LetPair (v, (x, y, t), e)) = LetPair (instVal (i, u, v), (x, y, t), instEnv (i, u, e))
    | instVal (i, u, Equal (v, w)) = Equal (instVal (i, u, v), instVal (i, u, w))
    | instVal (i, u, Less (v, w)) = Less (instVal (i, u, v), instVal (i, u, w))

  and instEnv (i, u, Cxt.Empty) = Cxt.Empty
    | instEnv (i, u, Cxt.Ext (x, Gen(j), e)) = 
        if i=j then Cxt.Ext (x, u, instEnv (i, u, e))
        else Cxt.Ext (x, Gen(j), instEnv (i, u, e))
    | instEnv (i, u, Cxt.Ext (x, v, e)) = Cxt.Ext (x, instVal (i, u, v), instEnv (i, u, e))
  
  fun genInEnv (x, Cxt.Empty) = NONE
    | genInEnv (x, Cxt.Ext (y, Gen(i), e')) = if x=y then SOME (i)
						else genInEnv (x, e')
    | genInEnv (x, Cxt.Ext (y, v, e')) = genInEnv (x, e')
(*
  and inst (x, u, e) =
      let fun findx (Cxt.Empty) = e
	    | findx (Cxt.Ext (y, Gen(i), e')) = if x=y then instEnv (i, reval u, e)
						else findx (e')
	    | findx (Cxt.Ext (y, v, e')) = findx e'
      in findx (e) end

  and instVal (x, u, e, w) =
      let fun findx (Cxt.Empty) = e
	    | findx (Cxt.Ext (y, Gen(i), e')) = if x=y then instVal(i, reval u, w)
						else findx (e')
	    | findx (Cxt.Ext (y, v, e')) = findx e'
      in findx (e) end
*)

  fun extGlob (x, M) = 
      let val (i, e) = !globE
      in 
          globE := (i+1, Cxt.Ext (x, eval (M, e), e));
          globN := Names.add (i, x, !globN)
      end

(* Term conversion
**
** conv : int * Value * Value -> bool
**
** conv (i, v, w) checks whether values v and w are alpha-beta-convertible
** i denotes the next free index for a generic value
** Uses eval.
**
** Invariants in conv (i, v, w)
** - all generic values in v and w carry an index <= i
*)

fun conv (i, v, w) = conv' (i, rwhnf v, rwhnf w)

and conv' (i, Gen(j), Gen(k)) = j=k

  | conv' (i, Def(j,v), w) = conv (i, v, w)
  | conv' (i, v, Def(j,w)) = conv (i, v, w)

  | conv' (i, Clos (Exp.Pi (Range.Prop, A, B), e), 
              Clos (Exp.Pi (Range.Prop, A', B'), e')) =
      conv (i, Clos (A, e), Clos (A', e')) andalso
      conv (i, Clos (B, e), Clos (B', e'))
  | conv' (i, Clos (Exp.Pi (Range.Type, A, B), e), 
              Clos (Exp.Pi (Range.Type, A', B'), e')) =
      conv (i, Clos (A, e), Clos (A', e')) andalso
      conv (i, Clos (B, e), Clos (B', e'))
  | conv' (i, Clos (Exp.Pi (Range.Dep x, A, B), e), 
              Clos (Exp.Pi (Range.Dep x', A', B'), e')) =
      conv (i, Clos (A, e), Clos (A', e')) andalso
      conv (i+1, Clos (B, Cxt.Ext (x, Gen (i), e)), 
                 Clos (B', Cxt.Ext (x', Gen (i), e')))

  | conv' (i, Clos (Exp.Sigma (Range.Prop, A, B), e), 
              Clos (Exp.Sigma (Range.Prop, A', B'), e')) =
      conv (i, Clos (A, e), Clos (A', e')) andalso
      conv (i, Clos (B, e), Clos (B', e'))
  | conv' (i, Clos (Exp.Sigma (Range.Type, A, B), e), 
              Clos (Exp.Sigma (Range.Type, A', B'), e')) =
      conv (i, Clos (A, e), Clos (A', e')) andalso
      conv (i, Clos (B, e), Clos (B', e'))
  | conv' (i, Clos (Exp.Sigma (Range.Dep x, A, B), e), 
              Clos (Exp.Sigma (Range.Dep x', A', B'), e')) =
      conv (i, Clos (A, e), Clos (A', e')) andalso
      conv (i+1, Clos (B, Cxt.Ext (x, Gen (i), e)), 
                 Clos (B', Cxt.Ext (x', Gen (i), e')))

  | conv' (i, Clos (Exp.Sum (A, B), e), Clos (Exp.Sum (A', B'), e')) =
      conv (i, Clos (A, e), Clos (A', e')) andalso
      conv (i, Clos (B, e), Clos (B', e'))

  | conv' (i, Clos (Exp.Atom A, e), Clos (Exp.Atom A', e')) = A = A'
  | conv' (i, Clos (Exp.Unit, e), Clos (Exp.Unit, e')) = true
  | conv' (i, Clos (Exp.Empty, e), Clos (Exp.Empty, e')) = true
  | conv' (i, Clos (Exp.Nat, e), Clos (Exp.Nat, e')) = true
  | conv' (i, Clos (Exp.Bool, e), Clos (Exp.Bool, e')) = true
  | conv' (i, Clos (Exp.List (T), e), Clos (Exp.List (T'), e')) = 
      conv (i, Clos (T, e), Clos (T', e'))
(*
  | conv' (i, Clos (Exp.Equal (M, N), e), Clos (Exp.Equal (M', N'), e')) =
      conv (i, Clos (M, e), Clos (M', e')) andalso
      conv (i, Clos (N, e), Clos (N', e')) 
  | conv' (i, Clos (Exp.Less (M, N), e), Clos (Exp.Less (M', N'), e')) =
      conv (i, Clos (M, e), Clos (M', e')) andalso
      conv (i, Clos (N, e), Clos (N', e')) 
*)
  | conv' (i, Equal (v, w), Equal (v', w')) =
      conv (i, v, v') andalso conv (i, w, w')
  | conv' (i, Less (v, w), Less (v', w')) =
      conv (i, v, v') andalso conv (i, w, w')

  | conv' (i, Clos (Exp.Not (A), e), w) = conv (i, Clos (Exp.notDef (A), e), w)
  | conv' (i, v, Clos (Exp.Not (A), e)) = conv (i, v, Clos (Exp.notDef (A), e))
  | conv' (i, Clos (Exp.Equiv (A, B), e), w) = 
      conv (i, Clos (Exp.equivDef (A, B), e), w)
  | conv' (i, v, Clos (Exp.Equiv (A, B), e)) = 
      conv (i, v, Clos (Exp.equivDef (A, B), e))

  | conv' (i, Pair (v, w), Pair (v', w')) = 
      conv (i, v, v') andalso conv (i, w, w')
  | conv' (i, Fst (v), Fst (w)) = conv (i, v, w)
  | conv' (i, Snd (v), Snd (w)) = conv (i, v, w)

  | conv' (i, Inl (v), Inl (w)) = conv (i, v, w)
  | conv' (i, Inr (v), Inr (w)) = conv (i, v, w)
  | conv' (i, Case (v, (x, s), (y, t), e), Case (v', (x', s'), (y', t'), e')) =
      conv (i, v, v') andalso
      conv' (i+1, whnf (s, Cxt.Ext (x, Gen (i), e)),
                  whnf (s', Cxt.Ext (x', Gen (i), e'))) andalso
      conv' (i+1, whnf (t, Cxt.Ext (y, Gen (i), e)),
                  whnf (t', Cxt.Ext (y', Gen (i), e'))) 

  | conv' (i, Lam (x, t, e), Lam (x', t', e')) =
      conv' (i+1, whnf (t, Cxt.Ext (x, Gen (i), e)), 
                  whnf (t', Cxt.Ext (x', Gen (i), e')))
  | conv' (i, App (v, w), App (v', w')) = 
      conv (i, v, v') andalso conv (i, w, w')

  | conv' (i, Star, Star) = true
  | conv' (i, Abort (v), Abort (v')) = conv (i, v, v')

  | conv' (i, Zero, Zero) = true
  | conv' (i, Succ (v), Succ (v')) = conv (i, v, v')
  | conv' (i, RecNat (v, f, s, (x, t), e), RecNat (v', f', s', (x', t'), e')) =
      conv (i, v, v') andalso
      conv' (i, whnf (s, e), whnf (s', e')) andalso
      conv' (i+2, whnf (t, Cxt.Ext (f, Gen(i+1), Cxt.Ext (x, Gen (i), e))),
                  whnf (t', Cxt.Ext (f', Gen(i+1), Cxt.Ext (x', Gen (i), e'))))

  | conv' (i, True, True) = true
  | conv' (i, False, False) = true
  | conv' (i, If (v, s, t, e), If (v', s', t', e')) = 
     conv (i, v, v') andalso
     conv' (i, whnf (s, e), whnf (s', e')) andalso
     conv' (i, whnf (t, e), whnf (t', e'))

  | conv' (i, Nil, Nil) = true
  | conv' (i, Cons (v, vs), Cons (v', vs')) = 
      conv (i, v, v') andalso conv (i, vs, vs')
  | conv' (i, RecList (v, f, s, (x, xs, t), e), RecList (v', f', s', (x', xs', t'), e')) =
      conv (i, v, v') andalso
      conv' (i, whnf (s, e), whnf (s', e')) andalso
      conv' (i+3, whnf (t, Cxt.Ext (f, Gen (i+2), Cxt.Ext (xs, Gen (i+1), Cxt.Ext (x, Gen (i), e)))),
                 whnf (t', Cxt.Ext (f', Gen (i+2), Cxt.Ext (xs', Gen (i+1), Cxt.Ext (x', Gen (i), e')))))
  | conv' (i, LetPair (v, (x, y, t), e), LetPair (v', (x', y', t'), e')) =
      conv (i, v, v') andalso
      conv' (i+2, whnf (t, Cxt.Ext (y, Gen (i+1), Cxt.Ext (x, Gen (i), e))),
                  whnf (t', Cxt.Ext (y', Gen (i+1), Cxt.Ext (x', Gen (i), e'))))
  | conv' _ = false

(*  fun eq (v, w) = conv (0, v, w) *)
 
  fun eqExp' ((i, e), M, N) = conv (i, Clos (M, e), Clos (N, e)) 
(*
  fun eqExp  (M, N) = eqExp' (empty, M, N)
*)
  fun eqExp2 (i, M, e, M', e') = conv (i, Clos (M, e), Clos (M', e'))

  fun eqExpVal ((i, e), M, v) = conv (i, Clos (M, e), v)

  (*  eqSubst (e, C, M, x, A) = true  iff C == [M/x]A in env e *)
  fun eqSubst ((i, e), C, M, x, A) = conv (i, 
        Clos (C, e), 
        Clos (A, Cxt.Ext (x, eval (M, e), e)))

  (*  eqSubstRec (e, C, M, x, A) = true  iff C(x) == [M(x)/x]A in env e *)
  fun eqSubstRec ((i, e), C, M, x, A) =
      let 
	  val e' = Cxt.Ext (x, Gen (i), e)
      in
	  conv (i+1, Clos (C, e'), 
		     Clos (A, Cxt.Ext (x, eval (M, e'), e)))
      end

  (*  eqSubstVar (e, C, y, x, A) = true  iff C(y) == [y/x]A in env e *)
  fun eqSubstVar ((i, e), C, y, x, A) =
        conv (i+1, Clos (C, Cxt.Ext (y, Gen (i), e)), 
        	   Clos (A, Cxt.Ext (x, Gen (i), e)))
(*
  fun singletonStream (x) = Stream.cons (x, Stream.empty)

  fun split (i, u, v) = if conv (i, u, v) 
        then Stream.cons (fn x => x, split' (i, u, v))
        else split' (i, u, v)
  
  and split' (i, u, Gen (j)) = singletonStream (fn x => Gen (j))
    | split' (i, u, Def (j, v)) = split' (i, u, v)
    | split' (i, u, Clos (t, e)) = split' (i, u, whnf (t, e))

    | split' (i, u, Pair (v, w)) = Stream.mapProd 
        (fn (C, D) => fn x => Pair (C x, D x)) (split (i, u, v), (i, u, w))

    | split' (i, u, Lam (x, t, e)) = Stream.map (fn C => fn y => Lam (x, 
        (split (i+1, u, whnf (t, Cxt.Ext (x, Gen (i), e))))
  and split (i, u, v) = split (i, whnf (u), whnf (v))
*)

(* genInClos:  int * Term * Env -> bool
** genInValue: int * Value -> bool
**
** checks whether the generic value corresponding to the integer
**  appears (syntactically) free in the value/closure
** (without further evaluation)
*)

fun genInClos (i, Exp.Var (x), e) = (case Cxt.lookup (x, e) of (* only local bindings *)
        SOME v => genInVal (i, v)
      | NONE => false) (* or internal error *)
  | genInClos (i, Exp.Lam (x, t), e) = genInClos (i, t, Cxt.Ext (x, Gen (i+1), e))
      (* hack: assign new variable x any generic value != i *)
  | genInClos (i, Exp.App (s, t), e) = genInClos (i, s, e) orelse genInClos (i, t, e)
  | genInClos (i, Exp.Pair (s, t), e) = genInClos (i, s, e) orelse genInClos (i, t, e)
  | genInClos (i, Exp.Fst (t), e) = genInClos (i, t, e)
  | genInClos (i, Exp.Snd (t), e) = genInClos (i, t, e)
  | genInClos (i, Exp.Inl (t), e) = genInClos (i, t, e)
  | genInClos (i, Exp.Inr (t), e) = genInClos (i, t, e)
  | genInClos (i, Exp.Case (r, (x, s), (y, t)), e) = genInClos (i, r, e)
      orelse genInClos (i, s, Cxt.Ext (x, Gen (i+1), e))
      orelse genInClos (i, t, Cxt.Ext (y, Gen (i+1), e))
  | genInClos (i, Exp.Star, e) = false
  | genInClos (i, Exp.Abort (t), e) = genInClos (i, t, e)
  | genInClos (i, Exp.Zero, e) = false
  | genInClos (i, Exp.Succ (t), e) = genInClos (i, t, e)
  | genInClos (i, Exp.RecNat (r, f, s, (x, t)), e) = genInClos (i, r, e)
      orelse genInClos (i, s, Cxt.Ext (f, Gen(i+1), e))
      orelse genInClos (i, t, Cxt.Ext (x, Gen(i+1), Cxt.Ext (f, Gen(i+1), e)))
  | genInClos (i, Exp.True, e) = false
  | genInClos (i, Exp.False, e) = false
  | genInClos (i, Exp.If (r, s, t), e) = genInClos (i, r, e) orelse genInClos (i, s, e) orelse genInClos (i, t, e)
  | genInClos (i, Exp.Nil, e) = false
  | genInClos (i, Exp.Cons (s, t), e) = genInClos (i, s, e) orelse genInClos (i, t, e)
  | genInClos (i, Exp.RecList (r, f, s, (x, xs, t)), e) = genInClos (i, r, e)
      orelse genInClos (i, s, Cxt.Ext (f, Gen(i+1), e))
      orelse genInClos (i, t, Cxt.Ext (xs, Gen(i+1), Cxt.Ext (x, Gen(i+1), Cxt.Ext (f, Gen(i+1), e))))
  | genInClos (i, Exp.LetPair (s, (x, y, t)), e) = genInClos (i, s, e) 
      orelse genInClos (i, t, Cxt.Ext (y, Gen(i+1), Cxt.Ext (x, Gen(i+1), e)))
  | genInClos (i, Exp.Pi (Range.Dep (x), A, B), e) = genInClos (i, A, e) orelse
      genInClos (i, B, Cxt.Ext (x, Gen (i+1), e))
  | genInClos (i, Exp.Pi (R, A, B), e) = genInClos (i, A, e) orelse genInClos (i, B, e)
  | genInClos (i, Exp.Sigma (Range.Dep (x), A, B), e) = genInClos (i, A, e) orelse
      genInClos (i, B, Cxt.Ext (x, Gen (i+1), e))
  | genInClos (i, Exp.Sigma (R, A, B), e) = genInClos (i, A, e) orelse genInClos (i, B, e)
  | genInClos (i, Exp.Sum (A, B), e) = genInClos (i, A, e) orelse genInClos (i, B, e)
  | genInClos (i, Exp.Unit, e) = false
  | genInClos (i, Exp.Empty, e) = false
  | genInClos (i, Exp.Atom (A), e) = false
  | genInClos (i, Exp.Nat, e) = false
  | genInClos (i, Exp.Bool, e) = false
  | genInClos (i, Exp.List (A), e) = genInClos (i, A, e)
  | genInClos (i, Exp.Less (s, t), e) = genInClos (i, s, e) orelse genInClos (i, t, e)
  | genInClos (i, Exp.Equal (s, t), e) = genInClos (i, s, e) orelse genInClos (i, t, e)
  | genInClos (i, Exp.Not (A), e) = genInClos (i, A, e)
  | genInClos (i, Exp.Equiv (A, B), e) = genInClos (i, A, e) orelse genInClos (i, B, e)
  | genInClos (i, Exp.Ann (t, (A, U)), e) = genInClos (i, t, e) orelse genInClos (i, A, e)


and genInVal (i, Gen (j)) = i=j
  | genInVal (i, Def (j, v)) = i=j
  | genInVal (i, Clos (t, e)) = genInClos (i, t, e)
  | genInVal (i, App (v, w)) = genInVal (i, v) orelse genInVal (i, w)
  | genInVal (i, Lam (x, t, e)) = genInClos (i, t, Cxt.Ext (x, Gen (i+1), e))
  | genInVal (i, Pair (v, w)) = genInVal (i, v) orelse genInVal (i, w)
  | genInVal (i, Fst (v)) = genInVal (i, v)
  | genInVal (i, Snd (v)) = genInVal (i, v)
  | genInVal (i, Inl (v)) = genInVal (i, v)
  | genInVal (i, Inr (v)) = genInVal (i, v)
  | genInVal (i, Case (v, (x, s), (y, t), e)) = genInVal (i, v)
      orelse genInClos (i, s, Cxt.Ext (x, Gen (i+1), e))
      orelse genInClos (i, t, Cxt.Ext (y, Gen (i+1), e))
  | genInVal (i, Star) = false
  | genInVal (i, Abort (v)) = genInVal (i, v)
  | genInVal (i, Zero) = false
  | genInVal (i, Succ (v)) = genInVal (i, v)
  | genInVal (i, RecNat (v, f, s, (x, t), e)) = genInVal (i, v)
      orelse genInClos (i, s, Cxt.Ext (f, Gen (i+1), e))
      orelse genInClos (i, t, Cxt.Ext (x, Gen(i+1), Cxt.Ext (f, Gen(i+1), e)))
  | genInVal (i, True) = false
  | genInVal (i, False) = false
  | genInVal (i, If (v, s, t, e)) = genInVal (i, v) orelse genInClos (i, s, e) orelse genInClos (i, t, e)
  | genInVal (i, Nil) = false
  | genInVal (i, Cons (v, w)) = genInVal (i, v) orelse genInVal (i, w)
  | genInVal (i, RecList (v, f, s, (x, xs, t), e)) = genInVal (i, v)
      orelse genInClos (i, s, Cxt.Ext (f, Gen(i+1), e))
      orelse genInClos (i, t, Cxt.Ext (xs, Gen(i+1), Cxt.Ext (x, Gen(i+1), Cxt.Ext (f, Gen(i+1), e))))
  | genInVal (i, LetPair (v, (x, y, t), e)) = genInVal (i, v)
      orelse genInClos (i, t, Cxt.Ext (y, Gen(i+1), Cxt.Ext (x, Gen(i+1), e)))
  | genInVal (i, Equal (v, w)) = genInVal (i, v) orelse genInVal (i, w)
  | genInVal (i, Less (v, w)) = genInVal (i, v) orelse genInVal (i, w)


(* bindVar : int * string * Exp * Env * Names * 
**          (int * string * Env * Names -> 'a) -> 'a
** capture-avoiding variable binding
**
** names: names that are available and whose lower instances are actually 
**        free in the term 
**
** lookes for maximal used instance of x in names
** checks whether x appears free in t
** if yes, renames it to next free instance
** if no, renames it to maximal used instance
*)

fun bindVar (i, x, t, e, names, c) = 
      let val max = Names.findMaxInst (x, names)
	  val e' = Cxt.Ext(x, Gen(i), e)
          val x' = if max = Names.null then x
		   else case lookup (max, e) of
		     SOME v => (case v of
			   Gen(i) => if genInClos (i, t, e') 
				      then Names.nextInst (max)
				      else max
			 | Def(i,v) => if genInClos (i, t, e') 
					then Names.nextInst (max)
					else max
			 | v => max)
		   | NONE => max
      in c (i+1, x', e', Names.add (i, x', names)) end

fun closToBind (i, x, t, e, names) = bindVar (i, x, t, e, names,
      fn (i', x', e', names') => (x', closToExp (i', t, e', names')))


and closToExp (i, Exp.Var(x), e, names) = 
     (case Cxt.lookup (x, e) of (* only lookup local bindings *)
        SOME v => valToExp (i, v, names) 
      | NONE => Exp.Var(x)) (* or internal error *)
  | closToExp (i, Exp.Lam(x,t), e, names) = 
      Exp.Lam (closToBind (i, x, t, e, names)) 
(*      bindVar (i, x, t, e, names, 
        fn (i', x', e', names') => Exp.Lam (x',closToExp (i', t, e', names')))
*)
  | closToExp (i, Exp.App (s, t), e, names) = 
      Exp.App (closToExp (i, s, e, names), closToExp (i, t, e, names))
  | closToExp (i, Exp.Pair (s, t), e, names) = 
      Exp.Pair (closToExp (i, s, e, names), closToExp (i, t, e, names))
  | closToExp (i, Exp.Fst (t), e, names) = Exp.Fst (closToExp (i, t, e, names))
  | closToExp (i, Exp.Snd (t), e, names) = Exp.Snd (closToExp (i, t, e, names))
  | closToExp (i, Exp.Inl (t), e, names) = Exp.Inl (closToExp (i, t, e, names))
  | closToExp (i, Exp.Inr (t), e, names) = Exp.Inr (closToExp (i, t, e, names))
  | closToExp (i, Exp.Case (r, (x, s), (y, t)), e, names) =
      Exp.Case (closToExp (i, r, e, names),
        closToBind (i, x, s, e, names),
	closToBind (i, y, t, e, names))
  | closToExp (i, Exp.Star, e, names) = Exp.Star
  | closToExp (i, Exp.Abort (t), e, names) = Exp.Abort (closToExp (i, t, e, names))
  | closToExp (i, Exp.Zero, e, names) = Exp.Zero
  | closToExp (i, Exp.Succ(t), e, names) = Exp.Succ (closToExp (i, t, e, names))
  | closToExp (i, Exp.RecNat (r, f, s, (x, t)), e, names) = 
      bindVar (i, f, Exp.Pair (s, t) (* hack *), e, names,
	fn (i', f', e', names') => Exp.RecNat (closToExp (i, r, e, names), f',
          closToExp (i', s, e', names'),
	  closToBind (i', x, t, e', names')))
  | closToExp (i, Exp.True, e, names) = Exp.True
  | closToExp (i, Exp.False, e, names) = Exp.False
  | closToExp (i, Exp.If (r, s, t), e, names) = 
      Exp.If (closToExp (i, r, e, names),
	      closToExp (i, s, e, names),
	      closToExp (i, t, e, names))
  | closToExp (i, Exp.Nil, e, names) = Exp.Nil
  | closToExp (i, Exp.Cons (s, t), e, names) = 
      Exp.Cons (closToExp (i, s, e, names), closToExp (i, t, e, names))
  | closToExp (i, Exp.RecList (r, f, s, (x, xs, t)), e, names) = 
      let 
	  val r' = closToExp (i, r, e, names)
      in  (* hack: abuse Pair for test if f appears in s or in t *)
	  bindVar (i, f, Exp.Pair (s, t), e, names, fn (i, f', e, names) => 
	    Exp.RecList (r', f',
              closToExp (i, s, e, names),
	      bindVar (i, x, t, e, names, fn (i, x', e, names) => 
  	        bindVar (i, xs, t, e, names, fn (i, xs', e, names) =>
		  (x', xs', closToExp (i, t, e, names))))))
      end
  | closToExp (i, Exp.LetPair (s, (x, y, t)), e, names) = 
      let 
	  val s' = closToExp (i, s, e, names)
      in
	  bindVar (i, x, t, e, names, fn (i, x', e, names) =>
  	    bindVar (i, y, t, e, names, fn (i, y', e, names) =>
              Exp.LetPair (s', (x', y', closToExp (i, t, e, names)))))
      end
  | closToExp (i, Exp.Pi (Range.Dep (x), A, B), e, names) = 
      let
	  val A' = closToExp (i, A, e, names)
      in 
	  bindVar (i, x, B, e, names, fn (i, x', e, names) =>
            Exp.Pi (Range.Dep (x'), A', closToExp (i, B, e, names)))
      end
  | closToExp (i, Exp.Pi (R, A, B), e, names) = Exp.Pi (R, 
      closToExp (i, A, e, names),
      closToExp (i, B, e, names))
  | closToExp (i, Exp.Sigma (Range.Dep (x), A, B), e, names) = 
      let
	  val A' = closToExp (i, A, e, names)
      in 
	  bindVar (i, x, B, e, names, fn (i, x', e, names) =>
            Exp.Sigma (Range.Dep (x'), A', closToExp (i, B, e, names)))
      end
  | closToExp (i, Exp.Sigma (R, A, B), e, names) = Exp.Sigma (R, 
      closToExp (i, A, e, names),
      closToExp (i, B, e, names))
  | closToExp (i, Exp.Sum (A, B), e, names) = Exp.Sum (
      closToExp (i, A, e, names),
      closToExp (i, B, e, names))
  | closToExp (i, Exp.Unit, e, names) = Exp.Unit
  | closToExp (i, Exp.Empty, e, names) = Exp.Empty
  | closToExp (i, Exp.Atom(X), e, names) = Exp.Atom(X)
  | closToExp (i, Exp.Nat, e, names) = Exp.Nat
  | closToExp (i, Exp.Bool, e, names) = Exp.Bool
  | closToExp (i, Exp.List (A), e, names) = Exp.List (closToExp (i, A, e, names))
  | closToExp (i, Exp.Less (M, N), e, names) = Exp.Less (closToExp (i, M, e, names),
      closToExp (i, N, e, names))
  | closToExp (i, Exp.Equal (M, N), e, names) = Exp.Equal (closToExp (i, M, e, names),
      closToExp (i, N, e, names))
  | closToExp (i, Exp.Not (A), e, names) = Exp.Not (closToExp (i, A, e, names))
  | closToExp (i, Exp.Equiv (A, B), e, names) = 
      Exp.Equiv (closToExp (i, A, e, names), closToExp (i, B, e, names))
  | closToExp (i, Exp.Ann (M, (A, U)), e, names) = Exp.Ann (closToExp (i, M, e, names),
      (closToExp (i, A, e, names), U))




and valToExp (i, Gen (j), names) = Exp.Var (Names.get (j, names))
  | valToExp (i, Def (j, v), names) = Exp.Var (Names.get (j, names))
  | valToExp (i, Lam (x, t, e), names) = Exp.Lam (closToBind (i, x, t, e, names))
  | valToExp (i, App (v, w), names) = Exp.App (valToExp (i, v, names), valToExp (i, w, names)) 
  | valToExp (i, Clos (t, e), names) = closToExp (i, t, e, names)

  | valToExp (i, Pair (v, w), names) = Exp.Pair (valToExp (i, v, names), valToExp (i, w, names))
  | valToExp (i, Fst (v), names) = Exp.Fst (valToExp (i, v, names))
  | valToExp (i, Snd (v), names) = Exp.Snd (valToExp (i, v, names))
  | valToExp (i, Inl (v), names) = Exp.Inl (valToExp (i, v, names))
  | valToExp (i, Inr (v), names) = Exp.Inr (valToExp (i, v, names))
  | valToExp (i, Case (v, (x, s), (y, t), e), names) = 
      Exp.Case (valToExp (i, v, names), 
        closToBind (i, x, s, e, names),
        closToBind (i, y, t, e, names))
  | valToExp (i, Star, names) = Exp.Star
  | valToExp (i, Abort (v), names) = Exp.Abort (valToExp (i, v, names))
  | valToExp (i, Zero, names) = Exp.Zero
  | valToExp (i, Succ (v), names) = Exp.Succ (valToExp (i, v, names))
  | valToExp (i, RecNat (v, f, s, (x, t), e), names) = 
      let 
	  val r = valToExp (i, v, names)
      in 
	  bindVar (i, f, Exp.Pair (s, t) (* hack *), e, names,
	    fn (i, f', e, names) => Exp.RecNat (r, f',
              closToExp (i, s, e, names),
	      closToBind (i, x, t, e, names)))
      end
  | valToExp (i, True, names) = Exp.True
  | valToExp (i, False, names) = Exp.False
  | valToExp (i, If (v, s, t, e), names) = Exp.If (valToExp (i, v, names),
      closToExp (i, s, e, names), closToExp (i, t, e, names))
  | valToExp (i, Nil, names) = Exp.Nil
  | valToExp (i, Cons (v, w), names) = Exp.Cons (valToExp (i, v, names),
      valToExp (i, w, names))
  | valToExp (i, RecList (v, f, s, (x, xs, t), e), names) = 
      let 
	  val r = valToExp (i, v, names)
      in  (* hack: abuse Pair for test if f appears in s or in t *)
	  bindVar (i, f, Exp.Pair (s, t), e, names, fn (i1, f', e1, names1) => 
	    Exp.RecList (r, f',
              closToExp (i1, s, e1, names1),
	      bindVar (i1, x, t, e1, names1, fn (i2, x', e2, names2) => 
  	        bindVar (i2, xs, t, e2, names2, fn (i3, xs', e3, names3) =>
		  (x', xs', closToExp (i3, t, e3, names3))))))
      end
  | valToExp (i, LetPair (v, (x, y, t), e), names) = 
      let
	  val s = valToExp (i, v, names)
      in
	  bindVar (i, x, t, e, names, fn (i, x', e, names) =>
  	    bindVar (i, y, t, e, names, fn (i, y', e, names) =>
              Exp.LetPair (s, (x', y', closToExp (i, t, e, names)))))
      end
  | valToExp (i, Equal (v, w), names) = Exp.Equal (valToExp (i, v, names),
      valToExp (i, w, names))
  | valToExp (i, Less (v, w), names) = Exp.Less (valToExp (i, v, names),
      valToExp (i, w, names))

fun toNames (Cxt.Empty) = !globN
  | toNames (Cxt.Ext (x, Gen(i), e')) = Names.add (i, x, toNames (e'))
  | toNames (Cxt.Ext (x, v, e')) = toNames (e')

(*
fun toExp ((i, e), v) = valToExp (i, v, toNames (e)) 
*)
fun toExp ((i, e), v) = valToExp (i, v, toNames (e)) 
fun toString (e, v) = Exp.toString (toExp (e, v)) 


val nat  = Clos (Exp.Nat, Cxt.Empty)
val bool = Clos (Exp.Bool, Cxt.Empty)

end (* structure Val *)
