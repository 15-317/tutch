(* $Id: exp.sml,v 1.3 2000/10/24 21:09:54 abel Exp $ *)

structure  Univ = struct
  datatype Univ = 
      Prop 
    | Type
end (* structure Univ *)

structure  Range = struct
  datatype Range = 
      Prop
    | Type
    | Dep of string
end (* structure Range *)

signature EXP =
sig

  datatype Exp =
      Pi of Range.Range * Exp * Exp       (* T ::= S -> T, A => B, !x:S. B   *)
    | Sigma of Range.Range * Exp * Exp    (*     | S * T,  A & B,  ?x:S. B   *)
    | Sum of Exp * Exp                    (*     | S + T,  A | B             *)
    | Unit                                (*     | 1, T                      *)
    | Empty                               (*     | 0, F                      *)
    | Atom of string                      (*     | P                         *)
    | Nat                                 (*     | nat                       *)
    | Bool                                (*     | bool                      *)
    | List of Exp                         (*     | T list                    *)
    | Equal of Exp * Exp                  (*     | s = t                     *)
    | Less of Exp * Exp                   (*     | s < t                     *)

    | Not of Exp                          (*     | ~ A                       *)
    | Equiv of Exp * Exp                  (*     | A <=> B                   *)
                                          (* app | A t                       *)

    | Var of string                       (* s ::= x                         *)
    | Pair of Exp * Exp                   (*     | (s, t)                    *)
    | Fst of Exp                          (*     | fst s                     *)
    | Snd of Exp                          (*     | snd s                     *)
    | Inl of Exp                          (*     | inl s                     *)
    | Inr of Exp                          (*     | inr s                     *)
    | Case of Exp * (string * Exp) * (string * Exp)     
                              (* | case r of inl x => s | inr y => t end     *)
    | Lam of (string * Exp)               (*     | fn x => s                 *)
    | App of Exp * Exp                    (*     | s t                       *)
    | Star                                (*     | ()                        *)
    | Abort of Exp                        (*     | abort s                   *)
    | Zero                                (*     | 0                         *)
    | Succ of Exp                         (*     | "s" t                     *)
    | RecNat of Exp * string * Exp * (string * Exp)
                              (* | rec r of f 0 => s | f (s x) => t end      *)
    | True                                (*     | true                      *)
    | False                               (*     | false                     *)
    | If of Exp * Exp * Exp               (*     | if r then e else t        *)
    | Nil                                 (*     | nil                       *)
    | Cons of Exp * Exp                   (*     | s :: t                    *)
    | RecList of Exp * string * Exp * (string * string * Exp)           
                              (* | rec r of f nil => s | f (x::xs) => t end  *)
    | LetPair of Exp * (string * string * Exp) 
                              (* | let (x,y) = M in N                        *)
    | Eq0                                 (*     | eq0                       *) 
    | EqS of Exp                          (*     | eqS s                     *) 
    | EqE0S of Exp                        (*     | eqE0S s                   *) 
    | EqES0 of Exp                        (*     | eqES0 s                   *) 
    | EqESS of Exp                        (*     | eqESS s                   *) 
    | Less0                               (*     | less0                     *) 
    | LessS of Exp                        (*     | lessS s                   *) 
    | LessE0 of Exp                       (*     | lessE0 s                  *) 
    | LessES of Exp                       (*     | lessES s                  *) 
    | EqN                                 (*     | eqN                       *) 
    | EqC of Exp                          (*     | eqC s                     *) 
    | EqENC of Exp                        (*     | eqENC s                   *) 
    | EqECN of Exp                        (*     | eqECN s                   *) 
    | EqECC of Exp                        (*     | eqECC s                   *) 
    | Ann of Exp * UExp                   (*     | (s : T) (s : A)           *)

  withtype UExp = Exp * Univ.Univ

  type Cxt = Exp Cxt.Cxt

  val andDef : Exp * Exp -> Exp
  val impliesDef : Exp * Exp -> Exp

  val notDef : Exp -> Exp
  val equivDef : Exp * Exp -> Exp

 (*  convRec (f, x, t) = t'
     replaces all applications of f to x by a variable "f x"  *)
  val convRec: string * string * Exp -> Exp

  (* val eq : Exp * Exp -> bool *)

  val termToString : Exp -> string
  val typeToString : Exp -> string
  val propToString : Exp -> string

  val termPretty : Exp -> string
  val typePretty : Exp -> string
  val propPretty : Exp -> string

  val toString : Exp -> string (* raw *)
(*
  val prettyString : Exp -> string
*)

(*
  val substVar : string * string * Exp -> Exp
  val subst : Exp * string * Exp -> Exp
  val split : Exp * Exp
*)

end (* signature EXP *)


structure Exp :> EXP =
struct

  datatype Exp =
      Pi of Range.Range * Exp * Exp       (* T ::= S -> T, A => B, !x:S. B   *)
    | Sigma of Range.Range * Exp * Exp    (*     | S * T,  A & B,  ?x:S. B   *)
    | Sum of Exp * Exp                    (*     | S + T,  A | B             *)
    | Unit                                (*     | 1, T                      *)
    | Empty                               (*     | 0, F                      *)
    | Atom of string                      (*     | P                         *)
    | Nat                                 (*     | nat                       *)
    | Bool                                (*     | bool                      *)
    | List of Exp                         (*     | T list                    *)
    | Equal of Exp * Exp                  (*     | s = t                     *)
    | Less of Exp * Exp                   (*     | s < t                     *)

    | Not of Exp                          (*     | ~ A                       *)
    | Equiv of Exp * Exp                  (*     | A <=> B                   *)
                                          (* app | A t                       *)

    | Var of string                       (* s ::= x                         *)
    | Pair of Exp * Exp                   (*     | (s, t)                    *)
    | Fst of Exp                          (*     | fst s                     *)
    | Snd of Exp                          (*     | snd s                     *)
    | Inl of Exp                          (*     | inl s                     *)
    | Inr of Exp                          (*     | inr s                     *)
    | Case of Exp * (string * Exp) * (string * Exp)     
                              (* | case r of inl x => s | inr y => t end     *)
    | Lam of (string * Exp)               (*     | fn x => s                 *)
    | App of Exp * Exp                    (*     | s t                       *)
    | Star                                (*     | ()                        *)
    | Abort of Exp                        (*     | abort s                   *)
    | Zero                                (*     | 0                         *)
    | Succ of Exp                         (*     | "s" t                     *)
    | RecNat of Exp * string * Exp * (string * Exp)
                              (* | rec r of f 0 => s | f (s x) => t end      *)
    | True                                (*     | true                      *)
    | False                               (*     | false                     *)
    | If of Exp * Exp * Exp               (*     | if r then e else t        *)
    | Nil                                 (*     | nil                       *)
    | Cons of Exp * Exp                   (*     | s :: t                    *)
    | RecList of Exp * string * Exp * (string * string * Exp)           
                              (* | rec r of f nil => s | f (x::xs) => t end  *)
    | LetPair of Exp * (string * string * Exp)
                              (* | let (x,y) = M in N                        *)
    | Eq0                                 (*     | eq0                       *) 
    | EqS of Exp                          (*     | eqS s                     *) 
    | EqE0S of Exp                        (*     | eqE0S s                   *) 
    | EqES0 of Exp                        (*     | eqES0 s                   *) 
    | EqESS of Exp                        (*     | eqESS s                   *) 
    | Less0                               (*     | less0                     *) 
    | LessS of Exp                        (*     | lessS s                   *) 
    | LessE0 of Exp                       (*     | lessE0 s                  *) 
    | LessES of Exp                       (*     | lessES s                  *) 
    | EqN                                 (*     | eqN                       *) 
    | EqC of Exp                          (*     | eqC s                     *) 
    | EqENC of Exp                        (*     | eqENC s                   *) 
    | EqECN of Exp                        (*     | eqECN s                   *) 
    | EqECC of Exp                        (*     | eqECC s                   *) 
    | Ann of Exp * UExp                   (*     | (s : T) (s : A)           *)

  withtype UExp = Exp * Univ.Univ

  type Cxt = Exp Cxt.Cxt

  fun andDef (A, B) = Sigma (Range.Prop, A, B)
  fun impliesDef (A, B) = Pi (Range.Prop, A, B)

  fun notDef (A) = impliesDef (A, Empty)
  fun equivDef (A, B) = andDef (impliesDef (A, B), impliesDef (B, A)) 

  fun strip (Ann (M, T)) = strip M
    | strip (M) = M

 (*  convRec (f, x, M) = M'
     replaces all applications of f to x by a variable "f x"  *)
  fun convRec (f, x, M') =

  let fun cnv (O as App (M, N)) = (case (strip (M), strip (N)) of
              (Var (g), Var (y)) => if (g=f) andalso (y=x) then Var (f^" "^x)
				   else O
	    | (M', N') => App (cnv (M), cnv (N)))
	| cnv (Var (y)) = Var (y)
	| cnv (Lam B) = Lam (cnvBind B)
	| cnv (Pair (N, M)) = Pair (cnv N, cnv M)
	| cnv (Fst M) = Fst (cnv M)
	| cnv (Snd M) = Snd (cnv M)
	| cnv (Inl M) = Inl (cnv M)
	| cnv (Inr M) = Inr (cnv M)
	| cnv (Case (M, N, O)) = Case (cnv M, cnvBind (N), cnvBind (O))
	| cnv (Star) = Star
	| cnv (Abort M) = Abort (cnv M)
	| cnv (Zero) = Zero
	| cnv (Succ M) = Succ (cnv M)
	| cnv (RecNat (M, g, N, B)) = if (g=f) orelse (g=x) 
	    then RecNat (cnv M, g, N, B)
	    else RecNat (cnv M, g, cnv N, cnvBind B)
	| cnv (True) = True
	| cnv (False) = False
	| cnv (If (M, N, O)) = If (cnv M, cnv N, cnv O)
	| cnv (Nil) = Nil
	| cnv (Cons (M, N)) = Cons (cnv M, cnv N)
	| cnv (RecList (M, g, N, B)) = if (g=f) orelse (g=x) 
	    then RecList (cnv M, g, N, B)
	    else RecList (cnv M, g, cnv N, cnvBind2 B)
	| cnv (LetPair (M, (x, y, N))) = LetPair (cnv (M), cnvBind2 (x, y, N))
	| cnv (Eq0) = Eq0
	| cnv (EqS (M)) = EqS (cnv (M))
	| cnv (EqE0S (M)) = EqE0S (cnv (M))
	| cnv (EqES0 (M)) = EqES0 (cnv (M))
	| cnv (EqESS (M)) = EqESS (cnv (M))
	| cnv (Less0) = Less0
	| cnv (LessS (M)) = LessS (cnv (M))
	| cnv (LessE0 (M)) = LessE0 (cnv (M))
	| cnv (LessES (M)) = LessES(cnv (M))
	| cnv (EqN) = Eq0
	| cnv (EqC (M)) = EqS (cnv (M))
	| cnv (EqENC (M)) = EqENC (cnv (M))
	| cnv (EqECN (M)) = EqECN (cnv (M))
	| cnv (EqECC (M)) = EqECC (cnv (M))

      and cnvBind (y, M) = if (y=f) orelse (y=x) 
			   then (y, M)          (* shadowing recursion var *)
			   else (y, cnv (M))
      and cnvBind2 (y, ys, M) = if (y=x) orelse (y=f) orelse (ys=x) orelse (ys=f)
            then (y, ys, M)
	    else (y, ys, cnv M)
  in cnv (M') end

(*  fun eq (s, t) = s = t *)


  fun paren s = "("^s^")"

  fun toString (Pi (Range.Prop, A, B)) = paren (toString A^" => "^toString B)
    | toString (Pi (Range.Type, A, B)) = paren (toString A^" -> "^toString B)
    | toString (Pi (Range.Dep x, A, B)) = paren ("!"^x^":"^toString A^". "^toString B)
    | toString (Sigma (Range.Prop, A, B)) = paren (toString A^" & "^toString B)
    | toString (Sigma (Range.Type, A, B)) = paren (toString A^" * "^toString B)
    | toString (Sigma (Range.Dep x, A, B)) = paren ("?"^x^":"^toString A^". "^toString B)
    | toString (Sum (A, B)) = paren (toString A ^" + "^ toString B)
    | toString (Unit) = "0"
    | toString (Empty) = "1"
    | toString (Atom (X)) = X
    | toString (Nat) = "nat"
    | toString (Bool) = "bool"
    | toString (List(T)) = paren (toString T ^ " list")
    | toString (Equal(M,N)) = paren (toString M ^" = "^toString N)
    | toString (Less(M,N)) = paren (toString M ^" < "^toString N)
    | toString (Not(A)) = toString (notDef (A))
    | toString (Equiv(A,B)) = toString (equivDef (A, B))
    | toString (Var(x)) = x
    | toString (Pair (M, N)) = paren (toString M ^ ", "^toString N)
    | toString (Fst (M)) = paren ("fst "^toString M)
    | toString (Snd (M)) = paren ("snd "^toString M)
    | toString (Inl (M)) = paren ("inl "^toString M)
    | toString (Inr (M)) = paren ("inr "^toString M)
    | toString (Case (M, (x,N), (y,O))) = paren ("case "^toString M^" of inl => "^toString N^" | inr => "^toString O^" end")
    | toString (Lam (x, M)) = paren ("fn "^x^" => "^toString M)
    | toString (App (M, N)) = paren (toString M ^ " " ^ toString N)
    | toString (Star) = "()"
    | toString (Abort M) = paren ("abort "^toString M)
    | toString (Zero) = "0"
    | toString (Succ (M)) = paren ("s "^toString M)
    | toString (RecNat (M, f, N, (x, O))) = paren ("rec "^toString M ^ " of "
        ^f^"(0) =>"^toString N^" | "^f^"(s "^x^") => "^toString O^" end")
    | toString (True) = "true"
    | toString (False) = "false"
    | toString (If (N, M, O)) = paren ("if "^toString N^" then "^toString M^" else "^toString O)
    | toString (Nil) = "nil"
    | toString (Cons (M, N)) = paren (toString M^" :: "^toString N)
    | toString (RecList (M, f, N, (x, xs, O))) = paren ("rec "^toString M ^ " of "
        ^f^"(nil) =>"^toString N^" | "^f^"("^x^"::"^xs^") => "^toString O^" end")
    | toString (LetPair (M, (x, y, N))) = paren ("let ("^x^", "^y^") = "
        ^toString M ^" in "^toString N)
    | toString (Eq0) = "eq0"
    | toString (EqS (M)) = paren ("eqS "^toString M)
    | toString (EqE0S (M)) = paren ("eqE0S "^toString M)
    | toString (EqES0 (M)) = paren ("eqES0 "^toString M)
    | toString (EqESS (M)) = paren ("eqESS "^toString M)
    | toString (Less0) = "less0"
    | toString (LessS (M)) = paren ("lessS "^toString M)
    | toString (LessE0 (M)) = paren ("lessE0 "^toString M)
    | toString (LessES (M)) = paren ("lessES "^toString M)
    | toString (EqN) = "eqN"
    | toString (EqC (M)) = paren ("eqC "^toString M)
    | toString (EqENC (M)) = paren ("eqENC "^toString M)
    | toString (EqECN (M)) = paren ("eqECN "^toString M)
    | toString (EqECC (M)) = paren ("eqECC "^toString M)
    | toString (Ann (M, (T, U))) = paren (toString M ^ " : " ^ toString T)
 
(* binding strength
   10: app right and all prefix constructors
    9: app left
    7: cons right
    6: cons left
    1: lam, if
    0: case, rec, ann
*)

  fun tm2s (tm as App (s, t), 10) = par (tm)
    | tm2s (tm as App (s, t),  n) = (tm2s (s, 9))^" "^(tm2s (t, 10))
    | tm2s (tm as Lam (B), n) = if n>=2 then par (tm)
				       else ("fn "^(bind2s B))
    | tm2s (tm as Var (x), n) = x
    | tm2s (tm as Case (s, B, B'), n) = if n>=2 then par (tm)
	else ("case "^(tm2s (s, 5))^" of inl "^(bind2s B)^" | inr "^(bind2s B')^" end")
    | tm2s (tm as (Pair (s, t)), n) = "("^(tm2s (s, 0))^", "^(tm2s (t, 0))^")"
    | tm2s (tm as (Fst (s)), n)  = if n>=9 then par (tm) else "fst "^(tm2s (s, 10))
    | tm2s (tm as (Snd (s)), n)  = if n>=9 then par (tm) else "snd "^(tm2s (s, 10))
    | tm2s (tm as (Inl (s)), n)  = if n>=9 then par (tm) else "inl "^(tm2s (s, 10))
    | tm2s (tm as (Inr (s)), n)  = if n>=9 then par (tm) else "inr "^(tm2s (s, 10))
    | tm2s (Star, n) = "()"
    | tm2s (tm as (Abort (s)), n)  = if n>=9 then par (tm) else "abort "^(tm2s (s, 10))
    | tm2s (Zero, n) = "0"
    | tm2s (tm as (Succ (t)), n) = if n>=9 then par (tm) else "s "^(tm2s (t, 10))
    | tm2s (tm as (RecNat (r, f, s, (x, t))), n) = if n>=2 then par (tm)
        else "rec "^tm2s(r,5)^" of "^f^" 0 => "^tm2s (s, 0)^" | "^f^" (s "
             ^x^") => "^tm2s (t, 0)^" end"
    | tm2s (True, n) = "true"
    | tm2s (False, n) = "false"
    | tm2s (tm as (If (r, s, t)), n) = if n>=2 then par (tm) 
        else "if "^tm2s (r, 5)^" then "^tm2s (s, 0)^" else "^tm2s (t, 0)
    | tm2s (Nil, n) = "nil"
    | tm2s (tm as (Cons (s, t)), n) = if n >= 7 then par (tm) 
        else tm2s (s, 7) ^ " :: " ^ tm2s(t, 6)
    | tm2s (tm as (RecList (r, f, s, (x,xs,t))), n) = if n>=2 then par (tm)
        else "rec "^tm2s (r, 5)^" of "^f^" nil => "^tm2s (s, 0)^
             " | "^f^" ("^x^" :: "^xs^") => "^tm2s (t, 0) ^ " end"
    | tm2s (tm as (LetPair (s, (x, y, t))), n) = if n >=2 then par (tm)
        else "let ("^x^", "^y^") = "^tm2s (s, 5)^" in "^tm2s (t, n)
    | tm2s (Eq0, n) = "eq0"
    | tm2s (tm as (EqS (t)), n) = if n>=9 then par (tm) else "eqS "^(tm2s (t, 10))
    | tm2s (tm as (EqE0S (t)), n) = if n>=9 then par (tm) else "eqE0S "^(tm2s (t, 10))
    | tm2s (tm as (EqES0 (t)), n) = if n>=9 then par (tm) else "eqES0 "^(tm2s (t, 10))
    | tm2s (tm as (EqESS (t)), n) = if n>=9 then par (tm) else "eqESS "^(tm2s (t, 10))
    | tm2s (Less0, n) = "less0"
    | tm2s (tm as (LessS (t)), n) = if n>=9 then par (tm) else "lessS "^(tm2s (t, 10))
    | tm2s (tm as (LessE0 (t)), n) = if n>=9 then par (tm) else "lessE0 "^(tm2s (t, 10))
    | tm2s (tm as (LessES (t)), n) = if n>=9 then par (tm) else "lessES "^(tm2s (t, 10))
    | tm2s (EqN, n) = "eqN"
    | tm2s (tm as (EqC (t)), n) = if n>=9 then par (tm) else "eqC "^(tm2s (t, 10))
    | tm2s (tm as (EqENC (t)), n) = if n>=9 then par (tm) else "eqENC "^(tm2s (t, 10))
    | tm2s (tm as (EqECN (t)), n) = if n>=9 then par (tm) else "eqECN "^(tm2s (t, 10))
    | tm2s (tm as (EqECC (t)), n) = if n>=9 then par (tm) else "eqECC "^(tm2s (t, 10))

    | tm2s (tm as (Ann (s, (A, Univ.Type))), n) = if n>=1 then par (tm) else tm2s (s, 0) ^" : "^ty2s (A, 0, true)
    | tm2s (tm as (Ann (s, (A, Univ.Prop))), n) = if n>=1 then par (tm) else tm2s (s, 0) ^" : "^p2s (A, 0, true)

    | tm2s (tm, _) = "!MALFORMED_TERM:"^toString tm^"!"

  and bind2s (x, s) = x^" => "^(tm2s (s, 1))
  and par (s) = "("^(tm2s (s, 0))^")"

(* Operator precedence
**
** Not > And > Or > Imp 
**  10   8,7   5,4  2,1  
**
*)

(*
  datatype Fix = 
      InfL  (* left side of infix op *)
    | InfR  (* right side of infix op *)
    | Pref  (* Operand of application *)
    | Nof
*)
  (* rm: rightmost position ? *)

  and ty2s (ty as (Pi (Range.Type, S, T)), n, rm) = if n>=2 then parT ty
        else ty2s (S, 2, false)^" -> "^ty2s (T, 1, rm)
    | ty2s (ty as (Pi (Range.Dep x, S, T)), n, rm) = (case rm of
          false => parT ty
	| true  => "{"^x^":"^ty2s (S, 0, true)^"}"^ty2s (T, 0, true))

    | ty2s (ty as (Sigma (Range.Type, S, T)), n, rm) = if n>=8 then parT ty
        else ty2s (S, 8, false)^" * "^ty2s (T, 7, rm)
    | ty2s (ty as (Sigma (Range.Dep x, S, T)), n, rm) = (case rm of
          false => parT ty
	| true  => "<"^x^":"^ty2s (S, 0, true)^">"^ty2s (T, 0, true))

    | ty2s (ty as (Sum (S, S')), n, rm) = if n >= 5 then parT ty
        else ty2s (S, 5, false)^" + "^ty2s (S', 4, rm)
     
    | ty2s (ty as (Atom (X)), n, rm) = X
    | ty2s (ty as (Unit), n, rm) = "1"
    | ty2s (ty as (Empty), n, rm) = "0"
    | ty2s (ty as (Nat), n, rm) = "nat"
    | ty2s (ty as (Bool), n, rm) = "bool"
    | ty2s (ty as (List(T)), n, rm) = ty2s (T, 10, false)^" list"
    | ty2s (ty, _, _) = "!MALFORMED_TYPE:"^toString ty ^"!"

  and parT ty = paren (ty2s (ty, 0, true))


  and p2s (p as (Pi (Range.Prop, S, T)), n, rm) = if n>=2 then parP p
        else p2s (S, 2, false)^" => "^p2s (T, 1, rm)
    | p2s (p as (Pi (Range.Dep x, S, T)), n, rm) = (case rm of
          false => parP p
	| true  => "!"^x^":"^ty2s (S, 0, true)^". "^p2s (T, 0, true))

    | p2s (p as (Sigma (Range.Prop, S, T)), n, rm) = if n>=8 then parP p
        else p2s (S, 8, false)^" & "^p2s (T, 7, rm)
    | p2s (p as (Sigma (Range.Dep x, S, T)), n, rm) = (case rm of
          false => parP p
	| true  => "?"^x^":"^ty2s (S, 0, true)^". "^p2s (T, 0, true))

    | p2s (p as (Sum (S, S')), n, rm) = if n >= 5 then parP p
        else p2s (S, 5, false)^" | "^p2s (S', 4, rm)
     
    | p2s (p as (App (A, M)), n, rm) = if n >= 10 then parP (p) 
        else p2s (A, 9, false) ^ " " ^ tm2s (M, 10)
    | p2s (p as (Equal (N, M)), n, rm) = tm2s (N, 5) ^ " = " ^ tm2s (M, 5)
    | p2s (p as (Less (N, M)), n, rm) = tm2s (N, 5) ^ " < " ^ tm2s (M, 5)

    | p2s (p as (Atom (X)), n, rm) = X
    | p2s (p as Unit, n, rm) = "T"
    | p2s (p as Empty, n, rm) = "F"
    | p2s (p as (Not (A)), n, rm) = "~" ^ p2s (A, 10, rm)
    | p2s (p as (Equiv (A, B)), n, rm) = if n >= 1 then parP p
        else p2s (A, 1, false)^" <=> "^p2s (B, 1, rm)
    | p2s (p, _, _) = "!MALFORMED_PROP:"^toString p^"!"

  and parP p = paren (p2s (p, 0, true))


  fun termToString (M) = tm2s (M, 0)
  fun typeToString (T) = ty2s (T, 0, true)
  fun propToString (A) = p2s (A, 0, true)

  fun termPretty (M) = tm2s (M, 0)
  fun typePretty (T) = ty2s (T, 0, true)
  fun propPretty (A) = p2s (A, 0, true)



(*
  fun prettyString (s) = tm2s (s, 0)
*)


end (* structure Exp *)

