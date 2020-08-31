(* $Id: uexp.sml,v 1.1 2000/10/24 22:42:11 abel Exp $ *)

(*
structure  Univ = struct
  datatype Univ = 
      Prop 
    | Type
end (* structure Univ *)
*)

signature UEXP =
sig
  
  type UExp = Exp.Exp * Univ.Univ
  type Cxt (* =  UExp Cxt.Cxt *)

  val toString: UExp -> string
  val pretty : UExp -> string
 
end (* signature UEXP *)


structure UExp :> UEXP =
struct
  
  type UExp = Exp.Exp * Univ.Univ
  type Cxt = UExp Cxt.Cxt 

  open Exp;

  fun paren s = "("^s^")"

  fun toString (Pi (Range.Prop, A, B), Univ.Prop) = 
        paren (toString (A, Univ.Prop)^" => "^toString (B, Univ.Prop))
    | toString (Pi (Range.Type, A, B), Univ.Type) = 
	paren (toString (A, Univ.Prop)^" -> "^toString (B, Univ.Prop))
    | toString (Pi (Range.Dep x, A, B), Univ.Prop) = 
        paren ("!"^x^":"^toString (A, Univ.Type)^". "^toString (B, Univ.Prop))
    | toString (Sigma (Range.Prop, A, B), Univ.Prop) = 
        paren (toString (A, Univ.Prop)^" & "^toString (B, Univ.Prop))
    | toString (Sigma (Range.Type, A, B), Univ.Type) = 
        paren (toString (A, Univ.Type)^" * "^toString (B, Univ.Type))
    | toString (Sigma (Range.Dep x, A, B), Univ.Prop) = 
        paren ("?"^x^":"^toString (A, Univ.Type)^". "^toString (B, Univ.Prop))
    | toString (Sum (A, B), Univ.Prop) = 
        paren (toString (A, Univ.Prop) ^" | "^ toString (B, Univ.Prop))
    | toString (Sum (A, B), Univ.Type) = 
        paren (toString (A, Univ.Type) ^" + "^ toString (B, Univ.Type))
    | toString (Unit, Univ.Type) = "0"
    | toString (Empty, Univ.Type) = "1"
    | toString (Unit, Univ.Prop) = "T"
    | toString (Empty, Univ.Prop) = "F"
    | toString (Atom (X), U) = X
    | toString (Nat, Univ.Type) = "nat"
    | toString (Bool, Univ.Type) = "bool"
    | toString (List(T), Univ.Type) = paren (toString (T, Univ.Type) ^ " list")
    | toString (Equal(M,N), Univ.Prop) = paren (Exp.toString M ^" = "^Exp.toString N)
    | toString (Less(M,N), Univ.Prop) = paren (Exp.toString M ^" < "^Exp.toString N)
    | toString (Not(A), Univ.Prop) = toString (notDef (A), Univ.Prop)
    | toString (Equiv(A,B), Univ.Prop) = toString (equivDef (A, B), Univ.Prop)
    | toString (App (A, M), Univ.Prop) = 
        paren (toString (A, Univ.Prop) ^" "^Exp.toString (M))
    | toString (M, Univ.Prop) = "$BAD PROP: "^Exp.toString (M)^"$"
    | toString (M, Univ.Type) = "$BAD TYPE: "^Exp.toString (M)^"$"
  
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
    | tm2s (tm as (Ann (s, AU)), n) = if n>=1 then par (tm) else tm2s (s, 0) ^" : "^ty2s (AU, 0, true)


    | tm2s (tm, _) = "!MALFORMED_TERM:"^toString (tm, Univ.Type)^"!"

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

  and ty2s (ty as (Pi (Range.Type, S, T), Univ.Type), n, rm) = 
        if n>=2 then parT ty
        else ty2s ((S, Univ.Type), 2, false)^" -> "^ty2s ((T, Univ.Type), 1, rm)
    | ty2s (ty as (Pi (Range.Dep x, S, T), Univ.Type), n, rm) = (case rm of
          false => parT ty
	| true  => "{"^x^":"^ty2s ((S, Univ.Type), 0, true)^"}"^ty2s ((T, Univ.Type), 0, true))

    | ty2s (ty as (Sigma (Range.Type, S, T), Univ.Type), n, rm) = if n>=8 then parT ty
        else ty2s ((S, Univ.Type), 8, false)^" * "^ty2s ((T, Univ.Type), 7, rm)
    | ty2s (ty as (Sigma (Range.Dep x, S, T), Univ.Type), n, rm) = (case rm of
          false => parT ty
	| true  => "<"^x^":"^ty2s ((S, Univ.Type), 0, true)^">"^ty2s ((T, Univ.Type), 0, true))

    | ty2s (ty as (Sum (S, S'), Univ.Type), n, rm) = if n >= 5 then parT ty
        else ty2s ((S, Univ.Type), 5, false)^" + "^ty2s ((S', Univ.Type), 4, rm)
     
    | ty2s (ty as (Atom (X), Univ.Type), n, rm) = X
    | ty2s (ty as (Unit, Univ.Type), n, rm) = "1"
    | ty2s (ty as (Empty, Univ.Type), n, rm) = "0"
    | ty2s (ty as (Nat, Univ.Type), n, rm) = "nat"
    | ty2s (ty as (Bool, Univ.Type), n, rm) = "bool"
    | ty2s (ty as (List(T), Univ.Type), n, rm) = ty2s ((T, Univ.Type), 10, false)^" list"

    | ty2s (p as (Pi (Range.Prop, S, T), Univ.Prop), n, rm) = 
        if n>=2 then parT p
        else ty2s ((S, Univ.Prop), 2, false)^" => "^ty2s ((T, Univ.Prop), 1, rm)
    | ty2s (p as (Pi (Range.Dep x, S, T), Univ.Prop), n, rm) = 
       (case rm of
          false => parT p
	| true  => "!"^x^":"^ty2s ((S, Univ.Type), 0, true)^". "^ty2s ((T, Univ.Prop), 0, true))

    | ty2s (p as (Sigma (Range.Prop, S, T), Univ.Prop), n, rm) = 
        if n>=8 then parT p
        else ty2s ((S, Univ.Prop), 8, false)^" & "^ty2s ((T, Univ.Prop), 7, rm)
    | ty2s (p as (Sigma (Range.Dep x, S, T), Univ.Prop), n, rm) = 
       (case rm of
          false => parT p
	| true  => "?"^x^":"^ty2s ((S, Univ.Type), 0, true)^". "^ty2s ((T, Univ.Prop), 0, true))

    | ty2s (p as (Sum (S, S'), Univ.Prop), n, rm) = 
        if n >= 5 then parT p
        else ty2s ((S, Univ.Prop), 5, false)^" | "^ty2s ((S', Univ.Prop), 4, rm)
     
    | ty2s (p as (App (A, M), Univ.Prop), n, rm) = 
        if n >= 10 then parT (p) 
        else ty2s ((A, Univ.Prop),9, false) ^ " " ^ tm2s (M, 10)
    | ty2s (p as (Equal (N, M), Univ.Prop), n, rm) = 
        tm2s (N, 5) ^ " = " ^ tm2s (M, 5)
    | ty2s (p as (Less (N, M), Univ.Prop), n, rm) = 
        tm2s (N, 5) ^ " < " ^ tm2s (M, 5)

    | ty2s (p as (Atom (X), Univ.Prop), n, rm) = X
    | ty2s (p as (Unit, Univ.Prop), n, rm) = "T"
    | ty2s (p as (Empty, Univ.Prop), n, rm) = "F"
    | ty2s (p as (Not (A), Univ.Prop), n, rm) = "~" ^ ty2s ((A, Univ.Prop),10, rm)
    | ty2s (p as (Equiv (A, B), Univ.Prop), n, rm) = if n >= 1 then parT p
        else ty2s ((A, Univ.Prop),1, false)^" <=> "^ty2s ((B, Univ.Prop), 1, rm)
    | ty2s (p, _, _) = "$BAD PROP/TYPE:"^toString p^"$"


  and parT ty = paren (ty2s (ty, 0, true))


  fun pretty (AU) = ty2s (AU, 0, true)

  fun termPretty (M) = tm2s (M, 0)

end (* structure UExp *)