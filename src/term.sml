(* $Id: term.sml,v 1.1 2000/10/17 22:23:14 abel Exp $ *)

signature TERM =
sig

  datatype Term =
      Var of string                   (* M ::= x                         *)
    | Pair of Term * Term             (*     | (M, N)                    *)
    | Fst of Term                     (*     | fst M                     *)
    | Snd of Term                     (*     | snd M                     *)
    | Inl of Term                     (*     | inl M                     *)
    | Inr of Term                     (*     | inr M                     *)
    | Case of Term * bind * bind      (*     | case M of inl x => L | inr y => N end *)
    | Lam of bind                     (*     | fn x => M                 *)
    | App of Term * Term              (*     | M N                       *)
    | Star                            (*     | ()                        *)
    | Abort of Term                   (*     | abort M                   *)
    | Ann of Term * Prop.Prop         (*     | (M : A)                   *)

  withtype bind = string * Term       (* B :: = x => M *)

  val eq : Term * Term -> bool
  val toString : Term -> string

end (* signature TERM *)


structure Term :> TERM =
struct

  datatype Term =
      Var of string                   (* M ::= x                         *)
    | Pair of Term * Term             (*     | (M, N)                    *)
    | Fst of Term                     (*     | fst M                     *)
    | Snd of Term                     (*     | snd M                     *)
    | Inl of Term                     (*     | inl M                     *)
    | Inr of Term                     (*     | inr M                     *)
    | Case of Term * bind * bind      (*     | case M of inl x => L | inr y => N end *)
    | Lam of bind                     (*     | fn x => M                 *)
    | App of Term * Term              (*     | M N                       *)
    | Star                            (*     | ()                        *)
    | Abort of Term                   (*     | abort M                   *)
    | Ann of Term * Prop.Prop         (*     | (M : A)                   *)

  withtype bind = string * Term       (* B :: = x => M *)

  fun eq (M, N) = M = N 

(* binding strength
   10: app right and all prefix constructors
    9: app left
    7: cons right
    6: cons left
    1: lam
    0: case, ann
*)
local

  fun tm2s (tm as App (M, N), 10) = par (tm)
    | tm2s (tm as App (M, N),  n) = (tm2s (M, 9))^" "^(tm2s (N, 10))
    | tm2s (tm as Lam (B), n) = if n>=2 then par (tm)
				       else ("fn "^(bind2s B))
    | tm2s (tm as Var (x), n) = x
    | tm2s (tm as Case (M, B, B'), n) = if n>=2 then par (tm)
	else ("case "^(tm2s (M, 5))^" of inl "^(bind2s B)^" | inr "^(bind2s B')^" end")
    | tm2s (tm as (Pair (M, N)), n) = "("^(tm2s (M, 0))^", "^(tm2s (N, 0))^")"
    | tm2s (tm as (Fst (M)), n)  = if n>=9 then par (tm) else "fst "^(tm2s (M, 10))
    | tm2s (tm as (Snd (M)), n)  = if n>=9 then par (tm) else "snd "^(tm2s (M, 10))
    | tm2s (tm as (Inl (M)), n)  = if n>=9 then par (tm) else "inl "^(tm2s (M, 10))
    | tm2s (tm as (Inr (M)), n)  = if n>=9 then par (tm) else "inr "^(tm2s (M, 10))
    | tm2s (Star, n) = "()"
    | tm2s (tm as (Abort (M)), n)  = if n>=9 then par (tm) else "abort "^(tm2s (M, 10))
    | tm2s (tm as (Ann (M, A)), n) = if n>=1 then par (tm) else tm2s (M, 0) ^" : "^Prop.prettyString (A)


  and bind2s (x, M) = x^" => "^(tm2s (M, 1))
  and par (M) = "("^(tm2s (M, 0))^")"

in

  fun toString (M) = tm2s (M, 0)

end

end (* structure Term *)

