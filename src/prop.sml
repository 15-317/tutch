(* $Id: prop.sml,v 1.1 2000/10/17 22:23:12 abel Exp $ *)

signature PROP =
sig

  datatype Prop =			(* A ::=         *)
    Atom of string			(*       P       *)
  | And of Prop * Prop			(*     | A & B   *)
  | Or of Prop * Prop			(*     | (A | B) *)
  | Implies of Prop * Prop		(*     | A => B  *)
  | True				(*     | T       *)
  | False				(*     | F       *)
  | Not of Prop           		(*     | ~ A     *)
  | Equiv of Prop * Prop		(*     | A <=> B *)

  val notDef : Prop -> Prop		(* ~A = (A => F) *)
  val equivDef : Prop * Prop -> Prop	(* A <=> B = (A => B) & (B => A) *)

  val eq : Prop * Prop -> bool

  val toString' : Prop -> string  (* even outer parentheses *)
  val toString : Prop -> string
  val prettyString : Prop -> string

end; (* signature PROP *)

structure Prop :> PROP =
struct

  datatype Prop =			(* A ::=         *)
    Atom of string			(*       P       *)
  | And of Prop * Prop			(*     | A & B   *)
  | Or of Prop * Prop			(*     | (A | B) *)
  | Implies of Prop * Prop		(*     | A => B  *)
  | True				(*     | T       *)
  | False				(*     | F       *)
  | Not of Prop           		(*     | ~ A     *)
  | Equiv of Prop * Prop                (*     | A <=> B *)

  fun notDef (A) = Implies (A, False)
  fun equivDef (A, B) = And (Implies (A, B), Implies (B, A))

  fun eq (Atom P, Atom Q) = (P = Q)
    | eq (And (A, B), And (C, D)) = eq (A, C) andalso eq (B, D)
    | eq (Or (A, B), Or (C, D)) = eq (A, C) andalso eq (B, D)
    | eq (Implies (A, B), Implies (C, D)) = eq (A, C) andalso eq (B, D)
    | eq (True, True) = true
    | eq (False, False) = true
    | eq (Not (A), Not (B)) = eq (A, B)	(* shortcut *)
    | eq (Equiv (A, B), Equiv (C, D)) = eq (A, C) andalso eq (B, D) (* shortcut *)
    | eq (Not (A), B) = eq (notDef (A), B)
    | eq (A, Not (B)) = eq (A, notDef (B))
    | eq (Equiv (A, B), C) = eq (equivDef (A, B), C)
    | eq (A, Equiv (B, C)) = eq (A, equivDef (B, C))
    | eq _ = false

  fun parens (s) = "(" ^ s ^ ")"

  fun toString' (Atom (P)) = P
    | toString' (And (A, B)) = parens (toString' A ^ " & " ^ toString' B)
    | toString' (Or (A, B)) = parens (toString' A ^ " | " ^ toString' B)
    | toString' (Implies (A, B)) = parens (toString' A ^ " => " ^ toString' B)
    | toString' (True) = "T"
    | toString' (False) = "F"
    | toString' (Not (A)) = "~" ^ (toString' A)
    | toString' (Equiv (A, B)) = parens (toString' A ^ " <=> " ^ toString' B)

  fun toString (Atom (P)) = P
    | toString (And (A, B)) = toString' A ^ " & " ^ toString' B
    | toString (Or (A, B)) = toString' A ^ " | " ^ toString' B
    | toString (Implies (A, B)) = toString' A ^ " => " ^ toString' B
    | toString (True) = "T"
    | toString (False) = "F"
    | toString (Not (A)) = "~" ^ (toString' A)
    | toString (Equiv (A, B)) = toString' A ^ " <=> " ^ toString' B

(* Operator precedence
**
** Not > And > Or > Imp > Equiv
**  5     4     3    2      1
**
** And, Or, Implies are right associative,
** Equiv is non-associative
*)
  (* pretty (A, n) = "...", n binding strength of operator to left *)
  fun pretty (Atom (P), n) = P
    | pretty (C as And (A, B), n) = if n>4 then par (C) 
	else pretty (A, 5) ^ " & " ^ pretty (B, 4)
    | pretty (C as Or (A, B), n) = if n>3 then par (C)
	else pretty (A, 4) ^ " | " ^ pretty (B, 3)
    | pretty (C as Implies (A, B), n) = if n>2 then par (C)
	else pretty (A, 3) ^ " => " ^ pretty (B, 2) 
    | pretty (True, n) = "T"
    | pretty (False, n) = "F"
    | pretty (Not (A), n) = "~" ^ pretty (A, 5)
    | pretty (C as Equiv (A, B), n) = if n>1 then par (C)
	else pretty (A, 2) ^ " <=> " ^ pretty (B, 2)
  and par (C) = parens (pretty (C, 0))

  fun prettyString (C) = pretty (C, 0)

end; (* structure Prop *)
