(* $Id: proofs.sml,v 1.2 2002/10/24 19:25:48 abel Exp $ *)

(* Syntax for propositional proofs
 * used by extsyn.sml ...
 *)

signature PROOFS =
sig

  datatype Hyp = Hyp of Prop.Prop	(* H ::= A      *)

  datatype Assertion =			(* E ::=        *)
    Line of Prop.Prop			(*       A      *)
  | Frame of Hyp * Proof		(*     | [H; P] *)
  and Proof =				(* P ::=        *)
    Final of Assertion			(*     | E      *)
  | Step of Assertion * Proof		(*     | E; P   *)

end; (* signature PROOFS *)

structure Proofs :> PROOFS =
struct

  datatype Hyp = Hyp of Prop.Prop	(* H ::= A      *)

  datatype Assertion =			(* E ::=        *)
    Line of Prop.Prop			(*       A      *)
  | Frame of Hyp * Proof		(*     | [H; P] *)
  and Proof =				(* P ::=        *)
    Final of Assertion			(*     | E      *)
  | Step of Assertion * Proof		(*     | E; P   *)

end; (* structure Proofs *)
