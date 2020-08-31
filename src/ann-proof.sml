(* $Id: ann-proof.sml,v 1.1 2000/10/17 22:23:07 abel Exp $ *)

signature ANNPROOF =
sig

  datatype Hyp = 
      Hyp of string * Prop.Prop  	(* H ::= A      *)

  datatype Assertion =			(* E ::=        *)
      Line of Term.Term * Prop.Prop	(*       A      *)
    | Frame of Hyp * Proof		(*     | [H; P] *)
  and Proof =				(* P ::=        *)
      Final of Assertion		(*     | E      *)
    | Step of Assertion * Proof		(*     | E; P   *)

end; (* signature ANNPROOF *)

structure AnnProof :> ANNPROOF =
struct

  datatype Hyp = 
      Hyp of string * Prop.Prop  	(* H ::= A      *)

  datatype Assertion =			(* E ::=        *)
      Line of Term.Term * Prop.Prop	(*       A      *)
    | Frame of Hyp * Proof		(*     | [H; P] *)
  and Proof =				(* P ::=        *)
      Final of Assertion		(*     | E      *)
    | Step of Assertion * Proof		(*     | E; P   *)

end; (* structure AnnProof *)
