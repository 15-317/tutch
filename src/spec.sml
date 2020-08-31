(* $Id: spec.sml,v 1.3 2000/10/27 17:08:14 abel Exp $ *)

signature SPEC =
sig

  datatype Spec = 
      Proof of string * Exp.Exp
    | AnnProof of string * Prop.Prop
    | Exp of string * Exp.Exp
    | Term of string * Exp.Exp

  type specs = Spec list

  val toString: Spec -> string

end (* signature SPEC *)


structure Spec :> SPEC = 
struct

  datatype Spec = 
      Proof of string * Exp.Exp
    | AnnProof of string * Prop.Prop
    | Exp of string * Exp.Exp
    | Term of string * Exp.Exp

  type specs = Spec list

  fun toString (Proof (x, A)) = "proof " ^ x ^ ": " ^ Exp.propPretty(A)
    | toString (Term (x, A))  = "term " ^ x ^ ": "  ^ Exp.propPretty(A)
    | toString (AnnProof (x, A)) = "annotated proof " ^ x ^": " ^ Prop.prettyString (A)
    | toString (Exp (x, T))  = "val " ^ x ^ ": "  ^ Exp.typePretty(T)

end (* structure Spec *)
