(* $Id: regtree.sml,v 1.1 2000/10/17 22:23:12 abel Exp $ *)

signature REGTREE =
sig

  (* Region trees *)
  (* corresponding to the data structures of tutch *)
  
  (* The position of Region in the products tells what is covered by that *)
  (* region. All trees right of the occurrence have to be joined with the *)
  (* region. *)

  datatype Exp =
      Atom of Region.Region                    (* atom, variable *)
    | Infix of Exp * Exp                (* infix operator *)
    | PFix  of Region.Region * Exp             (* pre or postfix operator *)
    | Quant of Exp * Region.Region * Exp       (* quantifier *)
    | Tern  of Exp * Exp * Exp * Region.Region (* case, rec *)
    | Tern' of Exp * Exp * Region.Region * Exp (* if *)

  val toRegion : Exp -> Region.Region

end (* signature REGTREE *)


structure RegTree :> REGTREE =
struct

  open Region

  (* Region trees *)
  (* corresponding to the data structures of tutch *)
  
  (* The position of Region in the products tells what is covered by that *)
  (* region. All trees right of the occurrence have to be joined with the *)
  (* region. *)

  datatype Exp =
      Atom of Region                    (* atom, variable *)
    | Infix of Exp * Exp                (* infix operator *)
    | PFix  of Region * Exp             (* pre or postfix operator *)
    | Quant of Exp * Region * Exp       (* quantifier *)
    | Tern  of Exp * Exp * Exp * Region (* case, rec *)
    | Tern' of Exp * Exp * Region * Exp (* if *)

  fun toRegion (Atom (r)) = r
    | toRegion (Infix (m, n)) = join (toRegion (m), toRegion (n))
    | toRegion (PFix (r, m)) = join (r, toRegion (m))
    | toRegion (Quant (m, r, n)) = join (r, toRegion (n))
    | toRegion (Tern (m, n, oo, r)) = r
    | toRegion (Tern' (m, n, r, oo)) = join (r, toRegion (oo)) 

end (* structure RegTree *)
