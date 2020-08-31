structure Univ =
struct    
    
    datatype Univ =
	Prop
      | Type
    
end; (* structure Univ *)


structure Range =
struct    

(* Indicates what is instantiated and where *)
    datatype Range =
	Prop
      | Type of int list
    
end; (* structure Range *)


signature PTEXP =
sig

    (* Represents either a proposition or a type *)
    type PTExp
	
    (* Uninstantiated formulas *)
    type Formula
	
    (* Exposed "root" of formulas *)
    datatype Root =			    (* A :: =                        *)
	Pred of string * int		    (*        P                      *)
      | Sum of Formula * Formula	    (*      | A | B, s + t           *)
      | Pi of Range * Formula * Formula	    (*      | A & B, ?x:t.A, s * t   *)
      | Sigma of Range * Formula * Formula  (*      | A => B, !x:t.A, s -> t *)
      | Unit                                (*      | T, 1                   *)
      | Empty                               (*      | F, 0                   *)
	
    val eq : PTExp * PTExp -> bool
	
    val fold : Root -> Formula
    val unfold : Formula -> Root
	
end; (* signature PTEXP *)


structure PTExp :> PTEXP =
struct

    (* Uninstantiated formulas *)
    datatype Formula = Label of int
	
    datatype PosExpContext = PosExpContext of int -> Exp option
	
    (* Represents either a proposition or a type *)
    datatype PTExp = Formula * PosExpContext * Univ
	
    (* Exposed "root" of formulas *)
    datatype Root =			   (* A :: =                        *)
	Pred of string * int		   (*        P                      *)
      | Sum of Formula * Formula	   (*      | A | B, s + t           *)
      | Pi of Range * Formula * Formula    (*      | A & B, ?x:t.A, s * t   *)
      | Sigma of Range * Formula * Formula (*      | A => B, !x:t.A, s -> t *)
      | Unit                               (*      | T, 1                   *)
      | Empty                              (*      | F, 0                   *)


end; (* structure PTExp *)
