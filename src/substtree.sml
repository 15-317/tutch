signature SUBSTTREE = 
sig

   type SubstTree

   val insert : SubstTree * Subst -> SubstTree
   val delete : SubstTree * int -> SubstTree

   val match  : SubstTree * Exp * (Subst -> 'a) * (unit -> 'a) -> 'a
   val app    : (Exp -> unit) -> SubstTree -> unit

end (* signature SUBSTTREE *)

structure SubstTree =
struct

   datatype Node = 
       Node of int * Subst * Node list
   type SubstTree = int * Node list

     (* (n, nodes) : SubstTree   
        n = Number of entries
        nodes = list of childs of a hypothetical root of the SubstTree

        Node(k, S, nodes) : Node
        k = minimal number of an Exp that is store under this Node
            if k>=n then the Node is invalid
        S = substitution of this Node
        nodes = list of childs
     *)

   val rootExp = Exp.Var(0)
   
   fun app f (n, nodes) =
       let 
	   fun tn M (Node (k, S, [])) = if k>=n then ()      (* Leaf *)
                 else f (Exp.subst (M, S))
	     | tn M (Node (k, S, nodes)) = if k>=n then ()
		 else let 
			  val M' = Exp.subst (M, S)
		      in 
			  List.app (tn M') nodes
		      end
       in
	   List.app (tn rootExp) nodes
       end
 
end (* structure SUBSTREE *)