signature FOL = 
sig

  (* We define first-order terms and formulas *)

  (* Need: - signatures
           - contexts
           - function symbols/parameters
           - bound variables and binders
           - places holders
           - unification variables with context
  *)

  type Signature
  type Context

  datatype Symbol =
      (* Propositions *)
      Forall
    | Exists
    | And
    | Or
    | Implies
    | True
    | False
      (* Types *)
    | Prod
    | Sum
    | Arrow
    | Unit
    | Empty
    | Nat
    | Bool 
    | List
    | Pred of int
    | Func of int

  val arity : Symbol -> int

  datatype Exp =
      Var of int
    | Lift of int
    | Bind of string * Exp
    | Sym of Symbol * Exp list
  (*  | EVar of ??? ref *)

  (* Var(i) : Exp
     i < 0  : Free variable (which get bound)
     i >=0  : Unification variable or position variable for SubstTree 
     (For each unification variable we need a context in which the term
      that is substituted for the unification variable is typable(valid).
      This could representen by a Signature list.

     Lift(i,M) : Exp
     i < 0  : Consider i to be added to all free variables in M.
     i >=0  : Consider i to be added to all unification variables in M .
   
     Bind(x,M) : Exp
     Bind variable -1 in M. Consider all free variables increased by 1.

     Sym(C,Ms) : Exp
     Any connective C with subexpressions Ms
  *)


  (* need Unification with undo! *)

  type Interval = int * int

  type Subst = Interval * (int -> Exp)

  val subst : Exp * Subst -> Exp
  (* val comb  : Subst * Subst -> Subst *)

  datatype MSCGResult =
      Disjoint  (* Case 1 *)
    | Instance of Subst  (* Case 2 *)
    | Match of Subst     (* Case 3,4 *)
    | Split of Subst * Subst * Subst (* Case 5 *)
  
  val mscg :  Subst * Subst -> Subst * Subst * Subst
  (* mscg(S1,S2) = (T,T1,T2) 
     Invariants:
     S1 = comb(T,T1)
     S2 = comb(T,T2)
     T is most specific

     Case 1:
     If T is the identity substitution ID then S1=T1 and S2=T2 
     and S1,S2 have nothing in common. This case has to be caught.
     If this case appears in the insertion procedure, then S2 has
     to be inserted as a brother of S1 (if S2 has nothing in common
     with all brothers of S1).
     Return: Disjoint

     Case 2:
     If S1=T and T1=ID then S2 is an instance of S1. This case also
     has to be treated separately.
     insert or match: continue with childs of S1
     Return: Instance(T2)

     Case 3:
     If S2=T and T2=ID then S1 is an instance of S2. This can happen 
     if S1 is a leaf and S2 has unification variables (match procedure).
     Return: Match(T1)

     Case 4: (special case of 3)
     If S1=S2=T and T1=T2=ID then both substitutions are equal.
     insert: do nothing
     match:  exact match (no unification variables)
     Return: Match(ID)

     Case 5:
     General result.
     insert: Split S1, make T1, T2 childs.
     match:  fail.
     Return: Split(T,T1,T2)
   *)

end (* signature FOL *)