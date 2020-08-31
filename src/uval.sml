(* $Id: uval.sml,v 1.1 2000/10/24 22:42:11 abel Exp $ *)

signature UVAL =
sig

  type UVal = Val.Val * Univ.Univ
  type Cxt = UVal Cxt.Cxt

  (* Evaluation:  evaluate as much as possible, not under lambda *)
  val eval : UExp.UExp * Val.Env -> UVal
  val reval : UVal -> UVal    

  (* Weak Head Normal Form:  evaluate as less as possible, whnf *)
  val whnf : UExp.UExp * Val.Env -> UVal
  val rwhnf : UVal -> UVal    

  (* Equality on Val and Exp.Exp *)
  val conv : int * UVal * UVal -> bool     (* test two whnfs for equality *)
  val eqExp' : Val.env * UExp.UExp * UExp.UExp -> bool
  val eqExpVal : Val.env * UExp.UExp * UVal -> bool

  val toString : Val.env * UVal-> string
  val pretty   : Val.env * UVal-> string

  val nat : UVal
  val bool : UVal

end (* signature UVAL *)


structure UVal :> UVAL = 
struct

  type UVal = Val.Val * Univ.Univ
  type Cxt = UVal Cxt.Cxt

  fun eval ((A, U), e) = (Val.eval (A, e), U)
  fun reval (V, U) = (Val.reval (V), U)

  fun whnf ((A, U), e) = (Val.whnf (A, e), U)
  fun rwhnf (V, U) = (Val.rwhnf (V), U)

  fun conv (i, (V, U), (V', U')) = U=U' andalso Val.conv (i, V, V')
  fun eqExp' (e, (A, U), (A', U')) = U=U' andalso Val.eqExp' (e, A, A')
  fun eqExpVal (e, (A, U), (V, U')) = U=U' andalso Val.eqExpVal (e, A, V)

  fun toString (e, (V, U)) = UExp.toString (Val.toExp (e, V), U) 
  fun pretty (e, (V, U)) = UExp.pretty (Val.toExp (e, Val.reval (V)), U) 
  
  val nat = (Val.nat, Univ.Type)
  val bool = (Val.bool, Univ.Type)

end (* structure UVal *)