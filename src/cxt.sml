(* $Id: cxt.sml,v 1.1 2000/10/17 22:23:08 abel Exp $ *)

signature CXT =
sig

  datatype 'a Cxt = 
      Empty
    | Ext of string * 'a * 'a Cxt

  val lookup : string * 'a Cxt -> 'a option
  val length : 'a Cxt -> int

end (* signature CXT *)

structure Cxt :> CXT =
struct

  datatype 'a Cxt = 
      Empty
    | Ext of string * 'a * 'a Cxt

  fun lookup (x, Empty) = NONE
    | lookup (x, Ext (y, A, G)) = if x=y then SOME A else lookup (x, G)

  fun length (Empty) = 0
    | length (Ext (x, A, G)) = 1 + length (G)

end (* structure Cxt *)