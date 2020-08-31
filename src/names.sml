(* $Id: names.sml,v 1.1 2000/10/24 22:42:11 abel Exp $ *)

signature NAMES = sig

    type name
    type index
    type names

    val null        : name
    val nextInst    : name -> name
    (* nextInst provides the next instance to an instance of a name,
       e.g. 
       type name = string
       fun  nextInst n = concat [n, "'"]
       val  name = "x"
       val  inst = "x''"
       val  next = nextInst inst 
       next = "x'''"
    *)

    val empty       : names
    val add         : index * name * names -> names
    val get         : index * names -> name
    val findMaxInst : name * names -> name
    (* findMaxInst finds the maximal instance of a name in a names collection,
       e.g.
       type names = name list
       val  names = ["x","x'","y","x'''"]
       val  max = findMaxInst ("x'", names)
       max = "x'''"
    *)

(* possible extentions: *)
(*    val delLast     : names -> names *)
(*    val delIndex    : index * names -> names *)
(*    val lookup      : index * names * (name -> 'a) * (unit -> 'a) -> 'a *)
 end

structure Names : NAMES = struct

    type name = string
    type index = int
    type names = (int*string) list

    val null = ""
    fun nextInst x = concat [x, "'"]

    val empty = []
  
    fun add (i, x, names) = (i,x)::names
    fun get (i, []) = null
      | get (i, (j,x)::names) = if i=j then x else get (i, names)

    (* "varFamInst rep x"  checks wether "x" is "rep" followed by primes *)

    fun varFamInst rep x = 
      let
        fun isPrimes [] = true
	  | isPrimes (#"'" :: cs) = isPrimes cs
	  | isPrimes _ = false
	fun isInst [] cs = isPrimes cs
	  | isInst (b :: bs) (c :: cs) = b=c andalso isInst bs cs
	  | isInst bs [] = false
      in isInst (String.explode rep) (String.explode x) end

    (* "findMaxInst (name, names)  finds the maximum instance of name in names
       if no instance can be found, "null" is returned
    *)

    fun findMaxInst' (x, max, []) = max
      | findMaxInst' (x, max, (i,y)::ys) = 
	if varFamInst x y andalso (not (varFamInst y max)) then findMaxInst' (x, y, ys)
	else findMaxInst' (x, max, ys)

    fun findMaxInst (x, ys) = findMaxInst' (x, null, ys)

(* old code:

    fun lookup (i, [], sc, fc) = fc()
      | lookup (i, (j,x)::names, sc, fc) = if (i=j) then sc x else lookup (i, names, sc, fc)
    fun delLast [] = []
      | delLast ((i,x)::names) = names

fun freeInst (x, ys) = 
  let val max = findMaxInst (x, ys)
  in if max="" then x else nextInst max end
*)

end

