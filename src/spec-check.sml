(* $Id: spec-check.sml,v 1.3 2000/10/24 21:09:54 abel Exp $ *)

signature SPECCHECK = 
sig

  val init : Spec.specs -> unit
  val tally: Spec.Spec -> bool
  val done : unit -> bool
  val nonEmpty: unit -> bool
  val printStatus : unit -> unit
  val getStatus : unit -> string 
  
end (* signature SPECCHECK *)

  
structure SpecCheck =
struct
  
  open Spec

  datatype SpecBox = SpecBox of Spec * (bool ref)
  
  type specList = SpecBox list

  fun initSpecList ([]) = []
    | initSpecList (spec :: specs) = 
        SpecBox (spec, ref false) :: initSpecList (specs)

  fun tallySpec ([], spec) = false
    | tallySpec (SpecBox (Proof (x, A), r) :: specList, Proof (y, B)) = 
(*
	if x=y andalso Val.eqExp (A, B) then (r := true; true)
*)
	if x=y andalso A = B then (r := true; true)
	else tallySpec (specList, Proof (y, B))
    | tallySpec (SpecBox (AnnProof (x, A), r) :: specList, AnnProof (y, B)) = 
	if x=y andalso Prop.eq (A, B) then (r := true; true)
	else tallySpec (specList, AnnProof (y, B))
    | tallySpec (SpecBox (Term (x, A), r) :: specList, Term (y, B)) = 
	if x=y andalso A = B then (r := true; true)
	else tallySpec (specList, Term (y, B))
    | tallySpec (SpecBox (Exp (x, S), r) :: specList, Exp (y, T)) = 
(* does not work with polymorphism
	if x=y andalso Val.eqExp (S, T) then (r := true; true)
*)
	if x=y andalso S = T then (r := true; true)
	else tallySpec (specList, Exp (y, T))
    | tallySpec (SpecBox _ :: specList, spec) = tallySpec (specList, spec)

  fun allDone ([]) = true
    | allDone (SpecBox (spec, ref true) :: specList) = allDone (specList)
    | allDone (SpecBox (spec, ref false) :: specList) = false
(*
  fun printBox (SpecBox (spec, ref true)) =
        print (":o) " ^ Spec.toString (spec) ^"\n")
    | printBox (SpecBox (spec, ref false)) =
        print (":=( " ^ Spec.toString (spec) ^"\n")
*)
  fun boxToString (SpecBox (spec, ref true)) = "[X] "^Spec.toString (spec)^"\n"
    | boxToString (SpecBox (spec, ref false))= "[-] "^Spec.toString (spec)^"\n"

  local (* state *)
    
    val specList = ref ([] : specList)

  in

    fun init (specs) = (specList := initSpecList (specs))
    fun tally (spec) = tallySpec (!specList, spec)
    fun done () = allDone (!specList)
    fun nonEmpty () = !specList <> []

    fun getStatus () = foldr (fn (box, s) => boxToString (box) ^ s) "" (!specList)
    fun printStatus () = if nonEmpty() then (
          if Chatter.finalResults() then print "\n" else ();
          if Chatter.actions() then print "Checking requirements...\n" else();
          if Chatter.finalResults() then print (getStatus ()) else (); 
(*          if Chatter.finalResults() then app printBox (!specList) else (); *)
          if true then if done()  (* always print final message *)
              then print ("Congratulations! All problems solved!\n") 
              else print ("Go for the remaining problems!\n")
          else ()
        ) else ()


  end (* local *)

end (* structure SpecCheck *)
