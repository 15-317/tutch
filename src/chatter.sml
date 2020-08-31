signature CHATTER =
sig

    type chatter

    val  min : chatter  (* error messages and overall result *)
    val  few : chatter  (* actions *)
    val  med : chatter  (* final results *)
    val  std : chatter  (* error details (hints) *)
    val  more: chatter  (* declaration details *) 
    val  max : chatter


    val  set : chatter -> unit

    val  declDetails : unit -> bool
    val  actions     : unit -> bool
    val  finalResults: unit -> bool
    val  errorDetails: unit -> bool

end (* signature CHATTER *)


structure Chatter :> CHATTER =
struct

    type chatter = int

    val min = 0
    val few = 2000
    val med = 5000
    val std = 6000
    val more= 7000
    val max = 9999

    val chatter = ref std

    fun set (level) = chatter := level
    
    fun declDetails ()  = !chatter >= more
    fun actions ()      = !chatter >= few
    fun finalResults () = !chatter >= med
    fun errorDetails () = !chatter >= std

end (* structure Chatter *)

