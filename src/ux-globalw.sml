structure Global :> GLOBAL =
struct

  type result = OS.Process.status

  exception Error of string * result

  val reqPath = "/afs/andrew.cmu.edu/course/15/317/req/"

  val exitOK = 0                   (* OK                                *)
  val exitInternalError = 1        (* uncaught exception, bug in Tutch  *)
  val exitSysError = 2             (* file not found, access denied etc *)
  val exitCmdLine = 3              (* invalid command line syntax       *)
  val exitParsing = 4              (* lexing or parsing error           *)
  val exitProofInvalid = 5         (* proof malformed, like [A;[B;B]]   *)
  val exitUnjustified = 6          (* proof contains unjustified lines  *)
  val exitWrongGoal = 7            (* goal does not match proof         *)
  val exitSpec = 8                 (* requirements file not found       *)
  val exitSpecIncomplete = 9       (* not all req. solved               *)
  val exitSubmission = 10          (* submission failed                 *)
  val exitTermCheck = 11           (* term does not check               *)

  fun exitMin (0, status) = status
    | exitMin (status, 0) = status
    | exitMin (status, status') = Int.min (status, status')

  fun isLocalPath name =
    let val {isAbs=isAbs, vol=vol, arcs=arcs} = OS.Path.fromString name
    in
       isAbs
       orelse
       (case arcs of
           [] => false
         | arc :: _ =>
              arc = OS.Path.parentArc orelse arc = OS.Path.currentArc)
    end

end (* structure GLOBAL *)
