structure Global :> GLOBAL =
struct

  type result = OS.Process.status

  exception Error of string * result

  val reqPath = "/afs/andrew.cmu.edu/course/15/317/req/"
  val submitPath = "/afs/andrew.cmu.edu/course/15/317/submit/"

  val course = "15-317"
  val administrators = "https://piazza.com/class/j6v67lt0h6b3xx"
  val submissionEmail = "rkavanagh@cs.cmu.edu"

  val msgCheckRegistered =
    "- you are registered student of course " ^ course ^ "\n" ^
    "- you registered with your Andrew ID for this course\n"
  val msgCheckAccess =
    "- your Kerberos tickets are not expired\n" ^
    "- you have access to the Andrew File System (/afs/andrew.cmu.edu) from your\n" ^
    "  machine right now\n"
  val msgContactAdmin =
    "If you made sure the fault is not on your side, please contact the course\n" ^
    "administrators now via a private post on Piazza ( " ^ administrators ^" ).\n" ^
    "Please supply the error message above, your Andrew ID and the command line.\n"

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
