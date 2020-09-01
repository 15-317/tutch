structure Global :> GLOBAL =
struct

  type result = OS.Process.status

  exception Error of string * result

  (* This won't work, but at least it will be clear what it's trying to do. *)
  val reqPath = "\\afs\\andrew.cmu.edu\\course\\15\\317\\req\\"
  val submitPath = "\\afs\\andrew.cmu.edu\\course\\15\\317\\submit\\"

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

  val fi = Word32.fromInt;
  val exitOK = fi(0)                   (* OK                                *)
  val exitInternalError = fi(1)        (* uncaught exception, bug in Tutch  *)
  val exitSysError = fi(2)             (* file not found, access denied etc *)
  val exitCmdLine = fi(3)              (* invalid command line syntax       *)
  val exitParsing = fi(4)              (* lexing or parsing error           *)
  val exitProofInvalid = fi(5)         (* proof malformed, like [A;[B;B]]   *)
  val exitUnjustified = fi(6)          (* proof contains unjustified lines  *)
  val exitWrongGoal = fi(7)            (* goal does not match proof         *)
  val exitSpec = fi(8)                 (* requirements file not found       *)
  val exitSpecIncomplete = fi(9)       (* not all req. solved               *)
  val exitSubmission = fi(10)          (* submission failed                 *)
  val exitTermCheck = fi(11)           (* term does not check               *)

  fun exitMin (0w0, status) = status
    | exitMin (status, 0w0) = status
    | exitMin (status, status') = Word32.min (status, status')

  (* We can't rely on OS.Path on Windows, so check it ourselves. *)
  fun isLocalPath name =
     (String.sub (name, 0) = #"/"
      orelse
      String.sub (name, 0) = #"\\"
      orelse
      String.substring (name, 0, 2) = "./"
      orelse
      String.substring (name, 0, 2) = ".\\"
      orelse
      String.substring (name, 0, 3) = "../"
      orelse
      String.substring (name, 0, 3) = "..\\")
     handle Subscript => false
     

end (* structure GLOBAL *)
