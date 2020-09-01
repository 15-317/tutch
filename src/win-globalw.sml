structure Global :> GLOBAL =
struct

  type result = OS.Process.status

  exception Error of string * result

  (* This won't work, but at least it will be clear what it's trying to do. *)
  val reqPath = "\\afs\\andrew.cmu.edu\\course\\15\\317\\req\\"

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
