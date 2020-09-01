signature GLOBAL =
sig

  type result = OS.Process.status

  exception Error of string * result

  val reqPath : string
  val submitPath : string

  val course : string
  val administrators : string
  val submissionEmail : string

  val msgCheckRegistered : string
  val msgCheckAccess : string
  val msgContactAdmin : string

  val exitOK             : result
  val exitInternalError  : result
  val exitSysError       : result
  val exitCmdLine        : result
  val exitParsing        : result
  val exitProofInvalid   : result
  val exitUnjustified    : result
  val exitWrongGoal      : result
  val exitTermCheck      : result
  val exitSpec           : result
  val exitSpecIncomplete : result
  val exitSubmission     : result

  val exitMin :  result *  result ->  result

  val isLocalPath : string -> bool

end (* signature GLOBAL *)

