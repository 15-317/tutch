(* $Id: top.sml,v 1.7 2002/10/24 19:25:49 abel Exp $ *)

signature TOP =
sig

  val top : string * string list -> OS.Process.status
  val debug : string list -> OS.Process.status

end; (* signature TOP *)

structure Top :> TOP =
struct

  val version = "0.52 beta, $Date: 2002/10/24 19:25:49 $"
  val tutchVersion = "TUTCH "^version
  val submitVersion = "SUBMIT "^version
  val tutExt = "tut"
  val valExt = "val"

  (*  saveValidation (fileName)
  *)
  fun saveValidation (fileName) =
      let val out = TextIO.openOut (fileName)
	  val _ = TextIO.output (out, SpecCheck.getStatus ())
	  val _ = TextIO.closeOut (out)
      in () end

  fun fileOpenMsg (fileName) =
      if Chatter.actions() then print ("[Opening file " ^ fileName ^ "]\n")
      else ();
  fun fileCloseMsg (fileName) =
      if Chatter.actions() then print ("[Closing file " ^ fileName ^ "]\n")
      else ();

(*
  fun checkProofResultToStatus (CheckProof.OK) = Global.exitOK
    | checkProofResultToStatus (CheckProof.Invalid) = Global.exitProofInvalid
    | checkProofResultToStatus (CheckProof.Unjustified) = Global.exitUnjustified
    | checkProofResultToStatus (CheckProof.WrongGoal) = Global.exitWrongGoal
*)

  fun checkStream (s) =
      let fun check (s, status) = check' (Stream.expose s, status)
	  and check' (Stream.Empty, status) = status
	    | check' (Stream.Cons (D, s), status) = 
(*
	      let val result  = Decl.check (D)
		  val status' = checkProofResultToStatus (result)
*)
	      let val status' = Decl.check (D)
	      in 
		  check (s, Global.exitMin (status, status'))
	      end
      in check (s, Global.exitOK) end

  fun readFile (fileName) =
      let
	val instream = TextIO.openIn fileName
	val _ = fileOpenMsg (fileName)
        val _ = Region.resetLines (fileName)
	val status = checkStream (Parser.parseStream instream)
	val _ = fileCloseMsg (fileName)
	val _ = TextIO.closeIn instream
      in status end

  fun readFiles' ([], status) = status
    | readFiles' (fileName::fileNames, status) = 
      let val status' = readFile (fileName)
      in
	  readFiles' (fileNames, Global.exitMin (status, status'))
      end

  fun readFiles (fileNames) = readFiles' (fileNames, Global.exitOK)

  fun printVersion (false) =
      if Chatter.actions() then print (tutchVersion ^ "\n") else ()
    | printVersion (true) =
      if Chatter.actions() then print (submitVersion ^ "\n") else ();
   
  (* MAIN ACTIONS: help, validate, run *)

  val optionsQVH = 
    "  -q | -Q  Be quiet | really quiet\n" ^ 
    "  -v | -V  Be verbose | maximum verbose\n" ^
    "  -h       Print this help message\n"
  val defaultExt =
    "Default extensions:\n" ^
    "  .tut     for proof files\n" ^
    "  .req     for requirement files\n" ^
    "Any file without extension will be given the proper extension automatically.\n"
  val helpOptionS = 
    "  -s file  Checks against `file` and submits files\n" ^
    "  -S       Schedules for submission. Only valid with -r\n"
  
  (*  help 
  *)

  fun helpTutch () = (print (
    "Usage: tutch [options] files\n" ^
    "Options:\n" ^
    "  -r file  Checks files against requirements in 'file'\n" ^
    optionsQVH ^
    defaultExt ^
    "If no files are given, but a requirements file 'file[.req]' is specified,\n" ^
    "'file.tut' will be checked against 'file.req'.\n" ^
    "Warning: This command DOES NOT SUBMIT your files!\n"); 
    Global.exitCmdLine
    ) 

  fun helpSubmit() = (print (
    "Usage: submit [options] -r file [files]\n" ^
    "Checks files against requirements in 'file' and submits the results.\n" ^
    "If no files are given, 'file.tut' will be checked against 'file.req'.\n" ^
    "Options:\n" ^
    optionsQVH
    ); 
    Global.exitCmdLine
    ) 

  fun help (submit, msgOpt) = 
       (printVersion (submit);
	case msgOpt of
	    NONE => ()
          | SOME msg => print (msg ^ "\n");
        if submit then helpSubmit() else helpTutch()) 


  fun abort (status) = (print "\nAborted\n"; status)
	
  (*  validate    
  *)
  fun validateSubmission (name) =
      let 
	  val _       = if Chatter.actions() then print (
            "Validating submission '"^name^"' in current directory\n") else ()
          val _       = ReqFile.readIfValidForSubmission (name, true)
 	  val files   = OSLib.allFiles (name)
          val _       = readFiles (files) 
	  val _       = SpecCheck.printStatus()
	  val valName = OSLib.addFileExt (name, valExt)
	  val _       = saveValidation (valName)
	  val _       = if Chatter.actions() then print (
            "Validation of submission saved in file "^valName^"\n") else ()
      in OS.Process.success end
      
  (*  run : string list * string * bool -> OS.Process.status
  *)
  fun submitStartMsg (files) = if Chatter.actions() then 
          (if Chatter.finalResults() then print "\n" else ();
	   print "[Submitting files: "; 
	   app (fn s => print (s ^ " ")) files;
	   print "]\n")
	else ()

  fun submitEndMsg () = if Chatter.actions() then print "[Submission OK]\n"
			else ()

  (*  run: Submission handling *) 
  fun run (files, reqKern, submit) = (printVersion (submit); run' (files, reqKern, submit))
  and run' (files, "", true) = raise Global.Error 
        ("No requirements file given for submission", Global.exitCmdLine)
    | run' (files, "", false) = run2 (files)
    | run' (files, reqFile, false) = run1 (files, reqFile, false)
    | run' (files, reqFile, true) = 
	let val status = run1 (files, reqFile, true)
	    val _ = submitStartMsg (files)
	    val _ = Submit.submit (ReqFile.kernel(), files)
	    val _ = submitEndMsg ()
        in status end

  (*  run1: Requirement file handling *)
  and run1 (files, "", submit) = run2 (files)
    | run1 (files, reqFile, submit) = (
        ReqFile.readIfValidForSubmission (reqFile, submit);
        run2 (files))

  (*  run2: Tutch files handling *)
  and run2 (files) = 
      let val status = readFiles (files)
	  val _ = SpecCheck.printStatus()
	  val status' = if SpecCheck.done() then Global.exitOK else Global.exitSpecIncomplete
      in 
	  Global.exitMin (status, status') 
      end


  (* parseArgs : string list -> Action
     
     side effect: sets Chatter
  *)

  datatype Action =
      Help of bool * string option
    | Run of string list * string * bool
    | Validate of string

  local 

    val reqFile = ref ""
    val submit = ref false
    
    fun setReqFile (name) = 
	if !reqFile = "" then reqFile := name
	else if !reqFile = name then ()
	     else raise Global.Error ("Duplicate specification of requirements file", Global.exitCmdLine)

    fun returnRun (files) =  Run (List.rev files, !reqFile, !submit)

    fun parseArgs' ([], []) = if !reqFile = "" then Help (!submit, NONE)
			     else returnRun [OSLib.addFileExt (OSLib.fileNameKernel (!reqFile), tutExt)]
      | parseArgs' ([], files) = returnRun (files)
      | parseArgs' ("" :: args, files) = parseArgs' (args, files)
      | parseArgs' ("-h" :: args, files) = Help (!submit, NONE)
      | parseArgs' ("--help" :: args, files) = Help (!submit, NONE)
      | parseArgs' ("-q" :: args, files) = (Chatter.set (Chatter.few); parseArgs' (args, files))
      | parseArgs' ("--quiet" :: args, files) = (Chatter.set (Chatter.few); parseArgs' (args, files))
      | parseArgs' ("-Q" :: args, files) = (Chatter.set (Chatter.min); parseArgs' (args, files))
      | parseArgs' ("--Quiet" :: args, files) = (Chatter.set (Chatter.min); parseArgs' (args, files))
      | parseArgs' ("-v" :: args, files) = (Chatter.set (Chatter.more); parseArgs' (args, files))
      | parseArgs' ("--verbose" :: args, files) = (Chatter.set (Chatter.more); parseArgs' (args, files))
      | parseArgs' ("-V" :: args, files) = (Chatter.set (Chatter.max); parseArgs' (args, files))
      | parseArgs' ("--Verbose" :: args, files) = (Chatter.set (Chatter.max); parseArgs' (args, files))
      | parseArgs' ("-r"::name::args, files) = (setReqFile (name);
          parseArgs' (args, files))
      | parseArgs' ("--requirements"::name::args, files) = 
          (setReqFile (name); parseArgs' (args, files))
      | parseArgs' ("--validate"::name::args, files) = Validate (name)
      | parseArgs' (name::args, files) = if String.sub (name, 0) <> #"-" 
	  then parseArgs' (args, OSLib.addFileExt (name, tutExt) :: files)
	  else Help (!submit, SOME ("Invalid option: "^name))
    
  in

    fun parseArgs (args) =
	(case SMLofNJ.SysInfo.getOSKind()
	   of SMLofNJ.SysInfo.WIN32 => parseArgs' (tl args, [])
	    | _ => parseArgs' (args, []))

  end (* local *)

  (*  top : string * string list -> OS.Process.status
  *)
  fun top (name, args) =
    let val action = parseArgs (args) 
    in 
      case action of
	  Help (submit, msgOpt) => help (submit, msgOpt)
	| Validate (name) => validateSubmission (name)
	| Run (files, reqKern, submit) => run (files, reqKern, submit)
    end 
      handle Global.Error (msg, status) => (print ("Error: "^msg); abort(status))
	   | Parsing.Error (msg) => (print msg; abort (Global.exitParsing))
	   | IO.Io (ioError) => (print (OSLib.ioErrorToString (ioError)); abort(Global.exitSysError))
	   | OS.SysErr (msg, _) => (print ("System error: "^msg); abort(Global.exitSysError)) 
	   | _ => (print ("Unrecognized exception"); abort(Global.exitInternalError))

  (*  debug : string * string list -> OS.Process.status
      do not catch exceptions
  *)
  fun debug (args) =
    let val action = parseArgs (args) 
    in 
      case parseArgs (args) of
	  Help (submit, msgOpt) => help (submit, msgOpt)
	| Validate (name) => validateSubmission (name)
	| Run (files, reqKern, submit) => run (files, reqKern, submit)
    end 

end; (* structure Top *)
