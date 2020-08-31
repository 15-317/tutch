signature SUBMIT =
sig

  val submit : string * string list -> unit (* raises Global.Error *)

  val printStatus : string -> unit (* prints out submission status *)

end (* signature SUBMIT *)


structure Submit :> SUBMIT =
struct

  val subExt = "sub"

  exception Error of string

  fun userName () = case OS.Process.getEnv ("USER") of
          NONE => raise Error "No user name in environment."
        | SOME user => user

  (*  copyFiles (files, destDir)

      Condition: files<>[]

      This function is OS-DEPENDENT!
  *)
  fun copyFiles (files, destDir) = 
      let val filesStr = List.foldr (fn (f, s) => f ^ " " ^ s) ""  files
          val cmdStr = "cp -p "^filesStr^destDir
          val status = OS.Process.system (cmdStr)
      in 
	  if status <> OS.Process.success then raise Error 
             "Cannot submit tutch files."
          (* ("Could not submit files. Execution of `" ^ cmdStr ^ "` failed")*)
	  else () 
      end

  (*  saveSpecStatus
  *)
  fun saveSpecStatus (reqKern, user, fileName) =
      let val out = TextIO.openOut (fileName)
	      handle IO.Io (ioError) => raise Error ("Cannot create submission for student '"^user^"'")
          val header = "Assignment: " ^ reqKern ^ "\n" 
                     ^ "Student ID: " ^ user    ^ "\n"
                     ^ "Date:       " ^ OSLib.date()  ^ "\n"
                     ^ "\n"
	  val _ = TextIO.output (out, header)
	  val _ = TextIO.output (out, SpecCheck.getStatus ())
	  val _ = TextIO.closeOut (out)
      in () end handle IO.Io (ioError) => raise Error "Cannot write submission"

  (*  printSpecStatus
  *)
  fun printSpecStatus (reqKern, user, fileName) =
      let val instream = TextIO.openIn (fileName)
	      handle IO.Io (ioError) => raise Error ("Cannot read submission of student '"^user^"'")
          fun inputLine' s = case TextIO.inputLine(s) of
                               NONE => ""
                             | SOME s => s
	  fun cat ("")   = ()
	    | cat (line) = (print (line); cat (inputLine'(instream)))
	  val _ = cat (inputLine'(instream))
	  val _ = TextIO.closeIn (instream)
      in () end handle IO.Io (ioError) => raise Error "Cannot print submission"

  fun mkDirOpt (dir) = 
      let fun fail () = raise Error "Cannot create submission directory"
              (* ("Cannot create "^dir^": a file of that name exists") *)
      in  ((case OS.FileSys.isDir (dir) of
	      false => fail()
	    | true => ())
	    handle OS.SysErr _ => (OS.FileSys.mkDir (dir)))
          handle OS.SysErr _ => fail()
      end
          
  fun error (msg) = raise Global.Error (
    "Submission FAILED. Reason: " ^ msg ^ ".\n" ^
    "Make sure:\n" ^
    Global.msgCheckRegistered ^
    Global.msgCheckAccess ^
    "- you have not passed the submission deadline\n" ^
    Global.msgContactAdmin ^
    "If your are very close to the deadline, please follow this procedure:\n" ^
    "\n" ^
    "Emergency submission:\n" ^
    "Tar and gzip ALL of your submission files into one archive file. E.g.:\n" ^
    "    tar czf myfiles.tgz file1.tut file2.tut file3.tut\n" ^
    "Preferably name the archive <req>.tgz, where <req>.req is the current\n" ^
    "requirements file. Attach this file to an e-mail with subject\n" ^
    "    submission: <yourId>/<req> \n" ^
    "and send it to "^Global.submissionEmail^".\n", Global.exitSubmission)
 
  (*  submit (reqKern, files)

      Condition: files<>[]
                 submitPath/user/ exists

      writes evaluation to:      submitPath/user/regKern.sub
      copies submitted files to: submitPath/user/regKern/files      
  *)
  fun submit (reqKern, files) =
      let val user = userName()
          val userDir = OS.Path.concat (Global.submitPath, user)
	  val subFileName = OS.Path.joinBaseExt {base = reqKern, ext = SOME subExt }
	  val subFullName = OS.Path.concat (userDir, subFileName) 
          val _ = saveSpecStatus (reqKern, user, subFullName)
          val filesDir = OS.Path.concat (userDir, reqKern)
          val _ = mkDirOpt (filesDir)
          val _ = copyFiles (files, filesDir);
      in () end handle Error msg => error (msg)  

  (*  printStatus (reqKern)

  *)
  fun statusError (msg) = raise Global.Error (
    "No submission status available: " ^ msg ^ ".\n" ^
    "Make sure:\n" ^
(*    "- you have spelled the submission name right\n" ^ *)
    Global.msgCheckRegistered ^
    Global.msgCheckAccess ^
    "- you have not passed the submission deadline\n" ^
    Global.msgContactAdmin, Global.exitSubmission)

  fun printFile (fileName) =
      let val {dir=dir, file=file} = OS.Path.splitDirFile (fileName)
      in print (file) end

  fun printFileNames (reqKern, filesDir) =
      let val names = OSLib.allFiles (filesDir)
	  handle OS.SysErr _ => []
      in 
	  if names=[] then (print "No files submitted.\n"; false)
	  else (print "Submitted files: ";
		app printFile names;
		print "\n"; 
		true)
      end

  fun printStatus (reqKern) =  
      let val user = userName()
          val userDir = OS.Path.concat (Global.submitPath, user)
          val _ = OS.FileSys.isDir (userDir)
	      handle OS.SysErr _ => raise Error ("Cannot access submission structure of student '"^user^"'")
          val filesDir = OS.Path.concat (userDir, reqKern)
          val subExists = printFileNames (reqKern, filesDir)
	      handle OS.SysErr _  => raise Error ("Cannot read submitted files of student '"^user^"'")
	  val subFileName = OS.Path.joinBaseExt {base = reqKern, ext = SOME subExt }
	  val subFullName = OS.Path.concat (userDir, subFileName) 
          val _ = if subExists 
		      then printSpecStatus (reqKern, user, subFullName)
		  else ()
      in () end handle Error msg => statusError (msg)  
          
end (* structure Submit *)
