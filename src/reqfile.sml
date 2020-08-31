signature REQFILE =
sig

  (* Constructors *)
  val set: string -> unit
  val readIfValidForSubmission: string * bool -> unit

  (* Selektors *)
  val isLocal: unit -> bool
  val name : unit -> string
  val kernel: unit -> string

  (* Methods *)
  val read : unit -> unit
  val checkIfValidForSubmission : bool -> unit 


end (* signature REQFILE *)


structure ReqFile :> REQFILE =
struct

  val reqExt = "req"

  structure Data =
  struct

    val isLocal = ref false
    val name = ref ""
    val kernel = ref ""

  end

  exception Error of string

  fun isLocal () = !Data.isLocal
  fun name    () = !Data.name
  fun kernel  () = !Data.kernel

  fun isExplicitelyRelative [] = false
    | isExplicitelyRelative (arc :: arcs) = 
      arc = OS.Path.parentArc orelse arc = OS.Path.currentArc

  fun set (fileName) =
    let val {isAbs=isAbs, vol=vol, arcs=arcs} = OS.Path.fromString (fileName)
	val extName = OSLib.addFileExt (fileName, reqExt)
    in
      Data.isLocal := (isAbs orelse isExplicitelyRelative (arcs));
      Data.name    := (if isLocal() then extName 
		      else OS.Path.concat (Global.reqPath, extName));
      Data.kernel  := (OSLib.fileNameKernel (name()))
    end

  fun noReqMsg () = if isLocal() 
        then "Cannot open local requirements file "^name()
        else "Cannot open global requirements file "^name() ^"\n"
  fun readReqMsg () = if isLocal() 
        then "Cannot read local requirements file "^name()
        else "Cannot read global requirements file "^name() ^"\n"

  fun error (msg) = raise Global.Error (if isLocal() then msg else msg^
        "Make sure:\n" ^
        "- you spelled the file name right\n" ^
        Global.msgCheckAccess ^
        Global.msgContactAdmin, Global.exitSpec)

  fun fileOpenMsg () =
      if Chatter.actions() then print ("[Opening requirements file " ^ name() ^ "]\n")
      else ();
  fun fileCloseMsg () =
      if Chatter.actions() then print ("[Closing requirements file " ^ name() ^ "]\n")
      else ();

  fun read () = 
      let val instream = TextIO.openIn (name())
	  handle IO.Io (ioError) => raise Error (noReqMsg())
	  val _ = fileOpenMsg()
          val _ = Region.resetLines (name())
	  val _ = SpecCheck.init (ParseSpec.parseStream (instream))
	  handle IO.Io (ioError) => raise Error (readReqMsg())
	  val _ =  fileCloseMsg ()
	  val _ = TextIO.closeIn instream
      in () end handle Error msg => error (msg)

  fun checkIfValidForSubmission (submit) = 
        if submit andalso isLocal() then 
          raise Global.Error ("Cannot use local requirements file for submission", Global.exitCmdLine) 
        else ()

  fun readIfValidForSubmission (reqFileName, submit) = 
       (set (reqFileName);
        checkIfValidForSubmission (submit);
        read())
						       
end (* structure ReqFile *)
