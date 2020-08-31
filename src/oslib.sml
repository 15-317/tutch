signature OSLIB = 
sig

  (*  A small library of OS-independent functions *)

  (*  allFiles(dir) returns all files in dir in a list (reverse order)
      dir/file
  *)
  val allFiles: string -> string list

  (*  if fileName has no extension, addFileExt adds ext.
  *)
  val addFileExt : string * string -> string

  (*  fileNameKernel (dir/base.ext) = base
  *)
  val fileNameKernel : string -> string

  (*  date returns the current date and time in standard format *)
  val date : unit -> string   

  val ioErrorToString : {name : string, function : string, cause : exn} -> string

end (* signature OSLIB *)


structure OSLib :> OSLIB =
struct

  (* In SML 110.59, OS.FileSys.readDir returns an option *)
  fun readDir' d = case OS.FileSys.readDir (d) of
                     NONE => ""
                   | SOME s => s

  (*  allFiles(dir) returns all files in dir in a list (reverse order)
      dir/file
  *)
  fun allFiles(dir) =
      let val dirstream = OS.FileSys.openDir (dir)
          fun all ("", files) = files
	    | all (s,  files) = all (readDir' (dirstream), 
				     OS.Path.concat (dir, s) :: files)
          val result = all (readDir' (dirstream), [])
          val _ = OS.FileSys.closeDir (dirstream)
      in result end

  (*  if fileName has no extension, addFileExt adds ext.
      This function is OS independent. 
  *)
  fun addFileExt (fileName, ext) =
      let val {base=base, ext=extOpt} = OS.Path.splitBaseExt (fileName)
      in case extOpt of
	  NONE => OS.Path.joinBaseExt {base = base, ext = SOME ext}
	| SOME _ => fileName
      end 

  (*  fileNameKernel (dir/base.ext) = base
      This function is OS independent. 
  *)
  fun fileNameKernel (fileName) =
      let val {dir = dir, file = file} = OS.Path.splitDirFile (fileName)
	  val {base = base, ext = ext} = OS.Path.splitBaseExt (file)
      in base end
 
  fun date() = Date.toString (Date.fromTimeLocal (Time.now()))

  fun ioErrorToString {name=name, function=function, cause=cause} =
      case function of
          "openIn" => "File not found: "^name
	| function => "I/O error: file: "^name^" function: "^function

end (* structure OSLib *)
