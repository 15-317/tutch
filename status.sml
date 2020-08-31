(* $Id: status.sml,v 1.1 2000/10/17 22:23:13 abel Exp $ *)

SMLofNJ.Internals.GC.messages false;
CM.make ("src/tutch.cm");

val version = "STATUS 0.1, $Date: 2000/10/17 22:23:13 $"

fun run ([]) = OS.Process.success
  | run (sub :: subs) = 
     (print ("Getting status of submission "^sub^"...\n");
      Submit.printStatus (sub);
      run' (subs))
and run' ([]) = run ([])
  | run' (subs) = (print "\n"; run (subs))

fun help() = (print (
    "Usage: status submissions\n" ^
    "Prints out the status of each submission given in 'submissions'\n"
    );
    Global.exitCmdLine)

datatype Action = 
    Help of string option
  | Run of string list

local

    fun returnRun (files) = Run (List.rev (files))

    fun parseArgs' ([], []) = Help (SOME "No submissions given")
      | parseArgs' ([], files) = returnRun (files)
      | parseArgs' ("-h"::args, files) = Help (NONE)
      | parseArgs' ("--help"::args, files) = Help (NONE)
      | parseArgs' (name::args, files) = if String.sub (name, 0) <> #"-" 
	  then parseArgs' (args, name::files)
	  else Help (SOME ("Invalid option: "^name))

in
    fun parseArgs (args) = parseArgs' (args, [])
end

  fun abort (status) = (print "\nAborted\n"; status)

fun status (name, args) = (print (version^"\n");
      (case parseArgs (args) of
	  Help (NONE) => help()
	| Help (SOME msg)  => (print (msg^"\n"); help())
	| Run (files) => run (files))  
      handle Global.Error (msg, status) => (print ("Error: "^msg); abort (status))
	   | IO.Io (ioError) => (print (OSLib.ioErrorToString (ioError)); abort(Global.exitSysError))
	   | OS.SysErr (msg, _) => (print ("System error: "^msg); abort (Global.exitSysError)) 
	   | _ => (print ("Unrecognized exception"); abort (Global.exitInternalError))) ;


SMLofNJ.exportFn ("bin/.heap/status", status);
