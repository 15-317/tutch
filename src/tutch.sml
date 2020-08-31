(* $Id: tutch.sml,v 1.2 2001/07/09 14:48:17 abel Exp $ *)

(* comment out first line if undefined in your version of SMLofNJ *)
(* call sml-cm with @SMLdebug=/dev/null instead *)
SMLofNJ.Internals.GC.messages false;
CM.make' ("tutch.cm");
SMLofNJ.exportFn (".heap/tutch", Top.top);

