(* $Id: tutch.sml,v 1.1 2001/07/09 14:48:17 abel Exp $ *)

(* comment out first line if undefined in your version of SMLofNJ *)
(* call sml-cm with @SMLdebug=/dev/null instead *)
SMLofNJ.Internals.GC.messages false;
CM.make ("src/tutch.cm");
SMLofNJ.exportFn ("bin/.heap/tutch", Top.top);

