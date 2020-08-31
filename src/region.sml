(* $Id: region.sml,v 1.1 2000/10/17 22:23:12 abel Exp $ *)
(* Author: Frank Pfenning *)

signature REGION =
sig

  datatype Region = Reg of int * int	(* r ::= (i,j) is interval [i,j) *)

  (* line numbering, used when printing regions *)
  val resetLines : string -> unit	(* start new file, reset line numbering *)
  val newLine : int -> unit		(* new line starts at character i *)

  val join : Region * Region -> Region	(* join(r1,r2) = smallest region enclosing r1 and r2 *)
  val toString : Region -> string	(* line1.col1-line2.col2, parsable by Emacs *)
  val wrap : Region * string -> string  (* add region to error message, parsable by Emacs *)


end (* signature REGION *)


structure Region :> REGION =
struct

  type pos = int			(* characters, starting at 0 *)
  datatype Region = Reg of pos * pos	(* r ::= (i,j) is interval [i,j) *)

  local
    (* !linePosList is a list of starting character positions for each input line *)
    (* used to convert character positions into line.column format *)
    (* maintained with state *)
    val linePosList = ref nil : pos list ref
    val fileName = ref ""
  in
    fun resetLines (fileName') = (linePosList := nil; fileName := fileName')
    fun newLine (i) = linePosList := i::(!linePosList)
    (* posToLineCol (i) = (line,column) for character position i *)
    fun posToLineCol (i) =
        let fun ptlc (j::js) = if i >= j then (List.length js, i-j)
			       else ptlc js
	      (* first line should start at 0 *)
	      (* nil means first "line" was not terminated by <newline> *)
	      | ptlc (nil) = (0, i)
	in
	  ptlc (!linePosList)
	end
    val fileName = fn () => !fileName
  end

  fun lineColToString (line,col) =
      Int.toString (line+1) ^ "." ^ Int.toString (col+1)

  (* join (r1, r2) = r
     where r is the  smallest region containing both r1 and r2
  *)
  fun join (Reg (i1, j1), Reg (i2, j2)) = Reg (Int.min (i1, i2), Int.max (j1, j2))

  (* toString r = "line1.col1-line2.col2", a format parsable by Emacs *)
  fun toString (Reg (i,j)) =
        lineColToString (posToLineCol i) ^ "-"
	^ lineColToString (posToLineCol j)

  (* wrap (r, msg) = msg' which contains filename and region *)
  fun wrap (r, msg) = (fileName() ^ ":" ^ toString r ^ " " ^ "Error: \n" ^ msg)
 


end  (* structure Region *)
