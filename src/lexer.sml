(* $Id: lexer.sml,v 1.3 2002/10/24 19:25:48 abel Exp $ *)
(* Author: Frank Pfenning *)
(* Modified: Andreas Abel *)

signature LEXER =
sig

  (* streams are not memoizing *)
  type front  = (Token.Token * Region.Region) IStream.front
  type stream = (Token.Token * Region.Region) IStream.stream

  val lexStream : TextIO.instream -> stream

end (* signature LEXER *)


structure Lexer :> LEXER =
struct

  open Token

  (* streams are not memoizing *)
  type front  = (Token.Token * Region.Region) IStream.front
  type stream = (Token.Token * Region.Region) IStream.stream

  fun isQuote      (c) = (c = #"'")
  fun isUnderscore (c) = (c = #"_")

  (* isIdChar (c) = B iff c is legal identifier constituent *)
  fun isIdChar (c) = Char.isLower(c) orelse Char.isUpper (c)
                     orelse Char.isDigit (c) orelse isUnderscore(c)
		     orelse isQuote (c)


  (* lex (inputFun) = (token, region) stream

     inputFun maintains state, reading input one line at a time and
     returning a string terminated by <newline> each time.
     The end of the stream is signalled by a string consisting only of ^D
     Argument to inputFun is the character position.
  *)
  fun lex (inputFun:int -> string) =
  let
    local (* local state maintained by the lexer *)
      val s = ref ""			(* current string (line) *)
      and left = ref 0			(* position of first character in s *)
      and right = ref 0			(* position after last character in s *)
      (* val _ = Region.resetLines ()	(* initialize line counter *)
         moved to top *)

      (* neither lexer nor parser should ever try to look beyond EOF *)
      val EOFString = String.str #"\^D"

      (* readNext () = ()
         Effect: read the next line, updating s, left, and right

         readNext relies on the invariant that identifiers are never
         spread across lines
      *)
      fun readNext () =
	  let
	    val nextLine = inputFun (!right)
	    val nextSize = String.size (nextLine)
	  in
	    if nextSize = 0		(* end of file? *)
	      then (s := EOFString;	(* fake EOF character string *)
		    left := !right;
		    right := !right + 1)
	    else (s := nextLine;
		  left := !right;
		  right := !right + nextSize;
		  Region.newLine (!left)) (* remember new line position *)
	  end
    in
      (* char (i) = character at position i
         Invariant: i >= !left
	 Effects: will read input if i >= !right
      *)
      fun char (i) =
	  if i >= !right then (readNext (); char (i))
	  else String.sub (!s, i - !left)

      (* string (i,j) = substring at region including i, excluding j
         Invariant: i >= !left and i < j and j < !right
                    Note that the relevant parts must already have been read!
	 Effects: None
      *)
      fun string (i,j) = String.substring (!s, i - !left, j-i)
    end

    (* The remaining functions do not access the state or *)
    (* stream directly, using only functions char and string *)

    (* idToToken (idCase, reg) takes the string at region reg.
       If it matches one of the keywords, the regarding token is returned.
       Otherwise an identifier token is returned.
    *)
    fun idToToken (idCase, reg as Region.Reg (i,j)) = case string (i,j) of
	"T"     => (T, reg)
      | "F"     => (F, reg)
      | "nat"   => (NAT, reg)
      | "bool"  => (BOOL, reg)
      | "list"  => (LIST, reg)  
      | "fst"   => (FST, reg)
      | "snd"   => (SND, reg)
      | "inl"   => (INL, reg)
      | "inr"   => (INR, reg)
      | "case"  => (CASE, reg)
      | "of"    => (OF, reg)
      | "end"   => (END, reg)  
      | "fn"    => (FN, reg)
      | "abort" => (ABORT, reg)
      | "s"     => (S, reg)
      | "rec"   => (REC, reg)
      | "true"  => (TRUE, reg)
      | "false" => (FALSE, reg)
      | "if"    => (IF, reg)
      | "then"  => (THEN, reg)
      | "else"  => (ELSE, reg)
      | "nil"   => (NIL, reg)     
      | "let"   => (LET, reg)
      | "in"    => (IN, reg)
      | "eq0"   => (EQ0, reg)
      | "eqS"   => (EQS, reg)
      | "eqE0S" => (EQE0S, reg)
      | "eqES0" => (EQES0, reg)
      | "eqESS" => (EQESS, reg)
      | "less0" => (LESS0, reg)
      | "lessS" => (LESSS, reg)
      | "lessE0"=> (LESSE0, reg)
      | "lessES"=> (LESSES, reg)
      | "eqN"   => (EQN, reg)
      | "eqC"   => (EQC, reg)
      | "eqENC" => (EQENC, reg)
      | "eqECN" => (EQECN, reg)
      | "eqECC" => (EQECC, reg)
      | "annotated" => (ANNOTATED, reg)   
      | "classical" => (CLASSICAL, reg)   
      | "proof" => (PROOF, reg)
      | "term"  => (TERM, reg) 
      | "val"   => (VAL, reg) 
      | "begin" => (BEGIN, reg)
      |  s      => (ID (idCase, s), reg)

    (* The main lexing functions take a character c and the next
       input position i and return a token with its region
       The name convention is lexSSS, where SSS indicates the state
       of the lexer (e.g., what has been lexed so far).

       Lexing errors are currently fatal---some error recovery code is
       indicated in comments.
    *)
    fun lexInitial (#",", i) = (COMMA, Region.Reg (i-1,i))
      | lexInitial (#":", i) = lexColon (char(i), i+1)
      | lexInitial (#";", i) = (SEMICOLON, Region.Reg (i-1,i))
      | lexInitial (#"(", i) = (LPAREN, Region.Reg (i-1,i))
      | lexInitial (#")", i) = (RPAREN, Region.Reg (i-1,i))
      | lexInitial (#"[", i) = (LBRACKET, Region.Reg (i-1,i))
      | lexInitial (#"]", i) = (RBRACKET, Region.Reg (i-1,i))
      | lexInitial (#"&", i) = (AMPERSAND, Region.Reg (i-1,i))
      | lexInitial (#"|", i) = (BAR, Region.Reg (i-1,i))
      | lexInitial (#"~", i) = (TILDE, Region.Reg (i-1,i))
      | lexInitial (#"!", i) = (EXCLAMATIONMARK, Region.Reg (i-1,i))
      | lexInitial (#"?", i) = (QUESTIONMARK, Region.Reg (i-1,i))
      | lexInitial (#".", i) = (DOT, Region.Reg (i-1,i))
      | lexInitial (#"=", i) = lexEqual (char(i), i+1)
      | lexInitial (#"<", i) = lexLess (char(i), i+1)
      | lexInitial (#"*", i) = (TIMES, Region.Reg (i-1,i))
      | lexInitial (#"+", i) = (PLUS, Region.Reg (i-1,i))
      | lexInitial (#"-", i) = lexMinus (char(i), i+1)
      | lexInitial (#"1", i) = (ONE, Region.Reg (i-1,i))
      | lexInitial (#"0", i) = (ZERO, Region.Reg (i-1,i))
      | lexInitial (#"%", i) = lexPercent (char(i), i+1)
      | lexInitial (#"\^D", i) = (EOF, Region.Reg (i-1,i-1))
      | lexInitial (c, i) =
	if Char.isSpace (c) then lexInitial (char (i),i+1)
	else if Char.isUpper(c) then lexID (Upper, Region.Reg (i-1,i))
	else if Char.isLower(c) then lexID (Lower, Region.Reg (i-1,i))
        else Parsing.error (Region.Reg (i-1,i), "Illegal character " ^ Char.toString (c))
        (* recover by ignoring: lexInitial (char(i), i+1) *)

    and lexEqual (#">", i) = (DOUBLEARROW, Region.Reg (i-2, i))
      | lexEqual (c, i)    = (EQUAL, Region.Reg (i-2, i-1))

    and lexLess (#"=", i) = lexLessEqual (char(i), i+1)
      | lexLess (c, i)    = (LESS, Region.Reg (i-2, i-1))

    and lexLessEqual (#">", i) = (BIDOUBLEARROW, Region.Reg (i-3, i))
      | lexLessEqual (c, i) =
          Parsing.error (Region.Reg (i-3, i), "Illegal character sequence `<="
			 ^ Char.toString(c) ^ "'")

    and lexMinus (#">", i) = (ARROW, Region.Reg (i-2, i))
      | lexMinus (c, i) = Parsing.error (Region.Reg (i-2, i), "Illegal character sequence `-"
					^ Char.toString(c) ^ "'")
    and lexColon (#":", i) = (DOUBLECOLON, Region.Reg (i-2, i))
      | lexColon (c, i)    = (COLON, Region.Reg (i-2, i-1))

    and lexID (idCase, Region.Reg (i,j)) =
        let fun lexID' (j) =
	        if isIdChar (char(j)) then lexID' (j+1)
		else 
		   idToToken (idCase, Region.Reg (i,j))
	in
	  lexID' (j)
	end

    and lexPercent (#"{", i) = lexDComment (char(i), 1, i+1)
      | lexPercent (c, i)    = lexComment (c, i)

    and lexComment (#"\n", i) = lexInitial (char(i), i+1)
      | lexComment (c, i)     = lexComment (char(i), i+1)

    and lexCommentPercent (#".", i) = (EOF, Region.Reg (i-2, i))
      | lexCommentPercent (c, i) = lexComment (c, i)

    (* functions lexing delimited comments below take nesting level l *)
    and lexDComment (#"}", l, i) = lexDCommentRBrace (char(i), l, i+1)
      | lexDComment (#"%", l, i) = lexDCommentPercent (char(i), l, i+1)
      | lexDComment (#"\^D", l, i) =
          (* pass comment beginning for error message? *)
          Parsing.error (Region.Reg (i-1,i-1), "Unclosed delimited comment at end of file")
	  (* recover: (EOF, (i-1,i-1)) *)
      | lexDComment (c, l, i) = lexDComment (char(i), l, i+1)

    and lexDCommentPercent (#"{", l, i) = lexDComment (char(i), l+1, i+1)
      | lexDCommentPercent (c, l, i) = lexDComment (c, l, i)

    and lexDCommentRBrace (#"%", 1, i) = lexInitial (char(i), i+1)
      | lexDCommentRBrace (#"%", l, i) = lexDComment (char(i), l-1, i+1)
      | lexDCommentRBrace (c, l, i) = lexDComment (c, l, i)

    fun lexContinue (j) = IStream.delay (fn () => lexContinue' (j))
    and lexContinue' (j) = lexContinue'' (lexInitial (char(j), j+1))

    and lexContinue'' (mt as (token, Region.Reg (i,j))) =
          IStream.Cons (mt, lexContinue (j))
  in
    lexContinue (0)
  end  (* fun lex (inputFun) = let ... in ... end *)

  fun lexStream (instream) = lex (fn i =>
         (case TextIO.inputLine (instream) of
            NONE => ""
          | SOME s => s))
 
end (* structure Lexer *)
