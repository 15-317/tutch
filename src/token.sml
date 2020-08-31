(* $Id: token.sml,v 1.4 2002/10/24 19:25:49 abel Exp $ *)

signature TOKEN =
sig

  datatype IdCase =
      Upper				(* [A-Z]id *)
    | Lower				(* [a-z]id *)

  datatype Token =
      EOF				(* end of file or stream *)
    | COMMA                             (* ',' *)
    | COLON				(* ':' *)
    | SEMICOLON				(* ';' *)
    | LPAREN | RPAREN			(* '(' ')' *)
    | LBRACKET | RBRACKET		(* '[' ']' *)
    | AMPERSAND				(* '&' *)
    | BAR				(* '|' *)
    | DOUBLEARROW			(* '=>' *)
    | BIDOUBLEARROW			(* '<=>' *)
    | T					(* 'T' *)
    | F					(* 'F' *)
    | TILDE				(* '~' *)
    | EXCLAMATIONMARK                   (* '!' *)
    | QUESTIONMARK                      (* '?' *)
    | DOT                               (* '.' *)
    | EQUAL				(* '=' *)
    | LESS                              (* '<' *)
    | TIMES                             (* '*' *)
    | PLUS                              (* '+' *)
    | ARROW                             (* '->' *)
    | ONE                               (* '1' *)
    | ZERO                              (* '0' *)
    | NAT                               (* 'nat' *)
    | BOOL                              (* 'bool' *)
    | LIST                              (* 'list' *)
    | FST                               (* 'fst' *)
    | SND                               (* 'snd' *)
    | INL                               (* 'inl' *)
    | INR                               (* 'inr' *)
    | CASE                              (* 'case' *)
    | OF                                (* 'of' *)
    | END				(* 'end' *)
    | FN                                (* 'fn' *)    
    | ABORT                             (* 'abort' *)
    | S                                 (* 's' *)
    | REC                               (* 'rec' *)
    | TRUE                              (* 'true' *)
    | FALSE                             (* 'false' *)
    | IF                                (* 'if' *)
    | THEN                              (* 'then' *)
    | ELSE                              (* 'else' *)
    | NIL                               (* 'nil' *)
    | DOUBLECOLON                       (* '::' *)    
    | LET                               (* 'let' *)
    | IN                                (* 'in' *)
    | EQ0                               (* 'eq0' *)
    | EQS                               (* 'eqS' *)
    | EQE0S                             (* 'eqES0' *)
    | EQES0                             (* 'eqE0S' *)
    | EQESS                             (* 'eqESS' *)
    | LESS0                             (* 'less0' *)
    | LESSS                             (* 'lessS' *)
    | LESSE0                            (* 'lessE0' *)
    | LESSES                            (* 'lessES' *)
    | EQN                               (* 'eqN' *)
    | EQC                               (* 'eqC' *)
    | EQENC                             (* 'eqENC ' *)
    | EQECN                             (* 'eqECN' *)
    | EQECC                             (* 'eqECC' *)
    | ANNOTATED				(* 'annotated' *)
    | CLASSICAL                         (* 'classical' *)
    | PROOF				(* 'proof' *)
    | TERM				(* 'term' *)
    | VAL                               (* 'val' *)
    | BEGIN				(* 'begin' *)
    | ID of IdCase * string		(* identifier *)

  val toString : Token -> string

end (* signature TOKEN *)


structure Token : TOKEN = 
struct

  datatype IdCase =
      Upper				(* [A-Z]id *)
    | Lower				(* [a-z]id *)

  datatype Token =
      EOF				(* end of file or stream *)
    | COMMA                             (* ',' *)
    | COLON				(* ':' *)
    | SEMICOLON				(* ';' *)
    | LPAREN | RPAREN			(* '(' ')' *)
    | LBRACKET | RBRACKET		(* '[' ']' *)
    | AMPERSAND				(* '&' *)
    | BAR				(* '|' *)
    | DOUBLEARROW			(* '=>' *)
    | BIDOUBLEARROW			(* '<=>' *)
    | T					(* 'T' *)
    | F					(* 'F' *)
    | TILDE				(* '~' *)
    | EXCLAMATIONMARK                   (* '!' *)
    | QUESTIONMARK                      (* '?' *)
    | DOT                               (* '.' *)
    | EQUAL				(* '=' *)
    | LESS                              (* '<' *)
    | TIMES                             (* '*' *)
    | PLUS                              (* '+' *)
    | ARROW                             (* '->' *)
    | ONE                               (* '1' *)
    | ZERO                              (* '0' *)
    | NAT                               (* 'nat' *)
    | BOOL                              (* 'bool' *)
    | LIST                              (* 'list' *)
    | FST                               (* 'fst' *)
    | SND                               (* 'snd' *)
    | INL                               (* 'inl' *)
    | INR                               (* 'inr' *)
    | CASE                              (* 'case' *)
    | OF                                (* 'of' *)
    | END				(* 'end' *)
    | FN                                (* 'fn' *)    
    | ABORT                             (* 'abort' *)
    | S                                 (* 's' *)
    | REC                               (* 'rec' *)
    | TRUE                              (* 'true' *)
    | FALSE                             (* 'false' *)
    | IF                                (* 'if' *)
    | THEN                              (* 'then' *)
    | ELSE                              (* 'else' *)
    | NIL                               (* 'nil' *)
    | LET                               (* 'let' *)
    | IN                                (* 'in' *)
    | EQ0                               (* 'eq0' *)
    | EQS                               (* 'eqS' *)
    | EQE0S                             (* 'eqES0' *)
    | EQES0                             (* 'eqE0S' *)
    | EQESS                             (* 'eqESS' *)
    | LESS0                             (* 'less0' *)
    | LESSS                             (* 'lessS' *)
    | LESSE0                            (* 'lessE0' *)
    | LESSES                            (* 'lessES' *)
    | EQN                               (* 'eqN' *)
    | EQC                               (* 'eqC' *)
    | EQENC                             (* 'eqENC ' *)
    | EQECN                             (* 'eqECN' *)
    | EQECC                             (* 'eqECC' *)
    | DOUBLECOLON                       (* '::' *)    
    | ANNOTATED				(* 'annotated' *)
    | CLASSICAL                         (* 'classical' *)
    | PROOF				(* 'proof' *)
    | TERM				(* 'term' *)
    | VAL                               (* 'val' *)
    | BEGIN				(* 'begin' *)
    | ID of IdCase * string		(* identifier *)

  fun toString' (NAT)  = "nat"
    | toString' (BOOL) = "bool"
    | toString' (LIST) = "list"
    | toString' (FST)  = "fst"
    | toString' (SND)  = "snd"
    | toString' (INL)  = "inl"
    | toString' (INR)  = "inr"
    | toString' (CASE) = "case"
    | toString' (OF)   = "of"
    | toString' (END)  = "end"
    | toString' (FN)   = "fn"
    | toString' (ABORT)= "abort"
    | toString' (REC)  = "rec"
    | toString' (TRUE) = "true"
    | toString' (FALSE)= "false"
    | toString' (IF)   = "if"
    | toString' (THEN) = "then"
    | toString' (ELSE) = "else"
    | toString' (NIL)  = "nil"
    | toString' (LET)  = "let"
    | toString' (IN)   = "in"
    | toString' (EQ0)  = "eq0"
    | toString' (EQS)  = "eqS"
    | toString' (EQE0S)= "eqE0S"
    | toString' (EQES0)= "eqES0"
    | toString' (EQESS)= "eqESS"
    | toString' (LESS0)= "less0"
    | toString' (LESSS)= "lessS"
    | toString' (LESSE0)="lessE0"
    | toString' (LESSES)="lessES"
    | toString' (EQN)  = "eqN"
    | toString' (EQC)  = "eqC"
    | toString' (EQENC)= "eqENC"
    | toString' (EQECN)= "eqECN"
    | toString' (EQECC)= "eqECC"
    | toString' (ANNOTATED) = "annotated"
    | toString' (CLASSICAL) = "classical"
    | toString' (PROOF)= "proof"
    | toString' (TERM) = "term"
    | toString' (VAL) = "val"
    | toString' (BEGIN)= "begin"

  fun toString (EOF)           = "end of file"
    | toString (ID(_,s))       = "identifier \"" ^ s ^ "\""
    | toString (COMMA)         = "`,'"
    | toString (COLON)         = "`:'"
    | toString (SEMICOLON)     = "`;'"
    | toString (LPAREN)        = "`('"
    | toString (RPAREN)        = "`)'"
    | toString (LBRACKET)      = "`['"
    | toString (RBRACKET)      = "`]'"
    | toString (AMPERSAND)     = "`&'"
    | toString (BAR)           = "`|'"
    | toString (DOUBLEARROW)   = "`=>'"
    | toString (BIDOUBLEARROW) = "`<=>'"
    | toString (T)             = "`T'"
    | toString (F)             = "`F'"
    | toString (TILDE)         = "`~'"
    | toString (EXCLAMATIONMARK) = "`!'"
    | toString (QUESTIONMARK)  = "`?'"
    | toString (DOT)           = "`.'"
    | toString (EQUAL)         = "`='"
    | toString (LESS)          = "<"
    | toString (TIMES)         = "`*'"
    | toString (PLUS)          = "`+'"
    | toString (ARROW)         = "`->'"
    | toString (ONE)           = "`1'"
    | toString (ZERO)          = "`0'"
    | toString (S)             = "`s'"
    | toString (DOUBLECOLON)   = "`::'"
    | toString (t)             = "keyword `" ^ toString' (t) ^ "'"

end (* structure TOKEN *)
