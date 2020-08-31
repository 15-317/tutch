signature FIXITY =
sig

  datatype associativity = Left | Right | None
  datatype precedence = Strength of int

  val maxPrec : precedence
  val minPrec : precedence

  val less : precedence * precedence -> bool
  val leq : precedence * precedence -> bool
  val compare : precedence * precedence -> order

  val inc : precedence -> precedence
  val dec : precedence -> precedence

  datatype fixity =
      Nonfix
    | Infix of precedence * associativity
    | Prefix of precedence
    | Postfix of precedence

  val prec : fixity -> precedence

end;  (* signature FIXITY *)

structure Fixity :> FIXITY =
struct
  (* Associativity ascribed to infix operators
     assoc ::= left    e.g. `<-'
	     | right   e.g. `->'
	     | none    e.g. `==' from some object language
  *)
  datatype associativity = Left | Right | None

  (* Operator Precedence *)
  datatype precedence = Strength of int

  (* Maximal and minimal precedence which can be declared explicitly *)
  val maxPrec = Strength(9999)
  val minPrec = Strength(0)

  fun less (Strength(p), Strength(q)) = (p < q)
  fun leq (Strength(p), Strength(q)) = (p <= q)
  fun compare (Strength(p), Strength(q)) = Int.compare (p, q)

  fun inc (Strength(p)) = Strength(p+1)
  fun dec (Strength(p)) = Strength(p-1)

  (* Fixities ascribed to constants *)
  datatype fixity =
      Nonfix
    | Infix of precedence * associativity
    | Prefix of precedence
    | Postfix of precedence

  (* prec (fix) = precedence of fix *)
  fun prec (Infix(p,_)) = p
    | prec (Prefix(p)) = p
    | prec (Postfix(p)) = p
    | prec (Nonfix) = inc (maxPrec)

end;  (* structure Fixity *)
