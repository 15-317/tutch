(* Infinite Stream Library *)
(* Author: Frank Pfenning *)
(* Modified: Andreas Abel *)

signature ISTREAM =
sig
  type 'a stream
  datatype 'a front = Cons of 'a * 'a stream

  (* Lazy stream construction and exposure *)
  val delay : (unit -> 'a front) -> 'a stream
  val expose : 'a stream -> 'a front

  (* Eager stream construction *)
  val cons : 'a * 'a stream -> 'a stream
end;

structure IStream :> ISTREAM =
struct
  datatype 'a stream = Stream of unit -> 'a front
  and 'a front = Cons of 'a * 'a stream

  fun delay (d) = Stream(d)
  fun expose (Stream(d)) = d ()

  fun cons (x, s) = Stream (fn () => Cons (x, s))
end;

