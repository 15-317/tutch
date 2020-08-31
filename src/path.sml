(* $Id: path.sml,v 1.1 2000/10/17 22:23:11 abel Exp $ *)

signature PATH =
sig

  (* Path *)
  datatype Path =
      Here                              (* #                            *)
    | Left of Path                      (* # &|=><=> A, [#; P], ~#      *) 
                                        (* (#,M), # M, (#:A),case # of..*)
    | Middle of Path                    (* case M of inl x => # | inr B *)
    | Right of Path                     (* A &|=><=> #, [A; #]          *)
                                        (* (M,#), M #, M:#, .inr x => #.*)
    | This of Path                      (* x : #, Line, Final           *)
(*    | Next of Path                      (* E ; #                        *) *)

  (* Reverse path, for accumulation in descent through a structure *)
  datatype RevPath =
      top
    | left of RevPath
    | middle of RevPath
    | right of RevPath
    | this of RevPath
(*    | next of RevPath *)
    
  val rev  : RevPath -> Path
  val rev' : RevPath * Path -> Path

  val toRegion : Path * RegTree.Exp -> Region.Region
  val toRegTree: Path * RegTree.Exp -> RegTree.Exp
end (* signature PATH *)


structure Path :> PATH =
struct

  (* Path *)
  datatype Path =
      Here                              (* #                            *)
    | Left of Path                      (* # &|=><=> A, [#; P], ~#      *) 
                                        (* (#,M), # M, (#:A),case # of..*)
    | Middle of Path                    (* case M of inl # | inr B      *)
    | Right of Path                     (* A &|=><=> #, [A; #]          *)
                                        (* (M,#), M #, M:#, ..inr # end *)
    | This of Path                      (* x : #, Line, Final, \x => #  *)

(*    | Next of Path  *)                    (* E ; #                        *)

  (* Reverse path, for accumulation in descent through a structure *)
  datatype RevPath =
      top
    | left of RevPath
    | middle of RevPath
    | right of RevPath
    | this of RevPath
(*    | next of RevPath *)
    
  fun rev' (top, p) = p
    | rev' (left (rp),  p) = rev' (rp, Left (p))
    | rev' (middle (rp),p) = rev' (rp, Middle (p))
    | rev' (right (rp), p) = rev' (rp, Right (p))
    | rev' (this (rp),  p) = rev' (rp, This (p))
(*    | rev' (next (rp),  p) = rev' (rp, Next (p)) *)

  fun rev (rp) = rev' (rp, Here) 
 
  (*  calculate region for a path *)
  fun toRegion (Here, rt) = RegTree.toRegion (rt)
    | toRegion (Left  (p), RegTree.Infix (rt1, rt2)) = toRegion (p, rt1)
    | toRegion (Right (p), RegTree.Infix (rt1, rt2)) = toRegion (p, rt2)
    | toRegion (This  (p), RegTree.PFix  (r, rt1))   = toRegion (p, rt1) 
    | toRegion (Left  (p), RegTree.Quant (rt1, r, rt2)) = toRegion (p, rt1)
    | toRegion (Right (p), RegTree.Quant (rt1, r, rt2)) = toRegion (p, rt2)
    | toRegion (Left  (p), RegTree.Tern (rt1, rt2, rt3, r)) = toRegion (p, rt1)
    | toRegion (Middle(p), RegTree.Tern (rt1, rt2, rt3, r)) = toRegion (p, rt2)
    | toRegion (Right (p), RegTree.Tern (rt1, rt2, rt3, r)) = toRegion (p, rt3)
    | toRegion (Left  (p), RegTree.Tern'(rt1, rt2, r, rt3)) = toRegion (p, rt1)
    | toRegion (Middle(p), RegTree.Tern'(rt1, rt2, r, rt3)) = toRegion (p, rt2)
    | toRegion (Right (p), RegTree.Tern'(rt1, rt2, r, rt3)) = toRegion (p, rt3)
  (* all others impossible *)

  (*  calculate remaining region tree for a path *)
  fun toRegTree (Here, rt) = rt
    | toRegTree (Left  (p), RegTree.Infix (rt1, rt2)) = toRegTree (p, rt1)
    | toRegTree (Right (p), RegTree.Infix (rt1, rt2)) = toRegTree (p, rt2)
    | toRegTree (This  (p), RegTree.PFix  (r, rt1))   = toRegTree (p, rt1) 
    | toRegTree (Left  (p), RegTree.Quant (rt1, r, rt2)) = toRegTree (p, rt1)
    | toRegTree (Right (p), RegTree.Quant (rt1, r, rt2)) = toRegTree (p, rt2)
    | toRegTree (Left  (p), RegTree.Tern (rt1, rt2, rt3, r)) = toRegTree (p, rt1)
    | toRegTree (Middle(p), RegTree.Tern (rt1, rt2, rt3, r)) = toRegTree (p, rt2)
    | toRegTree (Right (p), RegTree.Tern (rt1, rt2, rt3, r)) = toRegTree (p, rt3)
    | toRegTree (Left  (p), RegTree.Tern'(rt1, rt2, r, rt3)) = toRegTree (p, rt1)
    | toRegTree (Middle(p), RegTree.Tern'(rt1, rt2, r, rt3)) = toRegTree (p, rt2)
    | toRegTree (Right (p), RegTree.Tern'(rt1, rt2, r, rt3)) = toRegTree (p, rt3)
  (* all others impossible *)


end  (* structure Path *)
