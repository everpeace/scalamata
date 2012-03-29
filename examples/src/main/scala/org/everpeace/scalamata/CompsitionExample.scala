package org.everpeace.scalamata

object CompsitionExample extends App {

  import org.everpeace.scalamata._

  sealed abstract class S

  case object S0 extends S

  case object S1 extends S

  case object S2 extends S

  case object Sink extends S

  def σ(c: Char)
  = (s: S, x: Char) => (s, x) match {
    case (S0, _c) if c == _c => S1
    case (S0, _) => Sink
    case (S1, _c) if c == _c => S2
    case (S1, _) => Sink
    case (S2, _) => Sink
    case (Sink, _) => Sink
  }

  val only_a2 = DFA(σ('a'), S0, Set[S](S2))
  val only_b2 = DFA(σ('b'), S0, Set[S](S2))

  println("check Automata for aa")
  check("", only_a2)
  check("a", only_a2)
  check("a" * 2, only_a2)
  check("a" * 3, only_a2)

  println("\ncheck Automata for aabb by combyning two automata for aa and for bb")
  check("a" * 1 + "b" * 1, only_a2 >> only_b2)
  check("a" * 2 + "b" * 2, only_a2 >> only_b2)
  check("a" * 3 + "b" * 3, only_a2 >> only_b2)

  println("\ncheck Automata for aa|bb by combyning two automata for aa and for bb")
  check("a" * 1, only_a2 || only_b2)
  check("a" * 2, only_a2 || only_b2)
  check("b" * 1, only_a2 || only_b2)
  check("b" * 2, only_a2 || only_b2)

  println("\ncheck Automata for complement of aa")
  check("", !only_a2)
  check("a", !only_a2)
  check("a" * 2, !only_a2)
  check("ab", !only_a2)
  check("a" * 3, !only_a2)
  check("a" * 4, !only_a2)

  println("\ncheck Automata for aa?")
  check("", only_a2 ?)
  check("a", only_a2 ?)
  check("a" * 2, only_a2 ?)

  println("\ncheck Automata for (aa)*")
  check("", only_a2 *)
  check("a", only_a2 *)
  check("a" * 2, only_a2 *)
  check("a" * 3, only_a2 *)
  check("a" * 4, only_a2 *)
  check("a" * 5, only_a2 *)
  check("a" * 6, only_a2 *)

  println("\ncheck Automata for aa+ (=aa(aa)*)")
  check("", only_a2 +)
  check("a", only_a2 +)
  check("a" * 2, only_a2 +)
  check("a" * 3, only_a2 +)
  check("a" * 4, only_a2 +)
  check("a" * 5, only_a2 +)
  check("a" * 6, only_a2 +)

  def check(input: String, a: Automata[_, Char]) = println("%-6s".format(input) + "=> " + a.accept(input.toSeq))
}