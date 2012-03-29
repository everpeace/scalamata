package org.everpeace.scalamata

/**
 * Created by IntelliJ IDEA.
 * User: everpeace
 * Date: 12/03/28
 * Time: 8:13
 * To change this template use File | Settings | File Templates.
 */

object DFAExample extends App{

  import org.everpeace.scalamata._
  // alphabet
  sealed abstract class Alpha
  case object A extends Alpha
  case object B extends Alpha

  // DFA which process *AB
  // +-B-↓       +-A -+
  // +-> Q0 --A--+>Q1 + ---B--> Q2
  //     ↑         ↑------A-----+
  //     ↑----------------B-----+
  sealed abstract class State1
  case object Q0 extends State1
  case object Q1 extends State1
  case object Q2 extends State1
  val σ1: (State1, Alpha) => State1 = {
    case (Q0, A) => Q1
    case (Q0, B) => Q0
    case (Q1, A) => Q1
    case (Q1, B) => Q2
    case (Q2, A) => Q1
    case (Q2, B) => Q0
  }
  val _ABinDFA = DFA(σ1, Q0, Set[State1](Q2))

  // [output]
  // (true,Q2)
  // (false,Q0)
  println("DFA on {A,B}* which accepts *AB")
  println("input:"+Seq(A,B)+"=>"+_ABinDFA.process(Seq(A,B)))
  println("input:"+Seq(A,A,A,B)+"=>"+_ABinDFA.process(Seq(A,A,A,B)))
  // automata is equivalent to functions from Seq to (Boolean,State)
  println("input:"+Seq(A,B,A)+"=>"+_ABinDFA(Seq(A,B,A)))
  println("input:"+Seq(A,B,B,B)+"=>"+_ABinDFA(Seq(A,B,B,B)))

  // cartesian product of automata
  // [output]
  // (true,(Q2,Q2))
  println("\nDFA on ({A,B},{A,B})* which accepts *(A,A)(B,B)")
  println("input:"+Seq(A, A, B).zip(Seq(A, A, B))+"=>"+(_ABinDFA × _ABinDFA)(Seq(A, A, B).zip(Seq(A, A, B))))

}
