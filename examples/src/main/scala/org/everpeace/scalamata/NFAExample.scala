package org.everpeace.scalamata

object NFAExample extends App{

  import org.everpeace.scalamata._
  import org.everpeace.scalamata.DFAExample.Alpha
  import org.everpeace.scalamata.DFAExample.A
  import org.everpeace.scalamata.DFAExample.B

  // Sample of NFA which accepts *AB.
  //  +----> Q3 --A--> Q4 --B-->Q5
  //  +-A,B--+
  sealed class State2
  case object Q3 extends State2
  case object Q4 extends State2
  case object Q5 extends State2
  val σ2: (State2, Alpha) => Set[State2]
  = {
    case (Q3, A)=> Set(Q3,Q4)
    case (Q3, B)=> Set(Q3)
    case (Q4, B)=> Set(Q5)
    case _=> Set.empty
  }
  val _ABinNFA = NFA(σ2,Q3,Set[State2](Q5))

  //[output]:
  // (true)
  // (true)
  // (false)
  // (false)
  println("\nNFA on {A,B}* which accepts *AB")
  println("%s".format(Seq(A,B))+"=>"+_ABinNFA(Seq(A,B)))
  println("%s".format(Seq(A,A,A,B))+"=>"+_ABinNFA(Seq(A,A,A,B)))
  println("%s".format(Seq(A,B,A))+"=>"+_ABinNFA(Seq(A,B,A)))
  println("%s".format(Seq(A,B,B,B))+"=>"+_ABinNFA(Seq(A,B,B,B)))
}
