package org.everpeace.scalamata

import org.everpeace.scalamata.DFAExample.Alpha
import org.everpeace.scalamata.DFAExample.A
import org.everpeace.scalamata.DFAExample.B

class NFAExample extends App{
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
  // (true,Set(Q3,Q5))
  // (true,Set(Q3,Q5))
  // (false,Set(Q3,Q4))
  // (false,Set(Q3))
  println("\nNFA on {A,B}* which accepts *AB")
  println("input:"+Seq(A,B)+"=>"+_ABinNFA.accept(Seq(A,B)))
  println("input:"+Seq(A,A,A,B)+"=>"+_ABinNFA.accept(Seq(A,A,A,B)))
  println("input:"+Seq(A,B,A)+"=>"+_ABinNFA.accept(Seq(A,B,A)))
  println("input:"+Seq(A,B,B,B)+"=>"+_ABinNFA.accept(Seq(A,B,B,B)))
}
