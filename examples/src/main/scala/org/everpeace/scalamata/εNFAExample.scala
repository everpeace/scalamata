package org.everpeace.scalamata

object εNFAExample extends App{

  import org.everpeace.scalamata._
  import org.everpeace.scalamata.DFAExample.Alpha
  import org.everpeace.scalamata.DFAExample.A
  import org.everpeace.scalamata.DFAExample.B

  // ε-NFA sample which only accepts A+
  // Q6 --A--> Q7
  //  ↑----ε---+
  sealed abstract class State3
  case object Q6 extends State3
  case object Q7 extends State3

  val σ3:(State3, Either[Alpha, ε])=>Set[State3] ={
    case (Q6, Left(A)) => Set(Q7)
    case (Q7,Right(ε)) => Set(Q6)
    case _ => Set.empty
  }
  val Aplus = εNFA(σ3,Q6,Set[State3](Q7))

  //[output]
  // (false,Set(Q6))
  // (true,Set(Q7))
  // (false,Set())
  println("\nε-NFA on {A,B}* which only accepts A+:")
  println("input:"+Seq()+"=>"+Aplus(Seq()))
  println("input:"+Seq(A,A)+"=>"+Aplus(Seq(A,A)))
  println("input:"+Seq(B)+"=>"+Aplus(Seq(B)))
}
