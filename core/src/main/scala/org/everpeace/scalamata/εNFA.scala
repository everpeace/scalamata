package org.everpeace.scalamata

/**
 * ε: empty alphabet
 */
sealed trait ε
object ε extends ε

/**
 * Non Deterministic Finate Automata with ε-transitions
 * σ: transition function with ε-transition ( as partial function)
 * q0: initial state
 * f: member ship function of accepted states
 */
case class εNFA[Q,Σ](σ:(Q, Either[Σ, ε])=>Set[Q], q0:Q, f:Q=>Boolean) extends Automata[Set[Q],Σ]{
  // convert to NFA without ε-transition by eliminating ε-transition
  def process(input: Seq[Σ]) = asNFA.process(input)

  def asDFA = asNFA.asDFA

  def asNFA = {
    val _σ:(Q, Σ)=>Set[Q] = (q,x) =>{
      val normal_trans = σ(q,Left(x))
      val ε_bipassed = εClosure(q).flatMap(σ(_,Left(x)))
      normal_trans ++ ε_bipassed
    }
    val _f = (q:Q) => f(q) || (εClosure(q) exists f)
    NFA(_σ,q0,_f)
  }

  private def εClosure(q:Q):Set[Q] = {
    var visited = Set.empty[Q]
    val queue = new scala.collection.mutable.Queue[Q]()
    queue.enqueue(q)
    while(!queue.isEmpty){
      val new_neighbors = σ(queue.dequeue(),Right(ε)) -- visited
      queue ++= new_neighbors
      visited ++= new_neighbors
    }
    visited
  }
}