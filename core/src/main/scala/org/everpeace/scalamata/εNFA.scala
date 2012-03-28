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
case class εNFA[Q,Σ](σ:PartialFunction[(Q, Either[Σ, ε]),Set[Q]],q0:Q, f:Q=>Boolean) extends Automata[Set[Q],Σ]{
  // convert to NFA without ε-transition by eliminating ε-transition
  def accept(input: Seq[Σ]) = asNFA.accept(input)

  def asDFA = asNFA.asDFA

  def asNFA = {
    val _σ:(Q, Σ)=>Set[Q] = (q,x) =>{
      val σ_f = σ.orElse({ case _ => Set.empty[Q]}:PartialFunction[(Q, Either[Σ, ε]),Set[Q]])
      val normal_trans = σ_f(q,Left(x))
      val ε_bipassed = εClosure(q).flatMap(σ_f(_,Left(x)))
      normal_trans ++ ε_bipassed
    }
    val _f: Q => Boolean = q => f(q) || (εClosure(q) exists f)
    NFA(_σ,q0,_f)
  }

  private def εClosure(q:Q):Set[Q] = {
    val σ_f = σ orElse ({case _=> Set.empty[Q]}:PartialFunction[(Q, Either[Σ, ε]),Set[Q]])
    var visited = Set.empty[Q]
    val queue = new scala.collection.mutable.Queue[Q]()
    queue.enqueue(q)
    while(!queue.isEmpty){
      val new_neighbors = σ_f(queue.dequeue(),Right(ε)) -- visited
      queue ++= new_neighbors
      visited ++= new_neighbors
    }
    visited
  }
}