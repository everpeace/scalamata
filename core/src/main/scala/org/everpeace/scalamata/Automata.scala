package org.everpeace.scalamata

/**
 * Finite Automata
 */
trait Automata[Q, Σ]{
  // return (isAccepted, final state)
  def accept(input: Seq[Σ]): (Boolean, Q)

  def asDFA:DFA[Q, Σ]
  def asNFA:NFA[_,Σ]
}
