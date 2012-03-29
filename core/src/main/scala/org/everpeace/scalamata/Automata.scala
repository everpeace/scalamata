package org.everpeace.scalamata

/**
 * Finite Automata
 */
trait Automata[Q, Σ]{
  // return (isAccepted, final state)
  def process(input: Seq[Σ]): (Boolean, Q)
  def accept(input: Seq[Σ]) = process(input)._1
}
