package org.everpeace.scalamata

/**
 * Finite Automata
 */
trait Automata[Q, Σ]{
  // return (isAccepted, final state)
  def process(input: Seq[Σ]): (Boolean, Q)

  // process and return accepted or not.
  def accept(input: Seq[Σ]) = process(input)._1
}
