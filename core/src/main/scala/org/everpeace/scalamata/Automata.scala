package org.everpeace.scalamata

/**
 * Finite Automata
 */
trait Automata[Q, Σ]{
  // return (isAccepted, final state)
  def accept(input: Seq[Σ]): (Boolean, Q)

  def asDFA:DFA[Q, Σ]

  // parallel composition
//  def ||(another:Automata[Q, Σ]):Automata[Q, Σ]

  // concatenation
//  def >>>[_Q](another:Automata[_Q, Σ]):Automata[_, Σ]

  // def rep(min:Int, max:Int):Automata[Q, Σ]

  // replicate a^[k]
//  def rep(n:Int):Automata[Q, Σ]
  
  // replicate a^*
//  def repAstah:Automata[Q, Σ]

  // replicate: a^+
//  def repPlus:Automata[Q, Σ]

  // complement of automata
//  def neg:Automata[Q, Σ]

}