package org.everpeace.scalamata

/**
 * Non Deterministic Finite Automata without ε-transition
 * σ: non deterministic transition function
 * q0: initial state
 * f: member ship function of accepted states.
 */
case class NFA[Q, Σ](σ: (Q, Σ) => Set[Q], q0: Q, f: Q => Boolean) extends Automata[Set[Q], Σ]{
  // simulated by deterministic automata
  def accept(input: Seq[Σ]) = asDFA.accept(input)

  def asDFA = DFA[Set[Q],Σ]((qs, input) => qs.flatMap(σ(_, input)), // to state is unioned.
                            Set(q0),                                // initial state is {q0}
                            _.exists(f))

  def asNFA = this
}