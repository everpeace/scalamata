package org.everpeace.scalamata

/**
 * Deterministic Finite Automata
 * σ: transition function
 * q0: initial state
 * f: member ship function of accepted states
 */
case class DFA[Q, Σ](σ: (Q, Σ) => Q, q0: Q, f: Q => Boolean) extends Automata[Q, Σ]{

  def accept(input: Seq[Σ]): (Boolean, Q) = _accept(input)(q0)

  private def _accept(input: Seq[Σ])(q: Q): (Boolean, Q) = input match {
    case x :: xs => _accept(xs)(σ(q, x))
    case _ => (f(q), q)
  }

  // cartesian product of automata
  def ×[_Q, _Σ](another: DFA[_Q, _Σ]): DFA[(Q, _Q), (Σ, _Σ)]
  = DFA[(Q, _Q),(Σ, _Σ)]((q, x) => (this.σ(q._1, x._1), another.σ(q._2, x._2)),
    (this.q0, another.q0),
    q => this.f(q._1) && another.f(q._2))

  def asDFA = this

//  def >>>[_Q](another: Automata[_Q, Σ]) = {
//     val (σa,qa0,fa) = this.asDFA
//     val (σa,qb0,fb) = another.asDFA
//
//
//  }
}