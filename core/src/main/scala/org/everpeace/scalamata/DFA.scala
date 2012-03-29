package org.everpeace.scalamata

import scala.Either


/**
 * Deterministic Finite Automata
 * σ: transition function
 * q0: initial state
 * f: member ship function of accepted states
 */
case class DFA[Q, Σ](σ: (Q, Σ) => Q, q0:Q, f: Q => Boolean) extends Automata[Q, Σ]{

  def process(input: Seq[Σ]): (Boolean, Q) = _process(input)(q0)

  private def _process(input: Seq[Σ])(q: Q): (Boolean, Q) = {
    input match {
      case Seq(x,xs@_*) => _process(xs)(σ(q, x))
      case _ => (f(q), q)
    }
  }

  def asDFA = this
  def asNFA = NFA((q:Q, x:Σ)=>Set(this.σ(q,x)),q0,f)

  // cartesian product of automata
  def ×[_Q, _Σ](another: DFA[_Q, _Σ]): DFA[(Q, _Q), (Σ, _Σ)]
    = DFA({case (q, x) => (this.σ(q._1, x._1), another.σ(q._2, x._2))},
          (this.q0, another.q0),
          q => this.f(q._1) && another.f(q._2))
}