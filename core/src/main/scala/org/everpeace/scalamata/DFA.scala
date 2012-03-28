package org.everpeace.scalamata

import scala.Either


/**
 * Deterministic Finite Automata
 * σ: transition function
 * q0: initial state
 * f: member ship function of accepted states
 */
case class DFA[Q, Σ](σ: (Q, Σ) => Q, q0: Q, f: Q => Boolean) extends Automata[Q, Σ]{

  def accept(input: Seq[Σ]): (Boolean, Q) = _accept(input)(q0)

  private def _accept(input: Seq[Σ])(q: Q): (Boolean, Q) = {
    input match {
      case x :: xs => _accept(xs)(f_σ(q, x))
      case _ => (f(q), q)
    }
  }

  // cartesian product of automata
  def ×[_Q, _Σ](another: DFA[_Q, _Σ]): DFA[(Q, _Q), (Σ, _Σ)]
  = DFA({case (q, x) => (this.σ(q._1, x._1), another.σ(q._2, x._2))},
    (this.q0, another.q0),
    q => this.f(q._1) && another.f(q._2))

  def asDFA = this
  def asNFA = NFA((q:Q, x:Σ)=>Set(this.σ(q,x)),q0,f)

  // parallel composition of DFA
  //         +--ε-->this.q0
  // dummyS -+
  //         +--ε-->another.q0
  def ||[_Q](another: DFA[_Q, Σ]) = {
    object dummyS
    val _σ:(Either[dummyS.type,Either[Q, _Q]],Either[Σ,ε])=>Set[Either[dummyS.type,Either[Q, _Q]]]
    = {
      case (Left(dummyS),Right(ε))=> Set(Right(Left(this.q0)),Right(Right(another.q0)))
      case (Right(Left(q1)),Left(_x)) => Set(Right(Left(this.σ(q1,_x))))
      case (Right(Right(q2)),Left(_x)) => Set(Right(Right(another.σ(q2,_x))))
      case _ => Set.empty
    }
    val _f = (q:Either[dummyS.type,Either[Q, _Q]]) => q match{
      case Right(Left(q1)) => this.f(q1)
      case Right(Right(q2)) => another.f(q2)
      case _ => false
    }
    εNFA(_σ,dummyS,_f)
  }

  // concatenation of DFA
  // for any accepted state a of this, adding the ε-transition.
  // a --ε--> another.q0
  def >>[_Q](another: DFA[_Q,Σ]) ={
    val _σ:(Either[Q, _Q],Either[Σ, ε])=>Set[Either[Q, _Q]]= {
      case (Left(a1),Right(ε)) if this.f(a1)
        => Set(Right(another.q0))
      case (Left(q1),Left(_x))
        =>  Set(Left(this.σ(q1,_x)))
      case (Right(q2),Left(_x))
        =>  Set(Right(another.σ(q2,_x)))
      case _
        => Set.empty
    }
    val _f: Either[Q, _Q] => Boolean = { 
      case Right(q2) => another.f(q2)
      case _ => false
    }
    εNFA(_σ,Left(this.q0),_f)
  }
}