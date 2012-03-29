package org.everpeace.scalamata

/**
 * Non Deterministic Finite Automata without ε-transition
 * σ: non deterministic transition function
 * q0: initial state
 * f: member ship function of accepted states.
 */
case class NFA[Q, Σ](σ: (Q, Σ) => Set[Q], q0:Q, f: Q => Boolean) extends Automata[Set[Q], Σ]{
  // simulated by deterministic automata
  def process(input: Seq[Σ]) = asDFA.process(input)

  def asDFA = DFA[Set[Q],Σ]((qs, input) => qs.flatMap(σ(_, input)), // to state is unioned.
                            Set(q0),                                // initial state is {q0}
                            _.exists(f))

  def asNFA = this

  // parallel composition of NFA
  def ||[_Q](another: NFA[_Q, Σ])={
    sealed abstract trait dummyS
    object dummyS extends dummyS
    // dummyS -+--ε-->this.q0
    //         +--ε-->another.q0
    val _σ:(Either[dummyS,Either[Q, _Q]],Either[Σ,ε])=>Set[Either[dummyS,Either[Q, _Q]]]
    = {
      case (Left(dummyS),Right(ε))=> Set(Right(Left(this.q0)),Right(Right(another.q0)))
      case (Right(Left(q1)),Left(_x)) => this.σ(q1,_x) map (q => Right(Left(q)))
      case (Right(Right(q2)),Left(_x)) => another.σ(q2,_x) map (q => Right(Right(q)))
      case _ => Set.empty
    }
    // process if reached accepted state of one automata
    val _f = (q:Either[dummyS,Either[Q, _Q]]) => q match{
      case Right(Left(q1)) => this.f(q1)
      case Right(Right(q2)) => another.f(q2)
      case _ => false
    }
    εNFA(_σ,Left(dummyS).asInstanceOf[Either[dummyS,Either[Q, _Q]]],_f).asNFA
  }

  // concatenation of NFA
  def >>[_Q](another: NFA[_Q,Σ]) ={
    // for any accepted state a of this, adding the ε-transition.
    // a --ε--> another.q0
    val _σ:(Either[Q, _Q],Either[Σ, ε])=>Set[Either[Q, _Q]]= {
      case (Left(a1),Right(ε)) if this.f(a1)
        => Set(Right(another.q0))
      case (Left(q1),Left(_x))
        =>  this.σ(q1,_x) map (q => Left(q))
      case (Right(q2),Left(_x))
        =>  another.σ(q2,_x) map (q => Right(q))
      case _
        => Set.empty
    }
    // only process another automata's process states.
    val _f: Either[Q, _Q] => Boolean = {
      case Right(q2) => another.f(q2)
      case _ => false
    }
    εNFA(_σ,Left(this.q0),_f).asNFA
  }

//  def rep(n:Int):NFA[_,Σ]={
//    case _ if n == 1 => this
//    case _ if n > 1 => this >> rep(n-1)
//  }

  sealed abstract trait S
  object s0 extends S
  object sink  extends S

  def only_ε_accepted[Σ]
  = {
    val _σ:(S,Either[Σ,ε]) => Set[S] ={
      case (s0,Right(ε)) =>Set(s0)
      case (_,Left(_)) =>Set(sink)
      case _ => Set.empty
    }
    εNFA(_σ,s0,(_s:S)=>Set[S](s0).contains(_s)).asNFA
  }

  // 0 or 1
  def ? = only_ε_accepted || this

  // 0 or more
  def * = {
    val _σ: (Q,Either[Σ,ε])=>Set[Q] ={
      case (q,Right(ε)) if this.f(q) => Set(this.q0)
      case (q,Right(ε)) if !this.f(q) => Set.empty
      case (q,Left(x)) => σ(q,x)
      case _ => Set.empty
    }
    val _f = (q:Q) => q == q0 || f(q)
    εNFA(_σ,q0,_f).asNFA
  }

  // 1 or more
  def + = this >> (this *)

  // complement
  def unary_! = NFA(σ,q0, (q:Q) => !f(q))
}