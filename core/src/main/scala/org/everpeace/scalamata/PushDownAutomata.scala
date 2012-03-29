package org.everpeace.scalamata

import collection.immutable.{Stack => ScalaStack}

// special alphabet for stack being empty.
sealed abstract class $

case object $ extends $

/**
 * PushDown Automata (Non-Deterministic Automata with Pushdown Store(Stack)
 */
case class PushdownAutomata[Q, Σ, Γ](σ : (Q, Either[Σ, ε], Either[Γ, Either[ε, $]]) => Set[(Q, Either[Γ, ε])],
                                     q0: Q,
                                     f: Q => Boolean)
  extends Automata[Set[Q], Σ] {

  type Γ_ε$ = Either[Γ, Either[ε, $]]
  type Γ_ε = Either[Γ, ε]
  type Σ_ε = Either[Σ, ε]

  case class PDState(state:Q, stack:PDStack)

  def process(input: Seq[Σ]) = {
    val initialStack = PDStack(ScalaStack(Right(Right($))))
    val finalStates = _process(input)(Set(PDState(q0,initialStack))).map(_.state)
    (finalStates.exists(f),finalStates)
  }

  private def _process(input: Seq[Σ])(qss: Set[PDState]):Set[PDState]
  = input match{
    case Seq(x,xs@_*) => qss.flatMap(qs => action_with_pop(qs.state)(Left(x))(qs.stack)
                                            ++ action_without_pop(qs.state)(Left(x))(qs.stack)
                                            ++ action_with_pop(qs.state)(Right(ε))(qs.stack)
                                            ++ action_without_pop(qs.state)(Right(ε))(qs.stack))
    case _ => qss
  }

  private def action_without_pop(q: Q)(x: Σ_ε)(stack: PDStack): Set[PDState] = {
    if (stack.isEmpty) Set.empty
    else
      σ(q, x, Right(Left(ε)))
        .map(qv => PDState(qv._1, stack.push(qv._2)))
  }

  private def action_with_pop(q: Q)(x: Σ_ε)(stack: PDStack): Set[PDState] = {
    if (stack.isEmpty) Set.empty
    else
      σ(q, x, stack.head)
        .map(qv => PDState(qv._1, stack.pop.push(qv._2)))
  }

  // ε push-able stack.
  case class PDStack(stack: ScalaStack[Γ_ε$]) {
    def push(v: Γ_ε): PDStack = {
      case Right(ε) => PDStack(stack) // ignore ε-push
      case Left(γ) => PDStack(stack.push(Left(γ)))
    }

    def pop = PDStack(stack.pop)

    def head = stack.head

    def isEmpty = stack.isEmpty
  }

}