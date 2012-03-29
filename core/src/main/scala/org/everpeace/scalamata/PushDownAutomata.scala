package org.everpeace.scalamata

import collection.immutable.{Stack => ScalaStack}

// special alphabet for checking stack is empty.
sealed abstract class $
case object $ extends $

/**
 * PushDown Automata (Non-Deterministic Automata with Pushdown Store(Stack)
 * σ: transition function: (state, input alphabet, value to pop) =>(next state, value to push)
 * q0: initial state
 * f: member ship function of accepted states
 */
case class PushdownAutomata[Q, Σ, Γ](σ : (Q, Either[Σ, ε], Either[Either[Γ,$],ε]) => Set[(Q, Either[Either[Γ,$], ε])],
                                     q0: Q,
                                     f: Q => Boolean)
  extends Automata[Set[Q], Σ] {

  // type aliases for readability.
  type Γ$ = Either[Γ, $]
  type Γ$_ε = Either[Γ$,ε]
  type Σ_ε = Either[Σ, ε]

  // state of automata execution.
  case class PDState(state:Q, stack:PDStack)

  def process(input: Seq[Σ]) = {
    val initialStack = PDStack(ScalaStack(Right(Right($))))
    val finalStates = _process(input)(Set(PDState(q0,initialStack))).map(_.state)
    (finalStates.exists(f),finalStates)
  }

  private def _process(input: Seq[Σ])(ss: Set[PDState]):Set[PDState]
  = input match{
    case Seq(x,xs@_*) => ss.flatMap(s => process_input(x)(s)
                                            ++ εClosure(s.state)(s.stack).flatMap(process_input(x)(_)))
    case _ => ss.flatMap(s => εClosure(s))
  }

  // εClosure: transitions without consuming input.
  private def εClosure(s:PDState): Set[PDStack] ={
    var closure = Set.empty[PDState]
    val queue = new scala.collection.mutable.Queue[PDState]()
    queue.enqueue(s)
    while(!queue.isEmpty){
      val _s = queue.dequeue()
      val new_states = process_input(Right(ε))(_s) -- closure
      queue ++= new_states
      closure ++= new_states
    }
    closure
  }

  private def process_input(x:Σ_ε)(pds:PDState):Set[PDState] = action_with_head(x)(pds) ++ action_with_ε(x)(pds)

  // transition for just pushing to stack
  private def action_with_ε(x: Σ_ε)(pds:PDState): Set[PDState] = {
    if (pds.stack.isEmpty) Set.empty
    else
      σ(pds.state, x, Right(ε))
        .map(qv => PDState(qv._1, pds.stack.push(qv._2)))
  }

  // transition for popping from stack and pushing to stack
  private def action_with_head(x: Σ_ε)(pds:PDState): Set[PDState] = {
    if (pds.stack.isEmpty) Set.empty
    else
      σ(pds.state, x, Left(pds.stack.head))
        .map(qv => PDState(qv._1, pds.stack.pop.push(qv._2)))
  }

  // ε push-able stack.
  case class PDStack(stack: ScalaStack[Γ$]) {
    def push(v: Γ$_ε): PDStack = {
      case Right(ε) => PDStack(stack) // ignore ε-push
      case Left(_) => PDStack(stack.push(v))
    }

    def pop = PDStack(stack.pop)

    def head = stack.head

    def isEmpty = stack.isEmpty
  }

}