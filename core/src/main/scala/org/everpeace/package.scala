package org.everpeace

package object scalamata{

  // Set is equivalent to member ship function.
  implicit def set2func[E](set: Set[E]): E => Boolean = set.contains(_)

  // Automata on alphabet  is equivalent to Seq[Σ] => Boolean
  implicit def automaton2func[Q, Σ](a: Automata[Q, Σ]): Seq[Σ] => Boolean = a.accept(_)

  // εNFA and DFA can implicitly converted to NFA
  implicit def εNFA2NFA[Q,Σ](a:εNFA[Q, Σ]):NFA[Q, Σ] = a.asNFA
  implicit def DFA2NFA[Q,Σ](a:DFA[Q,Σ]):NFA[Q, Σ] = a.asNFA
}