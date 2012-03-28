package org.everpeace

package object scalamata{

  // Set is equivalent to member ship function.
  implicit def set2func[E](set: Set[E]): E => Boolean = set.contains(_)

  // Automata on alphabet  is equivalent to Seq[Σ] => (Boolean,Q)
  implicit def automaton2func[Q, Σ](a: Automata[Q, Σ]): Seq[Σ] => (Boolean, Q) = a.accept(_)

}