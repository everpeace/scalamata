Scalamata: Automata in Scala
----------------------------
Scalamata is implementation of some automata. Supported automata is below:

* Deterministic Finite Automata(DFA)
* Non-Deterministic Finite Automata(NFA)
* epsilon-Non-Deterministic Finite Automata(epsilon-NFA) (which is epsilon-transition equipped NFA)
* Pushdown Automata: epsilon-NFA with pushdown storage(stack)

Usuage
-------
### Define Automata

    import org.everpeace.scalamata._

    // Define States
    sealed abstract class S
    case object S0 extends S
    case object S1 extends S
    case object S2 extends S
    case object Sink extends S

    // Define Transition Function
    // S0--a-->S1--a-->S2
    val sigma : (S, Char) => S ={
      case (S0, 'a') => S1
      case (S1, 'a') => S2
      case _ => Sink
    }

    // Create DFA on Char as Alphabet and S as State.
    // Creating automata needs three arguments
    //   - transition function: (State,Alphabet) => State
    //   - initial state: State
    //   - accepted states: Set[State] or State=>Boolean
    val only_aa = DFA(sigma, S0, Set[S](S2))

### Run Automata

    // Input of Automata is Seq[Alphabet].
    // (i.e. Seq[Char] in this example)
    only_aa.accept("aa".toSeq)               //true
    only_aa.accept("".toSeq)                 //false
    only_aa.accept("ab".toSeq)               //false

### Compsiting Automata
  // concatenation
    only_aa >> only_aa      //only accepts aaaa

    // parallel composition
    only_aa || only_bb      //only accept aa|bb

    // complement
    ! only_aa               //only accept string except for aa

    // repetitions
    only_aa *               // 0 or more times
    only_aa +               // 1 or more times (== only_aa >> (only_aa *) )
    only_aa ?               // 0 re 1 times

### More Examples

see [examples](https://github.com/everpeace/scalamata/tree/master/examples/src/main/scala/org/everpeace/scalamata).

License
--------

All Solutions licensed under MIT License. See LICENSE.txt for further details.


Copyright
---------
Copyright (c) 2012 [everpeace](http://twitter.com/everpeace).


