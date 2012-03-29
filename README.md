Scalamata: Automata in Scala
----------------------------
Scalamata is implementation of some automata. Supported automata is below:
- Deterministic Finite Automata(DFA)
- Non-Deterministic Finite Automata(NFA)
- ƒÃ-Non-Deterministic Finite Automata(ƒÃNFA) (which is ƒÃ-transition equipped NFA)

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
    val ƒÐ : (S, Char) => S ={
      case (S0, 'a') => S1
      case (S1, 'a') => S2
      case _ => Sink
    }
    
    // Create DFA on Char as Alphabet and S as State.
    val only_a2 = DFA(ƒÐ, S0, Set[S](S2))

### run Automata

    // Input of Automata is Seq[Alphabet]. 
    // (i.e. Seq[Char] in this example)
    only_a2.accept("aa".toSeq)               //true
    only_a2.accept("".toSeq)                 //false
    only_a2.accept("ab".toSeq)               //false

### Compsiting Automata
	// concatenation
    only_a2 >> only_a2      //only accepts aaaa

    
    // parallel composition
    only_a2 >> only_b2      //only accept aa|bb
    
    // complement 
    ! only_a2               //only accept string except for aa
    
    // repetitions
    only_a2 *               // 0 or more times
    only_a2 +               // 1 or more times
    only_a2 ?               // 0 re 1 times

### More Examples

see [examples](https://github.com/everpeace/scalamata/tree/master/examples/src/main/scala/org/everpeace/scalamata).

License
--------

All Solutions licensed under MIT License. See LICENSE.txt for further details.


Copyright
---------
Copyright (c) 2011 [everpeace](http://twitter.com/everpeace).


