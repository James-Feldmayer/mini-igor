# de.blubberquark.mini-igor

A Clojure library for learning function from example inputs and outputs. Unlike statistical learning approaches that tweak numerical parameters, mini-igor looks at data structures and finds common patterns. mini-igor is based on the IGOR II system.

## Usage

mini-igor takes as its parameters a variable number of input-output examples.
The first example is assumed to be the "base case". You should supply a base case and at least three more examples.

    (use 'de.blubberquark.mini-igor)

    (def length (mini-igor [[] 0] [[a] 1] [[b a] 2] [[c b a] 3]))

	(length [:one :two :three :four :five :six])
	;=> 6

If your program requires recursion, you should arrange your examples in such a way that every input-example can be constructed from the previous input-example. This restriction is likely to be lifted by the next release.

## License

Copyright © 2015 Robert Pfeiffer

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
