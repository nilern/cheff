# Object Oriented Extensible Effects

(See [Eff](http://www.eff-lang.org/) for terminology.)

* An effect type is a class
* An effect instance is an instance of the class
    - Operations are just methods that take a delimited continuation and an
      arbitrary number of other arguments
    - Resources are just fields
    - `val` and `finally` are methods that don't take the continuation
* A computation is just a special message send that prepends a delimited
  continuation to the argument list
    - The main purpose of installing a handler is actually setting up the
      delimiter and storing the prompt tag in the effect instance
        * Calls to `val` and `finally` are also set up on top of and below the
          prompt, respectively; but this is mostly a convenience
    - Overriding methods at the handler setup point is convenient in some
      situations but complects defining the effect with installing it as a
      handler.
        * In Eff effect instances actually start out with *no methods at all*.
        * Needing to create a new class for every effect handler is *beyond
          Java-clunky* but a prototypal object model could help avoid that.

## TODO

- [ ] `finally`
- [ ] install several instances as handlers at once
- [ ] investigate prototypal object model
- [ ] use something more mainstream than Scheme
    * Multi-prompt multi-shot delimited continuation are hard to find...
