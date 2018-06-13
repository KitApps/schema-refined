## Refined

Set of predicates and ready-to-go schemas to make you data definitions as precise
as possible while maintaining extendiblity and readability of your code.

Sounds sophisticated? Well... The idea is to keep you away from errors & bugs,
to learn more - check out examples :) 

```clojure
[com.attendify/schema-refined "0.3.0"]
```

## Our Goals

* **Readability** and **Soundness**

* Being **as precise as we can**

* Avoid **as many bugs** as possible

* Provide clean and **useful** error messages

## Talks and Tutorials

* [Keep Your Data Safe with Refinement Types](TBD)

## Inspired By

* [Refined in Haskell](https://github.com/nikita-volkov/refined)
* [Refined in Scala](https://github.com/fthomas/refined)

## TODO

- [ ] Update implementation of `schema` protocols for `Struct` and `StructDispatch`
- [ ] Define comprehensive set of predicates for numeric types, strings and collections
- [ ] Valid JSON/XML checker with lazy deps check (not to include them upfront)
- [ ] Publish slides from the talk on refinement types and share link here
- [ ] Fill in "Usage" section in the README
- [ ] Start CHANGLELOG
- [ ] Setup CI with public results
- [ ] Render and release documentation
- [ ] Release on Clojars

## Usage 

TBD

## Future Versions (a.k.a In Progress)

* Separate "serialization" presentation (basic Scalar types) fro "business" logic and rules with
  a flexibility to send pointers to all predicates over the wire

## Contribute

* Check for open issues or open a fresh issue to start a discussion around a feature idea or a bug.
* Fork the repository on Github & fork master to `feature-*` (or `ft-`) branch to start making your changes.
* Write a test which shows that the bug was fixed or that the feature works as expected.

or simply...

* Use it.
* Enjoy it.
* Spread the word.

## License

`schema-refined` is licensed under the MIT license, available at [http://opensource.org/licenses/MIT](http://opensource.org/licenses/MIT)
and also in the LICENSE file.
