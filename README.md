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

Get ready! 

```clojure
(require '[schema-refined.core :as r])
(require '[schema.core :as s])
```

Basic primitives, collections and composability:

```clojure
(def Coord (r/OpenClosedIntervalOf double -180.0 180.0))

(def GeoPoint {:lat Coord :lng Coord})

(def GeoPolygon (r/BoundedListOf GeoPoint 1 50))

(def input [{:lat 48.8529 :lng 2.3499}
            {:lat 51.5085 :lng -0.0762}
            {:lat 40.0086 :lng 28.9802}])

(s/check GeoPolygon input)
```

### Sum Types

Schema previously had `s/either` to deal with sum types. Which didn't work the way e.g. `one-of` doesn't work
when dealing with JSON schema: the description is fragile and error messages is not useful at all ("typing" message
that given data does not conform any of the listed options would only confuse). That's why `schema` switch to
`conditional` where you have to specify branching predicate in advance. `schema-refined` includes slightly more
readable version of conditionals `r/dispatch-on` that covers the fundamental use case of having a single predicate
to decide on the branch (option).

```clojure
(defn BoundedGeoPolygon [n]
  (r/BoundedListOf GeoPoint n))

(def Point (BoundedGeoPolygon 1))

(def Line (BoundedGeoPolygon 2))

(def Triangle (BoundedGeoPolygon 3))

(def RandomShape GeoPolygon)

(def GeoShape
  (r/dispatch-on count
    1 Point
    2 Line
    3 Triangle
    :else RandomShape))
```

### Product Types

Product types with `r/Struct`:

```clojure

```

### More?

To find more examples and use cases, please see doc strings (whenever applicable) and tests.

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
