## schema-refined

Powerful "refined" steroids for `schema` library.

Type refinement is all about making the types (schemas) more precise to keep you away from errors and bugs.
All heavy lifting of checking/validation the shape of the data is done by [schema](https://github.com/plumatic/schema) library.
Here we introduce a few new concepts to make schemas flexible and more expressive:

* `types` are basically schemas (see `Schema` protocol). All operations with types return types, but we're trying to deal with them as data not just functions. We gain a lot from such approach (`schema` would force you treat you type checks as black boxes)

* `predicates`, like `(LessThan 10)` or `NonEmpty`, that you can combine using logical operations `and`, `or`, `not` etc (or define your own)

* product types with `Struct` that you can accrete and reduce "on fly" (think, as maps on steroids)

* sum types with explicit dispatching that you can use not loosing flexibility (think, as schema's `conditional` on steroids)

* `refined` to glue all the above together (think, as schema's `constrained` on steroids)

And more!

```clojure
[com.attendify/schema-refined "0.3.0-alpha4"]
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

## Usage 

Get ready! 

```clojure
(require '[schema-refined.core :as r])
(require '[schema.core :as s])
```

Basic primitives, collections and composability:

```clojure
;; "manually" with refined and predicates
(def Coord (r/refined double (r/OpenClosedInterval -180.0 180.0)))

;; the same using built-in types
;; (or functions to create types from other types, a.k.a. generics)
(def Coord (r/OpenClosedIntervalOf double -180.0 180.0))

;; product type using a simple map
(def GeoPoint {:lat Coord :lng Coord})

;; using built-in types
(def Route (r/BoundedListOf GeoPoint 2 50))

;; or same with predicates
(def Route (r/refined [GeoPoint] (BoundedSize 2 50)))

(def input [{:lat 48.8529 :lng 2.3499}
            {:lat 51.5085 :lng -0.0762}
            {:lat 40.0086 :lng 28.9802}])

;; Route now is a valis schema,
;; so you can use it as any other schema
(s/check Route input)
```

Even more motivational example:

```clojure
(def InZurich {:lat (r/refined double (r/Epsilon 47.37 0.03))
               :lng (r/refined dobule (r/Epsilon 8.54 0.03))})

(def InRome {:lat (r/refined double (r/Epsilon 41.90 0.03))
             :lng (r/refined double (r/Epsilon 12.49 0.03))})

;; you can use schemas as predicates
(def RouteFromZurich (r/refined Route (r/First InZurich)))
(def RouteToRome (r/refined Route (r/Last InRome)))
(def RouteFromZurichToRome (r/refined Route (r/And (r/First InZurich) (r/Last InRome))))

;; or even more
(def FromZurichToRome (r/And (r/First InZurich) (r/Last InRome)))
(def RouteFromZurichToRomeWithLess3Hopes
  (r/refined Route (r/And FromZurichToRome (r/BoundedSize 2 5))))
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

### Refined

TBD

### More?

To find more examples and use cases, please see doc strings (whenever applicable) and tests.

## Future Versions (a.k.a In Progress)

* Separate "serialization" presentation (basic Scalar types) fro "business" logic and rules with
  a flexibility to send pointers to all predicates over the wire

## TODO

- [ ] Update implementation of `schema` protocols for `Struct` and `StructDispatch`, rename Dispatch to make
      sure that's clear enoght that we do expect structs (or maps) as options
- [ ] Define comprehensive set of predicates for numeric types, strings and collections, rethinkg the basic
      idea behind `refined`, `types` and `predicates`, e.g. when dealing with `Less` or `Greater`
- [ ] Publish slides from the talk on refinement types and share link here
- [ ] Fill in "Usage" section in the README
- [ ] Start CHANGLELOG
- [ ] Setup CI with public results
- [ ] Render and release documentation
- [ ] Release on Clojars
- [ ] Reimplement all *intervals* to get better error messages

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
