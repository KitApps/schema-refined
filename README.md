## schema-refined

Powerful "refined" steroids for [schema](https://github.com/plumatic/schema) library.

Type refinement is all about making the types (schemas) more precise to keep you away from errors and bugs. All heavy lifting of checking/validation the shape of the data is done by `schema` library. Here we introduce a few new concepts to make schemas flexible and more expressive:

* `types` are basically schemas (see `Schema` protocol). All operations with types return types, but we're trying to deal with them as data not just functions. We gain a lot from such approach (`schema` would force you treat your type checks as black boxes)

* `predicates`, like `(LessThan 10)` or `NonEmpty`, that you can combine using logical operations `and`, `or`, `not` etc (or define your own)

* product types with `Struct` that you can accrete and reduce "on fly" (think, as maps on steroids)

* sum types with explicit dispatching that you can use not loosing flexibility (think, as `schema/conditional` on steroids)

* `refined` to glue all the above together (think, as `schema/constrained` on steroids)

And more!

Add to your project with Leiningen/Boot:

```clojure
[com.attendify/schema-refined "0.3.0-alpha4"]
```

or with deps.edn

```clojure
com.attendify/schema-refined {:mvn/version "0.3.0-alpha4"}
```

## Our Goals

* **Readability** and **Soundness**

* Being **as precise as we can**

* Avoid **as many bugs** as possible

* Provide clean and **useful** error messages

## Talks and Tutorials

* [Keep Your Data Safe with Refined Types](https://speakerdeck.com/kachayev/keep-your-data-safe-with-refined-types)

## Inspired By

* [Refined in Haskell](https://github.com/nikita-volkov/refined)
* [Refined in Scala](https://github.com/fthomas/refined)

## Usage 

Get ready! 

```clojure
(require '[schema-refined.core :as r])
(require '[schema.core :as schema])
```

### Refined

`schema-refined.core/refined` is a supercharged version of `schema.core/constrained`. This function takes
two params: a **type** (which should be a valid schema) and a **predicate** (which should either satisfy
`schema-refiend.core/Predicate` protocol or be a function from value of given **type** to boolean) and
returns a schema that checks both that "basic" schema (given as a **type**) is satisfied and the predicates
returns `true` for this specific value. You can also use another schema as a predicate. There are a lot of
built-in **predicates**, please check the listing below. **Predicates** are very composable, you can create
a new one from existing using logical rules `And`, `Or`, `Not` and `On` (checks predicate after applying to
the value given function). There're also a few high-level predicaetes to deal with collections, like `Forall`,
`First`, `Last` etc.

Motivational example.

```clojure
;; "manually" with refined and predicates
(def LatCoord (r/refined double (r/OpenClosedInterval -90.0 90.0)))

;; the same using built-in types
;; (or functions to create types from other types, a.k.a. generics)
(def LngCoord (r/OpenClosedIntervalOf double -180.0 180.0))

;; Product type using a simple map
(def GeoPoint {:lat LatCoord :lng LngCoord})

;; using built-in types
(def Route (r/BoundedListOf GeoPoint 2 50))

;; or same with predicates
(def Route (r/refined [GeoPoint] (BoundedSize 2 50)))

(def input [{:lat 48.8529 :lng 2.3499}
            {:lat 51.5085 :lng -0.0762}
            {:lat 40.0086 :lng 28.9802}])

;; Route now is a valid schema, so you can use it as any other schema
(schema/check Route input)
```

Even more motivational example.

```clojure
(def InZurich {:lat (r/refined double (r/OpenInterval 47.34 47.39))
               :lng (r/refined double (r/OpenInterval 8.51 8.57))})

(def InRome {:lat (r/refined double (r/OpenInterval 41.87 41.93))
             :lng (r/refined double (r/OpenInterval 12.46 12.51))})

;; you can use schemas as predicates
(def RouteFromZurich (r/refined Route (r/First InZurich)))
(def RouteToRome (r/refined Route (r/Last InRome)))
(def RouteFromZurichToRome (r/refined Route (r/And (r/First InZurich) (r/Last InRome))))

;; or even more
;; note, that predicates are composable
(def FromZurichToRome (r/And (r/First InZurich) (r/Last InRome)))
(def RouteFromZurichToRomeWithLess3Hops
  (r/refined Route (r/And FromZurichToRome (r/BoundedSize 2 5))))
```

### Naming Convention

The library follows a few rules on how names are made, so it's easier to make sense of types and predicates:

* function that takes **type** (schema) to create refined version has `Of` suffix. E.g. `NonEmptyListOf`

* specific refined **type** has suffix of a basic **type**, predicates are suffix-free. E.g. `LowerCased` is
  a **predicate**, `LowerCasedStr` is a **type**

### Sum Types

Schema previously had `s/either` to deal with sum types. Which didn't work the way e.g. `one-of` doesn't work
when dealing with JSON schema: the description is fragile and error messages is not useful at all ("typing" message
that given data does not conform any of the listed options would only confuse). That's why `schema` switch to
`conditional` where you have to specify branching predicate in advance. `schema-refined` includes slightly more
readable version of conditionals `r/dispatch-on` that covers the fundamental use case of having a single predicate
to decide on the branch (option).

```clojure
(def EmptyScrollableList
  {:items (s/eq [])
   :totalCount (s/eq 0)
   :hasNext (s/eq false)
   :hasPrev (s/eq false)
   :nextPageCursor (s/eq nil)
   :prevPageCursor (s/eq nil)})

(defn NonEmptyScrollableListOf [dt]
  (dispatch-on (juxt :hasNext :hasPrev)
    [false false] (SinglePageOf dt)
    [true  false] (FirstPageOf dt)
    [false true]  (LastPageOf dt)
    [true  true]  (ScrollableListSliceOf dt)))

(defn ScrollableListOf [dt]
  (dispatch-on :totalCount
    0 EmptyScrollableList
    :else (NonEmptyScrollableListOf dt)))
```

### Product Types

`schema-refined.core/Struct` creates a **product type** which works like a simple map, but can be flexible
refined with `schema-refined.core/guard`. Guarded struct still can be changed "on fly" using `assoc` (think:
adding a new **field** to the **record**) and `dissoc` (think: removing specific **field** from the **record**).

```clojure
(def -FreeTicket (Struct
                   :id Id
                   :type (s/eq "free")
                   :title NonEmptyStr
                   :quantity (OpenIntervalOf 1 1e4)
                   :description (s/maybe NonEmptyStr)
                   :status (s/enum :open :closed)))

(def FreeTicket (guard -FreeTicket '(:quantity :status) enough-sits-when-open))

;; #<StructMap {:description (constrained Str should-not-be-blank)
;;              :type (eq "free")
;;              :title (constrained Str should-not-be-blank)
;;              :status (enum :open :closed)
;;              :id java.lang.String
;;              :quantity (constrained int should-be-bounded-by-range-given)}
;;   Guarded with
;;     enough-sits-when-open over '(:quantity :status)>
```

You can easily extend the **type** now:

```clojure
(def -PaidTicket (assoc FreeTicket
                        :type (s/eq "paid")
                        :priceInCents PositiveInt
                        :taxes [Tax]
                        :fees (s/enum :absorb :pass)))

(def PaidTicket
  (guard -PaidTicket '(:taxes :fees) pass-tax-included))

;; #<StructMap {...}
;;   Guarded with
;;     enough-sits-when-open over '(:quantity :status)
;;     pass-tax-included over '(:taxes :fees)>
```

and reduce:

```clojure
(dissoc PaidTicket :status) 

;; #<StructMap {...}
;;   Guarded with
;;     pass-tax-included over '(:taxes :fees)>

;; (only one guard left)
```

`schema-refined.core/StructDispatch` provides you the same functionality as `schema-refined.core/dispatch-on`,
but the resulting **type** behaves list a one created with `schema-refined.core/Struct`.

```clojure
(def Ticket (StructDispatch :type
              "free" FreeTicket
              "paid" PaidTicket))

;; #<StructDispatch on '(:type):
;;     free => {...}
;;     paid => {...}>

;; note, that when using `schema.core/conditional` the following would not
;; give you intended result! but it works as expected here
(def CreateTicketRequest (dissoc Ticket :id :status))
```

### More?

To find more examples and use cases, please see doc strings (whenever applicable) and tests.

## Future Versions (a.k.a In Progress)

* Separate "serialization" presentation (basic Scalar types) fro "business" logic and rules with
  a flexibility to send pointers to all predicates over the wire
  
* Maybe we need another way to deal with generics to provide flexibility with higher kinded types
  (using function to build a new type hides some information about the underlying representation and
  it's impossible to extend w/o reimplemention)

* Try to catch "impossible" predicates (which defines empty sets of values), like `(And (Less 10) (Greater 100))`

* Clean and concise way to represent transformation invariants (right now you can only define your output
  type as a function from input value, doing manual manipulations, which might be kinda tricky and not very
  obvious for the reader of your code)
  
* Support generative testing (probably bridge to the existing tools)

## Appendix A: Builtin Predicates & Types

### Predicate Combinators

* `Not`
* `And`
* `Or`
* `On`

### Ordering Predicates

* `Equal`
* `Less`
* `LessOrEqual`
* `Greater`
* `GreaterOrEqual`
* `Ascending`
* `AscendingBy`
* `AscendingOn`
* `Descending`
* `DescendingBy`
* `DescendingOn`
* `OpenInterval`
* `ClosedInterval`
* `OpenClosedInterval`
* `ClosedOpenInterval`
* `Epsilon`

### Ordering Types

* `OpenIntervalOf`
* `ClosedIntervalOf`
* `OpenClosedIntervalOf`
* `ClosedOpenIntervalOf`

### Numerical Predicates

* `Even`
* `Odd`
* `Modulo`
* `DivisibleBy`
* `NonDivisibleBy`

### Numerical Types

* `PositiveOf`
* `NegativeOf`
* `NonNegativeOf`
* `NonPositiveOf`
* `PositiveInt`
* `NegativeInt`
* `NonNegativeInt`
* `NonPositiveInt`
* `PositiveDouble`
* `NegativeDouble`
* `NonNegativeDouble`
* `NonPositiveDouble`
* `EpsilonOf`

### String Predicates

* `Uri`
* `Url`
* `StartsWith`
* `EndsWith`
* `Includes`
* `LowerCased`
* `UpperCased`

### String Types

* `NonEmptyStr`
* `BoundedSizeStr`
* `DigitChar`
* `ASCIILetterChar`
* `ASCIILetterOrDigitChar`
* `BitChar`
* `BitStr`
* `IntStr`
* `FloatStr`
* `UriStr`
* `UrlStr`
* `StartsWithStr`
* `EndsWithStr`
* `IncludesStr`
* `LowerCasedStr`
* `UpperCasedStr`

### Collection Predicates

* `Empty`
* `NonEmpty`
* `BoundedSize`
* `Distinct`
* `DistinctBy`
* `Forall`
* `Exists`
* `First`
* `Second`
* `Index`
* `Rest`
* `Last`
* `Butlast`
* `Pairwise`

### Collection Types

* `EmptyList`
* `EmptySet`
* `EmptyMap`
* `NonEmptyListOf`
* `NonEmptyMapOf`
* `NonEmptySetOf`
* `BoundedListOf`
* `BoundedSetOf`
* `BoundedMapOf`
* `SingleValueListOf`
* `SingleValueSetOf`
* `SingleValueMapOf`
* `UniqueItemsListOf`
* `NonEmptyUniqueItemsListOf`

## Contribute

* Check for open issues or open a fresh issue to start a discussion around a feature idea or a bug.
* Fork the repository on Github & fork master to `feature-*` (or `ft-`) branch to start making your changes.
* Write a test which shows that the bug was fixed or that the feature works as expected.

or simply...

* Use it.
* Enjoy it.
* Spread the word.

## License

`schema-refined` is licensed under the MIT license, available at [MIT](http://opensource.org/licenses/MIT) and also in the LICENSE file.
