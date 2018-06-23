(ns schema-refined.core-test
  (:require [schema-refined.core :as r]
            [clojure.test :as t]
            [schema.core :as s]))

(defmacro ok! [dt value]
  `(t/is (nil? (s/check ~dt ~value))))

(defmacro not-ok! [dt value]
  `(t/is (some? (s/check ~dt ~value))))

(defn numeric-map [size]
  (->> size
       range
       (map-indexed vector)
       (into {})))

(t/testing "refined"
  (let [LatCoord (r/refined double (r/OpenClosedInterval -90.0 90.0))
        LngCoord (r/OpenClosedIntervalOf double -180.0 180.0)
        GeoPoint {:lat LatCoord :lng LngCoord}
        Route    (r/BoundedListOf GeoPoint 2 50)

        input [{:lat 47.3529 :lng 8.5199}
               {:lat 51.5085 :lng -0.0762}
               {:lat 41.8705 :lng 12.4750}]]

    (t/deftest refined-with-built-in-predicates
      (ok! Route input))

    (t/deftest refined-with-built-in-pred-generics
      (let [InZurich {:lat (r/refined double (r/OpenInterval 47.34 47.39))
                      :lng (r/refined double (r/OpenInterval 8.51 8.57))}

            InRome {:lat (r/refined double (r/OpenInterval 41.87 41.93))
                    :lng (r/refined double (r/OpenInterval 12.46 12.51))}

            RouteFromZurich       (r/refined Route (r/First InZurich))
            RouteToRome           (r/refined Route (r/Last InRome))
            RouteFromZurichToRome (r/refined Route (r/And (r/First InZurich) (r/Last InRome)))

            FromZurichToRome                   (r/And (r/First InZurich) (r/Last InRome))
            RouteFromZurichToRomeWithLess3Hops (r/refined Route (r/And FromZurichToRome (r/BoundedSize 2 5)))]
        (ok! RouteFromZurichToRome input)
        (ok! RouteFromZurichToRomeWithLess3Hops input)))

    (t/deftest refined-with-on-predicate
      (ok! (r/refined GeoPoint (r/On :lng r/NegativeDouble))
           {:lat 51.5085 :lng -0.0762})

      (not-ok! (r/refined GeoPoint (r/On :lat r/NegativeDouble))
               {:lat 47.3529 :lng 8.5199})))

  (t/deftest refined-with-boolean-predicates
    (ok! (r/refined s/Int (r/Not r/NegativeInt)) 42)
    (ok! (r/refined s/Int (r/And r/PositiveInt (r/Less 108))) 42)
    (ok! (r/refined s/Int (r/Or r/PositiveInt (r/Less -7))) -42)

    (not-ok! (r/refined s/Int (r/Not r/NegativeInt)) -42)
    (not-ok! (r/refined s/Int (r/And r/PositiveInt  (r/Less 108))) 142)
    (not-ok! (r/refined s/Int (r/Or r/PositiveInt (r/Less -7))) -3))

  (t/deftest refined-with-equal-predicate
    (ok! (r/refined s/Int (r/Equal 42)) 42)
    (ok! (r/refined s/Str (r/Equal "doom")) "doom")

    (not-ok! (r/refined s/Int (r/Equal 42)) 43)
    (not-ok! (r/refined s/Str (r/Equal "doom")) "Doom"))

  (t/deftest refined-with-less-predicate
    (ok! (r/refined s/Int (r/Less 108)) 42)
    (ok! (r/refined double (r/Less 0.7)) 0.5)

    (not-ok! (r/refined s/Int (r/Less 108)) 108)
    (not-ok! (r/refined double (r/Less 0.7)) 3.14))

  (t/deftest refined-with-less-or-equal-predicate
    (ok! (r/refined s/Int (r/LessOrEqual 108)) 42)
    (ok! (r/refined s/Int (r/LessOrEqual 108)) 108)
    (ok! (r/refined double (r/LessOrEqual 0.7)) 0.7)

    (not-ok! (r/refined s/Int (r/LessOrEqual 108)) 109)
    (not-ok! (r/refined double (r/LessOrEqual 0.7)) 3.14))

  (t/deftest refined-with-greater-predicate
    (ok! (r/refined s/Int (r/Greater 42)) 108)
    (ok! (r/refined double (r/Greater 0.5)) 0.7)

    (not-ok! (r/refined s/Int (r/Greater 108)) 108)
    (not-ok! (r/refined double (r/Greater 3.14)) 0.7))

  (t/deftest refined-with-greater-or-equal-predicate
    (ok! (r/refined s/Int (r/GreaterOrEqual 42)) 108)
    (ok! (r/refined s/Int (r/GreaterOrEqual 108)) 108)
    (ok! (r/refined double (r/GreaterOrEqual 0.7)) 0.7)

    (not-ok! (r/refined s/Int (r/GreaterOrEqual 109)) 108)
    (not-ok! (r/refined double (r/GreaterOrEqual 3.14)) 0.7))

  (t/deftest refined-with-open-interval-predicate
    (ok! (r/refined s/Int (r/OpenInterval 0 43)) 42)
    (ok! (r/refined double (r/OpenInterval 0.0 1.0)) 0.7)
    (ok! (r/refined s/Int (r/Epsilon 10 5)) 10)
    (ok! (r/refined s/Int (r/Epsilon 10 5)) 13)
    (ok! (r/refined s/Int (r/Epsilon 10 5)) 7)

    (not-ok! (r/refined s/Int (r/OpenInterval 0 43)) 0)
    (not-ok! (r/refined s/Int (r/OpenInterval 0 43)) 43)
    (not-ok! (r/refined s/Int (r/OpenInterval 0 43)) -7)
    (not-ok! (r/refined s/Int (r/OpenInterval 0 43)) 108)
    (not-ok! (r/refined double (r/OpenInterval 0.0 1.0)) 0.0)
    (not-ok! (r/refined double (r/OpenInterval 0.0 1.0)) 1.0)
    (not-ok! (r/refined double (r/OpenInterval 0.0 1.0)) 3.14)
    (not-ok! (r/refined double (r/OpenInterval 0.0 1.0)) -3.14)
    (not-ok! (r/refined s/Int (r/Epsilon 10 5)) 5)
    (not-ok! (r/refined s/Int (r/Epsilon 10 5)) 15)
    (not-ok! (r/refined s/Int (r/Epsilon 10 5)) -7)
    (not-ok! (r/refined s/Int (r/Epsilon 10 5)) 108))

  (t/deftest refined-with-closed-interval-predicate
    (ok! (r/refined s/Int (r/ClosedInterval 0 43)) 42)
    (ok! (r/refined s/Int (r/ClosedInterval 0 43)) 0)
    (ok! (r/refined s/Int (r/ClosedInterval 0 43)) 43)
    (ok! (r/refined double (r/ClosedInterval 0.0 1.0)) 0.7)
    (ok! (r/refined double (r/ClosedInterval 0.0 1.0)) 0.0)
    (ok! (r/refined double (r/ClosedInterval 0.0 1.0)) 1.0)

    (not-ok! (r/refined s/Int (r/ClosedInterval 0 43)) -7)
    (not-ok! (r/refined s/Int (r/ClosedInterval 0 43)) 108)
    (not-ok! (r/refined double (r/ClosedInterval 0.0 1.0)) 3.14)
    (not-ok! (r/refined double (r/ClosedInterval 0.0 1.0)) -3.14))

  (t/deftest refined-with-open-closed-interval-predicate
    (ok! (r/refined s/Int (r/OpenClosedInterval 0 43)) 42)
    (ok! (r/refined s/Int (r/OpenClosedInterval 0 43)) 43)
    (ok! (r/refined double (r/OpenClosedInterval 0.0 1.0)) 0.7)
    (ok! (r/refined double (r/OpenClosedInterval 0.0 1.0)) 1.0)

    (not-ok! (r/refined s/Int (r/OpenClosedInterval 0 43)) -7)
    (not-ok! (r/refined s/Int (r/OpenClosedInterval 0 43)) 108)
    (not-ok! (r/refined s/Int (r/OpenClosedInterval 0 43)) 0)
    (not-ok! (r/refined double (r/OpenClosedInterval 0.0 1.0)) 3.14)
    (not-ok! (r/refined double (r/OpenClosedInterval 0.0 1.0)) -3.14)
    (not-ok! (r/refined double (r/OpenClosedInterval 0.0 1.0)) 0.0))

  (t/deftest refined-with-closed-open-interval-predicate
    (ok! (r/refined s/Int (r/ClosedOpenInterval 0 43)) 42)
    (ok! (r/refined s/Int (r/ClosedOpenInterval 0 43)) 0)
    (ok! (r/refined double (r/ClosedOpenInterval 0.0 1.0)) 0.7)
    (ok! (r/refined double (r/ClosedOpenInterval 0.0 1.0)) 0.0)

    (not-ok! (r/refined s/Int (r/ClosedOpenInterval 0 43)) -7)
    (not-ok! (r/refined s/Int (r/ClosedOpenInterval 0 43)) 108)
    (not-ok! (r/refined s/Int (r/ClosedOpenInterval 0 43)) 43)
    (not-ok! (r/refined double (r/ClosedOpenInterval 0.0 1.0)) 3.14)
    (not-ok! (r/refined double (r/ClosedOpenInterval 0.0 1.0)) -3.14)
    (not-ok! (r/refined double (r/ClosedOpenInterval 0.0 1.0)) 1.0))

  (t/deftest refined-with-even-predicate
    (ok! (r/refined s/Int r/Even) 108)

    (not-ok! (r/refined s/Int r/Even) 13))

  (t/deftest refined-with-odd-predicate
    (ok! (r/refined s/Int r/Odd) 13)

    (not-ok! (r/refined s/Int r/Odd) 108))

  (t/deftest refined-with-modulo-predicate
    (ok! (r/refined s/Int (r/Modulo 7 3)) 24)
    (ok! (r/refined s/Int (r/Modulo 7 3)) -25)

    (not-ok! (r/refined s/Int (r/Modulo 7 3)) 25)
    (not-ok! (r/refined s/Int (r/Modulo 7 3)) -24))

  (t/deftest refined-with-divisible-by-predicate
    (ok! (r/refined s/Int (r/DivisibleBy 7)) 21)
    (ok! (r/refined s/Int (r/DivisibleBy 7)) -28)
    (ok! (r/refined s/Int (r/DivisibleBy 7)) 0)
    (ok! (r/refined s/Int (r/DivisibleBy 7)) 7)

    (not-ok! (r/refined s/Int (r/DivisibleBy 7)) 25)
    (not-ok! (r/refined s/Int (r/DivisibleBy 7)) -24))

  (t/deftest refined-with-non-divisible-by-predicate
    (ok! (r/refined s/Int (r/NonDivisibleBy 7)) 25)
    (ok! (r/refined s/Int (r/NonDivisibleBy 7)) -24)

    (not-ok! (r/refined s/Int (r/NonDivisibleBy 7)) 21)
    (not-ok! (r/refined s/Int (r/NonDivisibleBy 7)) -28)
    (not-ok! (r/refined s/Int (r/NonDivisibleBy 7)) 0)
    (not-ok! (r/refined s/Int (r/NonDivisibleBy 7)) 7))

  (t/deftest refined-with-starts-with-predicate
    (ok! (r/refined s/Str (r/StartsWith "https://")) "https://attendify.com")

    (not-ok! (r/refined s/Str (r/StartsWith "https://"))
             "ftp://attendify.com/long-file-name.txt"))

  (t/deftest refined-with-ends-with-predicate
    (ok! (r/refined s/Str (r/EndsWith ".com")) "https://attendify.com")

    (not-ok! (r/refined s/Str (r/EndsWith ".com"))
             "ftp://attendify.com/long-file-name.txt"))

  (t/deftest refined-with-includes-predicate
    (ok! (r/refined s/Str (r/Includes "attendify")) "https://attendify.com")

    (not-ok! (r/refined s/Str (r/Includes "attendify"))
             "https://example.com"))

  (t/deftest refined-with-lower-cased-predicate
    (ok! (r/refined s/Str r/LowerCased) "https://attendify.com")

    (not-ok! (r/refined s/Str r/LowerCased) "Hello"))

  (t/deftest refined-with-upper-cased-predicate
    (ok! (r/refined s/Str r/UpperCased) "ACE")

    (not-ok! (r/refined s/Str r/UpperCased) "https://attendify.com"))

  (t/deftest refined-with-empty-predicate
    (ok! (r/refined [s/Num] r/Empty) [])
    (ok! (r/refined [s/Num] r/Empty) '())
    (ok! (r/refined s/Str r/Empty) "")
    (ok! (r/refined {s/Keyword s/Str} r/Empty) {})

    (not-ok! (r/refined s/Str r/Empty) "doom")
    (not-ok! (r/refined [s/Num] r/Empty) [1 2 3])
    (not-ok! (r/refined {s/Keyword s/Str} r/Empty) {:boom "Doom"})
    (not-ok! (r/refined [s/Str] r/Empty) ["a" "b" "c"])
    (not-ok! (r/refined [s/Any] r/Empty) [["a"] ["b" "c"] ["c" "d"]])
    (not-ok! (r/refined s/Str r/Empty) nil)
    (not-ok! (r/refined s/Str r/Empty) '()))

  (t/deftest refined-with-not-empty-predicate
    (ok! (r/refined s/Str r/NonEmpty) "doom")
    (ok! (r/refined [s/Num] r/NonEmpty) [1 2 3])
    (ok! (r/refined {s/Keyword s/Str} r/NonEmpty) {:boom "Doom"})
    (ok! (r/refined [(r/refined s/Str r/NonEmpty)] r/NonEmpty) ["a" "b" "c"])
    (ok! (r/refined [(r/refined [(r/refined s/Str r/NonEmpty)] r/NonEmpty)] r/NonEmpty)
         [["a"] ["b" "c"] ["c" "d"]])

    (not-ok! (r/refined [s/Num] r/NonEmpty) [])
    (not-ok! (r/refined [s/Num] r/NonEmpty) '())
    (not-ok! (r/refined s/Str r/NonEmpty) nil)
    (not-ok! (r/refined s/Str r/NonEmpty) '())
    (not-ok! (r/refined s/Str r/NonEmpty) "")
    (not-ok! (r/refined {s/Keyword s/Str} r/NonEmpty) {}))

  (t/deftest refined-with-bounded-size-predicate
    (let [min-size    1
          max-size    3
          BoundedSize (r/BoundedSize min-size max-size)]
      (doseq [size (range min-size (inc max-size))]
        (ok! (r/refined [s/Num] BoundedSize) (range size))
        (ok! (r/refined #{s/Num} BoundedSize) (set (range size)))
        (ok! (r/refined {s/Num s/Num} BoundedSize) (numeric-map size)))

      (not-ok! (r/refined [s/Num] BoundedSize) [])
      (not-ok! (r/refined #{s/Num} BoundedSize) #{})
      (not-ok! (r/refined {s/Num s/Num} BoundedSize) {})
      (not-ok! (r/refined [s/Num] BoundedSize) (range (inc max-size)))
      (not-ok! (r/refined #{s/Num} BoundedSize) (-> max-size inc range set))
      (not-ok! (r/refined {s/Num s/Num} BoundedSize) (numeric-map (inc max-size)))))

  (t/deftest refined-with-distinct-predicate
    (ok! (r/refined [s/Num] r/Distinct) (range 7))
    (ok! (r/refined [s/Num] r/Distinct) [])

    (not-ok! (r/refined [s/Num] r/Distinct) (repeat 7 1)))

  (t/deftest refined-with-distinct-by-predicate
    (ok! (r/refined [{:foo s/Num}] (r/DistinctBy :foo)) (map #(-> {:foo %}) (range 7)))
    (ok! (r/refined [{:foo s/Num}] (r/DistinctBy :foo)) [])

    (not-ok! (r/refined [{:foo s/Num}] (r/DistinctBy :foo))
             (->> 1
                  (repeat 7)
                  (map #(-> {:foo %})))))

  (t/deftest refined-with-forall-predicate
    (ok! (r/refined [s/Int] (r/Forall odd?)) (range 1 10 2))
    (ok! (r/refined [s/Int] (r/Forall r/PositiveInt)) (range 1 10))
    (ok! (r/refined [s/Str] (r/Forall r/Empty)) (repeat 10 ""))

    (not-ok! (r/refined [s/Int] (r/Forall odd?)) (range 1 10))
    (not-ok! (r/refined [s/Int] (r/Forall r/PositiveInt)) (conj (range 1 10) -1))
    (not-ok! (r/refined [s/Str] (r/Forall r/Empty)) (into (repeat 10 "") ["a" ""])))

  (t/deftest refined-with-exists-predicate
    (ok! (r/refined [s/Int] (r/Exists odd?)) (into (range 0 10 2) [2 7 5]))
    (ok! (r/refined [s/Int] (r/Exists r/PositiveInt)) (into (range -10 -5) [-4 1 0]))
    (ok! (r/refined [s/Str] (r/Exists r/Empty)) (into (repeat 10 "a") ["a" "" "a"]))

    (not-ok! (r/refined [s/Int] (r/Exists odd?)) (range 0 10 2))
    (not-ok! (r/refined [s/Int] (r/Exists r/PositiveInt)) (range -10 -5))
    (not-ok! (r/refined [s/Str] (r/Exists r/Empty)) (repeat 10 "a")))

  (t/deftest refined-with-first-predicate
    (ok! (r/refined [s/Int] (r/First odd?)) (conj (range 0 10 2) 1))
    (ok! (r/refined [s/Int] (r/First r/PositiveInt)) (conj (range -10 -5) 1))
    (ok! (r/refined [s/Str] (r/First r/Empty)) (conj (repeat 10 "a") ""))

    (not-ok! (r/refined [s/Int] (r/First odd?)) (into (range 0 10 2) [1 2]))
    (not-ok! (r/refined [s/Int] (r/First r/PositiveInt)) (into (range -10 -5) [1 -2]))
    (not-ok! (r/refined [s/Str] (r/First r/Empty)) (into (repeat 10 "a") ["" "a"])))

  (t/deftest refined-with-second-predicate
    (ok! (r/refined [s/Int] (r/Second odd?)) (into (range 0 10 2) [1 2]))
    (ok! (r/refined [s/Int] (r/Second r/PositiveInt)) (into (range -10 -5) [1 -2]))
    (ok! (r/refined [s/Str] (r/Second r/Empty)) (into (repeat 10 "a") ["" "a"]))

    (not-ok! (r/refined [s/Int] (r/Second odd?)) (conj (range 0 10 2) 1))
    (not-ok! (r/refined [s/Int] (r/Second r/PositiveInt)) (conj (range -10 -5) 1))
    (not-ok! (r/refined [s/Str] (r/Second r/Empty)) (conj (repeat 10 "a") "")))

  (t/deftest refined-with-index-predicate
    (ok! (r/refined [s/Int] (r/Index 2 odd?)) (into (range 0 10 2) [1 2 4]))
    (ok! (r/refined [s/Int] (r/Index 2 r/PositiveInt)) (into (range -10 -5) [1 -2 -3]))
    (ok! (r/refined [s/Str] (r/Index 2 r/Empty)) (into (repeat 10 "a") ["" "a" "a"]))

    (not-ok! (r/refined [s/Int] (r/Index 2 odd?)) (conj (range 0 10 2) 1))
    (not-ok! (r/refined [s/Int] (r/Index 2 r/PositiveInt)) (conj (range -10 -5) 1))
    (not-ok! (r/refined [s/Str] (r/Index 2 r/Empty)) (conj (repeat 10 "a") "")))

  (t/deftest refined-with-rest-predicate
    (ok! (r/refined [s/Int] (r/Rest even?)) (conj (range 0 10 2) 1))
    (ok! (r/refined [s/Int] (r/Rest r/NegativeInt)) (conj (range -10 -5) 1))
    (ok! (r/refined [s/Str] (r/Rest r/NonEmpty)) (conj (repeat 10 "a") ""))

    (not-ok! (r/refined [s/Int] (r/Rest even?)) (into (range 0 10 2) [1 2]))
    (not-ok! (r/refined [s/Int] (r/Rest r/NegativeInt)) (into (range -10 -5) [1 -2]))
    (not-ok! (r/refined [s/Str] (r/Rest r/NonEmpty)) (into (repeat 10 "a") ["" "a"])))

  (t/deftest refined-with-last-predicate
    (ok! (r/refined [s/Int] (r/Last odd?)) (conj (vec (range 0 10 2)) 1))
    (ok! (r/refined [s/Int] (r/Last r/PositiveInt)) (conj (vec (range -10 -5)) 1))
    (ok! (r/refined [s/Str] (r/Last r/Empty)) (conj (vec (repeat 10 "a")) ""))

    (not-ok! (r/refined [s/Int] (r/Last odd?)) (into (range 0 10 2) [1 2]))
    (not-ok! (r/refined [s/Int] (r/Last r/PositiveInt)) (into (range -10 -5) [1 -2]))
    (not-ok! (r/refined [s/Str] (r/Last r/Empty)) (into (repeat 10 "a") ["" "a"])))

  (t/deftest refined-with-butlast-predicate
    (ok! (r/refined [s/Int] (r/Butlast even?)) (conj (vec (range 0 10 2)) 1))
    (ok! (r/refined [s/Int] (r/Butlast r/NegativeInt)) (conj (vec (range -10 -5)) 1))
    (ok! (r/refined [s/Str] (r/Butlast r/NonEmpty)) (conj (vec (repeat 10 "a")) ""))

    (not-ok! (r/refined [s/Int] (r/Butlast even?)) (into (range 0 10 2) [1 2]))
    (not-ok! (r/refined [s/Int] (r/Butlast r/NegativeInt)) (into (range -10 -5) [1 -2]))
    (not-ok! (r/refined [s/Str] (r/Butlast r/NonEmpty)) (into (repeat 10 "a") ["" "a"])))

  (t/deftest refined-with-pairwise-predicate
    (let [sum-equals-3? (fn [[a b]] (= 3 (+ a b)))]
      (ok! (r/refined [s/Int] (r/Pairwise sum-equals-3?)) [1 2 1])

      (not-ok! (r/refined [s/Int] (r/Pairwise sum-equals-3?)) [1 1])))

  (t/deftest refined-with-ascending-on-predicate
    (ok! (r/refined [{:price s/Int}] (r/AscendingOn :price))
         (map #(-> {:price %}) (range 10)))

    (not-ok! (r/refined [{:price s/Int}] (r/AscendingOn :price))
             (conj (map #(-> {:price %}) (range 10)) {:price 5})))

  (t/deftest refined-with-descending-on-predicate
    (ok! (r/refined [{:price s/Int}] (r/DescendingOn :price))
         (map #(-> {:price %}) (range 10 0 -1)))

    (not-ok! (r/refined [{:price s/Int}] (r/DescendingOn :price))
             (conj (map #(-> {:price %}) (range 10 0 -1)) {:price 5})))

  (let [SponsorshipLevel (s/enum "bronze" "silver" "gold")
        better-sponsor?  (fn [{a-level :level} {b-level :level}]
                           (cond
                             (= a-level b-level)  0
                             (= a-level "bronze") -1
                             (= b-level "gold")   -1
                             :else 1))]
    (t/deftest refined-with-ascending-by-predicate
      (ok! (r/refined [{:level SponsorshipLevel}] (r/AscendingBy better-sponsor?))
           [{:level "bronze"} {:level "silver"} {:level "silver"} {:level "gold"}])

      (not-ok! (r/refined [{:level SponsorshipLevel}] (r/AscendingBy better-sponsor?))
               [{:level "bronze"} {:level "gold"} {:level "silver"}]))

    (t/deftest refined-with-descending-by-predicate
      (ok! (r/refined [{:level SponsorshipLevel}] (r/DescendingBy better-sponsor?))
           [{:level "gold"} {:level "silver"} {:level "silver"} {:level "bronze"}])

      (not-ok! (r/refined [{:level SponsorshipLevel}] (r/DescendingBy better-sponsor?))
               [{:level "gold"} {:level "bronze"} {:level "silver"}])))

  (t/deftest refined-with-ascending-predicate
    (ok! (r/refined [s/Int] r/Ascending) (range 10))

    (not-ok! (r/refined [s/Int] r/Ascending) (conj (range 10) 5)))

  (t/deftest refined-with-descending-predicate
    (ok! (r/refined [s/Int] r/Descending) (range 10 0 -1))

    (not-ok! (r/refined [{:price s/Int}] r/Descending) (conj (range 10 0 -1) 5))))

(t/deftest validate-empty-values
  (ok! r/EmptyList [])
  (ok! r/EmptyList '())
  (ok! r/EmptyMap {})

  (not-ok! r/EmptyList [1 2 3])
  (not-ok! r/EmptyMap {:boom "Doom"})
  (not-ok! r/EmptyList ["a" "b" "c"])
  (not-ok! r/EmptyList [["a"] ["b" "c"] ["c" "d"]]))

(t/deftest validate-non-empty-values
  (ok! r/NonEmptyStr "doom")
  (ok! (r/NonEmptyListOf s/Num) [1 2 3])
  (ok! (r/NonEmptyMapOf s/Keyword s/Str) {:boom "Doom"})
  (ok! (r/NonEmptyListOf r/NonEmptyStr) ["a" "b" "c"])
  (ok! (r/NonEmptyListOf (r/NonEmptyListOf r/NonEmptyStr)) [["a"] ["b" "c"] ["c" "d"]])
  (not-ok! (r/NonEmptyListOf s/Num) [])
  (not-ok! (r/NonEmptyListOf s/Num) '())
  (not-ok! r/NonEmptyStr nil)
  (not-ok! r/NonEmptyStr '())
  (not-ok! r/NonEmptyStr "")
  (not-ok! (r/NonEmptyMapOf s/Keyword s/Str) {}))

(t/deftest validate-urls
  (ok! r/UriStr "https://attendify.com")
  (ok! r/UriStr "ftp://attendify.com/long-file-name.txt")
  (not-ok! r/UriStr "attendify com")

  (ok! r/UrlStr "https://attendify.com")
  (ok! r/UrlStr "ftp://attendify.com/long-file-name.txt")
  (ok! r/UrlStr "ftp://")
  (not-ok! r/UrlStr "attendify com"))

(t/deftest range-length-string
  (ok! (r/BoundedSizeStr 1 10) "a")
  (ok! (r/BoundedSizeStr 1 10) "abcdeabcde")
  (ok! (r/BoundedSizeStr 1 10) "abcde     ")
  (not-ok! (r/BoundedSizeStr 1 10) "")
  (not-ok! (r/BoundedSizeStr 1 10) "abcdeabcdeabcde")
  (not-ok! (r/BoundedSizeStr 1 10) "abcdeabcde     ")
  (ok! (r/BoundedSizeStr 1 10 true) "abcdeabcde     ")
  (not-ok! (r/BoundedSizeStr 1 10 true) " "))

(t/deftest validate-bounded-collections
  (let [min-size    1
        max-size    3]
    (doseq [size (range min-size (inc max-size))]
      (ok! (r/BoundedListOf s/Num min-size max-size) (range size))
      (ok! (r/BoundedSetOf s/Num min-size max-size) (set (range size)))
      (ok! (r/BoundedMapOf s/Num s/Num min-size max-size) (numeric-map size)))

    (not-ok! (r/BoundedListOf s/Num min-size max-size) [])
    (not-ok! (r/BoundedSetOf s/Num min-size max-size) #{})
    (not-ok! (r/BoundedMapOf s/Num s/Num min-size max-size) {})
    (not-ok! (r/BoundedListOf s/Num min-size max-size) (range (inc max-size)))
    (not-ok! (r/BoundedSetOf s/Num min-size max-size) (-> max-size inc range set))
    (not-ok! (r/BoundedMapOf s/Num s/Num min-size max-size) (numeric-map (inc max-size)))

    (ok! (r/BoundedListOf s/Num max-size) (range max-size))
    (ok! (r/BoundedSetOf s/Num max-size)
         (set (range max-size)))
    (ok! (r/BoundedMapOf s/Num s/Num max-size)
         (->> max-size
              range
              (map-indexed vector)
              (into {})))

    (doseq [size (conj (range max-size) (inc max-size))]
      (not-ok! (r/BoundedListOf s/Num max-size) (range size))
      (not-ok! (r/BoundedSetOf s/Num max-size) (-> size range set))
      (not-ok! (r/BoundedMapOf s/Num s/Num max-size) (numeric-map size)))

    (ok! (r/SingleValueListOf s/Num) [1])
    (ok! (r/SingleValueSetOf s/Num) #{1})
    (ok! (r/SingleValueMapOf s/Num s/Num) {1 1})

    (doseq [size [0 2]]
      (not-ok! (r/BoundedListOf s/Num max-size) (range size))
      (not-ok! (r/BoundedSetOf s/Num max-size) (-> size range set))
      (not-ok! (r/BoundedMapOf s/Num s/Num max-size) (numeric-map size)))))

(t/deftest validate-digit-char
  (doseq [i (range 10)]
    (ok! r/DigitChar (str i)))

  (not-ok! r/DigitChar "attendify.com")
  (not-ok! r/DigitChar "")
  (not-ok! r/DigitChar ".")
  (not-ok! r/DigitChar "j"))

(t/deftest validate-ascii-letter-char
  (doseq [i (map char (range (int \a) (inc (int \z))))]
    (ok! r/ASCIILetterChar (str i)))
  (doseq [i (map char (range (int \A) (inc (int \Z))))]
    (ok! r/ASCIILetterChar (str i)))

  (not-ok! r/ASCIILetterChar "attendify.com")
  (not-ok! r/ASCIILetterChar "")
  (not-ok! r/ASCIILetterChar ".")
  (not-ok! r/ASCIILetterChar "7"))

(t/deftest validate-ascii-letter-or-digit-char
  (doseq [i (map char (range (int \a) (inc (int \z))))]
    (ok! r/ASCIILetterOrDigitChar (str i)))
  (doseq [i (map char (range (int \A) (inc (int \Z))))]
    (ok! r/ASCIILetterOrDigitChar (str i)))
  (doseq [i (range 10)]
    (ok! r/ASCIILetterOrDigitChar (str i)))

  (not-ok! r/ASCIILetterOrDigitChar "attendify.com")
  (not-ok! r/ASCIILetterOrDigitChar "")
  (not-ok! r/ASCIILetterOrDigitChar "."))

(t/deftest validate-bit-char
  (ok! r/BitChar "0")
  (ok! r/BitChar "1")

  (not-ok! r/BitChar "attendify.com")
  (not-ok! r/BitChar "")
  (not-ok! r/BitChar ".")
  (not-ok! r/BitChar "j")
  (not-ok! r/BitChar "7"))

(t/deftest validate-bit-str
  (ok! r/BitStr "0")
  (ok! r/BitStr "1")
  (ok! r/BitStr "0001")
  (ok! r/BitStr "101010")

  (not-ok! r/BitStr "attendify.com")
  (not-ok! r/BitStr "   ")
  (not-ok! r/BitStr "000000200")
  (not-ok! r/BitStr "j")
  (not-ok! r/BitStr "1111 "))

(t/deftest validate-int-str
  (ok! r/IntStr "0")
  (ok! r/IntStr "3")
  (ok! r/IntStr "-401")
  (ok! r/IntStr "101410")
  (ok! r/IntStr "000000200")

  (not-ok! r/IntStr "attendify.com")
  (not-ok! r/IntStr "   ")
  (not-ok! r/IntStr "j")
  (not-ok! r/IntStr "1111 "))

(t/deftest validate-float-str
  (ok! r/FloatStr "0")
  (ok! r/FloatStr "3.14")
  (ok! r/FloatStr "-123.203201")
  (ok! r/FloatStr "101410")
  (ok! r/FloatStr "1111 ")

  (not-ok! r/FloatStr "attendify.com")
  (not-ok! r/FloatStr "   ")
  (not-ok! r/FloatStr "j")
  (not-ok! r/FloatStr "3_14"))

(t/deftest validate-starts-with-str
  (ok! (r/StartsWithStr "https://") "https://attendify.com")

  (not-ok! (r/StartsWithStr "https://") "ftp://attendify.com/long-file-name.txt"))

(t/deftest validate-ends-with-str
  (ok! (r/EndsWithStr ".com") "https://attendify.com")

  (not-ok! (r/EndsWithStr ".com") "ftp://attendify.com/long-file-name.txt"))

(t/deftest validate-includes-str
  (ok! (r/IncludesStr "attendify") "https://attendify.com")

  (not-ok! (r/IncludesStr "attendify") "https://example.com"))

(t/deftest validate-lower-cased-str
  (ok! r/LowerCasedStr "https://attendify.com")

  (not-ok! r/LowerCasedStr "Hello"))

(t/deftest validate-upper-cased-str
  (ok! r/UpperCasedStr "ACE")

  (not-ok! r/UpperCasedStr "https://attendify.com"))

(t/deftest validate-positive-numeric
  (ok! (r/PositiveOf s/Int) 42)
  (ok! r/PositiveInt 42)
  (ok! (r/PositiveOf double) 3.14)
  (ok! r/PositiveDouble 3.14)

  (not-ok! (r/PositiveOf s/Int) 0)
  (not-ok! r/PositiveInt 0)
  (not-ok! (r/PositiveOf s/Int) -7)
  (not-ok! r/PositiveInt -7)
  (not-ok! (r/PositiveOf double) -3.14)
  (not-ok! r/PositiveDouble -3.14))

(t/deftest validate-negative-numeric
  (ok! (r/NegativeOf s/Int) -42)
  (ok! r/NegativeInt -42)
  (ok! (r/NegativeOf double) -3.14)
  (ok! r/NegativeDouble -3.14)

  (not-ok! (r/NegativeOf s/Int) 0)
  (not-ok! r/NegativeInt 0)
  (not-ok! (r/NegativeOf s/Int) 7)
  (not-ok! r/NegativeInt 7)
  (not-ok! (r/NegativeOf double) 3.14)
  (not-ok! r/NegativeDouble 3.14))

(t/deftest validate-non-negative-numeric
  (ok! (r/NonNegativeOf s/Int) 42)
  (ok! r/NonNegativeInt 42)
  (ok! (r/NonNegativeOf double) 3.14)
  (ok! r/NonNegativeDouble 3.14)
  (ok! (r/NonNegativeOf s/Int) 0)
  (ok! r/NonNegativeInt 0)

  (not-ok! (r/NonNegativeOf s/Int) -7)
  (not-ok! r/NonNegativeInt -7)
  (not-ok! (r/NonNegativeOf double) -3.14)
  (not-ok! r/NonNegativeDouble -3.14))

(t/deftest validate-non-positive-numeric
  (ok! (r/NonPositiveOf s/Int) -42)
  (ok! r/NonPositiveInt -42)
  (ok! (r/NonPositiveOf double) -3.14)
  (ok! r/NonPositiveDouble -3.14)
  (ok! (r/NonPositiveOf s/Int) 0)
  (ok! r/NonPositiveInt 0)

  (not-ok! (r/NonPositiveOf s/Int) 7)
  (not-ok! r/NonPositiveInt 7)
  (not-ok! (r/NonPositiveOf double) 3.14)
  (not-ok! r/NonPositiveDouble 3.14))

(t/deftest validate-numeric-open-interval
  (ok! (r/OpenIntervalOf s/Int 0 43) 42)
  (ok! (r/OpenIntervalOf double 0.0 1.0) 0.7)
  (ok! (r/EpsilonOf s/Int 10 5) 10)
  (ok! (r/EpsilonOf s/Int 10 5) 13)
  (ok! (r/EpsilonOf s/Int 10 5) 7)

  (not-ok! (r/OpenIntervalOf s/Int 0 43) 0)
  (not-ok! (r/OpenIntervalOf s/Int 0 43) 43)
  (not-ok! (r/OpenIntervalOf s/Int 0 43) -7)
  (not-ok! (r/OpenIntervalOf s/Int 0 43) 108)
  (not-ok! (r/OpenIntervalOf double 0.0 1.0) 0.0)
  (not-ok! (r/OpenIntervalOf double 0.0 1.0) 1.0)
  (not-ok! (r/OpenIntervalOf double 0.0 1.0) 3.14)
  (not-ok! (r/OpenIntervalOf double 0.0 1.0) -3.14)
  (not-ok! (r/EpsilonOf s/Int 10 5) 5)
  (not-ok! (r/EpsilonOf s/Int 10 5) 15)
  (not-ok! (r/EpsilonOf s/Int 10 5) -7)
  (not-ok! (r/EpsilonOf s/Int 10 5) 108))

(t/deftest validate-numeric-closed-interval
  (ok! (r/ClosedIntervalOf s/Int 0 43) 42)
  (ok! (r/ClosedIntervalOf s/Int 0 43) 0)
  (ok! (r/ClosedIntervalOf s/Int 0 43) 43)
  (ok! (r/ClosedIntervalOf double 0.0 1.0) 0.7)
  (ok! (r/ClosedIntervalOf double 0.0 1.0) 0.0)
  (ok! (r/ClosedIntervalOf double 0.0 1.0) 1.0)

  (not-ok! (r/ClosedIntervalOf s/Int 0 43) -7)
  (not-ok! (r/ClosedIntervalOf s/Int 0 43) 108)
  (not-ok! (r/ClosedIntervalOf double 0.0 1.0) 3.14)
  (not-ok! (r/ClosedIntervalOf double 0.0 1.0) -3.14))

(t/deftest validate-numeric-open-closed-interval
  (ok! (r/OpenClosedIntervalOf s/Int 0 43) 42)
  (ok! (r/OpenClosedIntervalOf s/Int 0 43) 43)
  (ok! (r/OpenClosedIntervalOf double 0.0 1.0) 0.7)
  (ok! (r/OpenClosedIntervalOf double 0.0 1.0) 1.0)

  (not-ok! (r/OpenClosedIntervalOf s/Int 0 43) -7)
  (not-ok! (r/OpenClosedIntervalOf s/Int 0 43) 108)
  (not-ok! (r/OpenClosedIntervalOf s/Int 0 43) 0)
  (not-ok! (r/OpenClosedIntervalOf double 0.0 1.0) 3.14)
  (not-ok! (r/OpenClosedIntervalOf double 0.0 1.0) -3.14)
  (not-ok! (r/OpenClosedIntervalOf double 0.0 1.0) 0.0))

(t/deftest validate-numeric-closed-open-interval
  (ok! (r/ClosedOpenIntervalOf s/Int 0 43) 42)
  (ok! (r/ClosedOpenIntervalOf s/Int 0 43) 0)
  (ok! (r/ClosedOpenIntervalOf double 0.0 1.0) 0.7)
  (ok! (r/ClosedOpenIntervalOf double 0.0 1.0) 0.0)

  (not-ok! (r/ClosedOpenIntervalOf s/Int 0 43) -7)
  (not-ok! (r/ClosedOpenIntervalOf s/Int 0 43) 108)
  (not-ok! (r/ClosedOpenIntervalOf s/Int 0 43) 43)
  (not-ok! (r/ClosedOpenIntervalOf double 0.0 1.0) 3.14)
  (not-ok! (r/ClosedOpenIntervalOf double 0.0 1.0) -3.14)
  (not-ok! (r/ClosedOpenIntervalOf double 0.0 1.0) 1.0))

(t/deftest validate-distinct-list
  (ok! (r/DistinctListOf s/Num) (range 7))
  (ok! (r/DistinctListOf s/Num) [])
  (ok! (r/NonEmptyDistinctListOf s/Num) (range 7))

  (not-ok! (r/DistinctListOf s/Num) (repeat 7 1))
  (not-ok! (r/NonEmptyDistinctListOf s/Num) []))

(t/deftest validate-at-least-map
  (ok! (r/AtLeastMap {:foo s/Int}) {:foo 1})
  (ok! (r/AtLeastMap {:foo s/Int}) {:foo 1 :bar 2})

  (not-ok! (r/AtLeastMap {:foo s/Int}) {})
  (not-ok! (r/AtLeastMap {:foo s/Int}) {:bar 2}))

(t/deftest validate-non-strict-map
  (ok! (r/NonStrictMap {:foo s/Int}) {:foo 1})
  (ok! (r/NonStrictMap {:foo s/Int}) {})

  (not-ok! (r/NonStrictMap {:foo s/Int}) {:foo 1 :bar 2})
  (not-ok! (r/NonStrictMap {:foo s/Int}) {:bar 2}))

(def -Ticket (r/Struct :id r/NonEmptyStr
                        :rev r/NonEmptyStr
                        :price (s/maybe s/Num)
                        :paid? s/Bool))

(def Ticket
  (r/guard
   -Ticket
   '(:price :paid?)
   (fn [{:keys [paid? price]}]
     (or (false? paid?)
         (and (some? price) (< 0 price))))
   'paid-ticket-should-have-price))

(t/deftest struct-with-guards
  (ok! Ticket {:id "1" :rev "2" :paid? true :price 10})
  (not-ok! Ticket {:id "1" :rev "2" :paid? true})
  (not-ok! Ticket {:id "1" :rev "2" :paid? true :price nil})
  (ok! (dissoc Ticket :id :rev) {:paid? true :price 10})
  (not-ok! (dissoc Ticket :id :rev) {:paid? true :price nil})
  (ok! (dissoc Ticket :price) {:id "1" :rev "2" :paid? true}))

(def -BaseCode (r/map->struct {:id r/NonEmptyStr
                                :rev r/NonEmptyStr
                                :name r/NonEmptyStr}))

;; still struct
(def UnlockCode (assoc -BaseCode
                       :codeType (s/eq "unlock")
                       :code r/NonEmptyStr))

;; still struct
(def DiscountCode (assoc -BaseCode
                         :codeType (s/eq "discount")
                         :discountPercent (r/ClosedIntervalOf int 0 100)))

;; should be converted to strct inside Dispatch
(def SecretCode {:codeType (s/eq "secret")
                 :noValues r/NonEmptyStr})

(def Code (r/StructDispatch
           :codeType
           "unlock" UnlockCode
           "discount" DiscountCode
           "secret" SecretCode
           "downstream" (r/StructDispatch
                         :fromDownstream
                         false {:fromDownstream (s/eq false)}
                         true {:fromDownstream (s/eq true)})
           "customSlice" (assoc (r/StructDispatch
                                 '(:name)
                                 (fn [{:keys [name]}] (inc (count name)))
                                 1 {:name r/NonEmptyStr}
                                 2 {:name r/NonEmptyStr})
                                :codeType
                                (s/eq "customSlice"))))

(def CounterWithElse (r/StructDispatch
                      :num
                      1 {:num (s/eq 1)}
                      2 {:num (s/eq 2)}
                      :else {:num s/Any}))

(def CreateCodeRequest (dissoc Code :id :rev))

(t/deftest dispatch-struct
  (ok! CreateCodeRequest {:codeType "unlock"
                          :name "First"
                          :code "Boom!"})
  (ok! CreateCodeRequest {:codeType "discount"
                          :name "Second"
                          :discountPercent (int 50)})
  (ok! CreateCodeRequest {:codeType "secret"
                          :noValues "It's a secret!"})
  (not-ok! CreateCodeRequest {:id "1"
                              :codeType "unlock"
                              :name "Third"
                              :code "Fail :("})
  (not-ok! CreateCodeRequest {:codeType "unknown"
                              :name "Would not work"})

  (t/testing "dissoc from keys slice for top-level dispatch"
    (t/is (thrown? IllegalArgumentException (dissoc Code :codeType))))

  (t/testing "dissoc from downstream slices"
    (t/is (thrown? IllegalArgumentException (dissoc Code :fromDownstream))))

  (t/testing "dispatch with duplicated options"
    (t/is (thrown? IllegalArgumentException
                   (r/StructDispatch
                    :fromDownstream
                    true {:fromDownstream (s/eq false)}
                    true {:fromDownstream (s/eq true)}))))

  (t/testing "custom keys slice"
    (ok! CreateCodeRequest {:codeType "customSlice"
                            :name "z"})
    (not-ok! CreateCodeRequest {:codeType "customSlice"
                                :name "zzzz"}))

  (t/testing "else branch"
    (ok! CounterWithElse {:num 1})
    (ok! CounterWithElse {:num 2})
    (ok! CounterWithElse {:num 100})))

;;
;; check tough printing cases
;;

(defn test-print! [schema pattern]
  (let [dv (with-out-str (pr schema))]
    (t/is (.contains dv pattern) pattern)))

(t/deftest print-vector-values
  (test-print! (r/refined [double] (r/Greater 10)) "[double]")
  (test-print! (r/refined [s/Str] r/NonEmpty) "[string]")
  (test-print! (r/refined #{int} (r/On count (r/Less 10))) "#{int}")

  (test-print! (r/refined' double (r/Less 0)) "double")
  (test-print! (r/refined' [float] (r/On count (r/Greater 0))) "[float]")

  (let [Coord {:lat float :lng float}
        Route (r/refined' [Coord] (r/On count (r/GreaterOrEqual 2)))]
    (test-print! Route "[Coord]")))
