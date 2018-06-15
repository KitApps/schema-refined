(ns schema-refined.core-test
  (:require [schema-refined.core :as r]
            [clojure.test :as t]
            [schema.core :as s]))

(defmacro ok! [dt value]
  `(t/is (nil? (s/check ~dt ~value))))

(defmacro not-ok! [dt value]
  `(t/is (some? (s/check ~dt ~value))))

(t/testing "refined"
  (let [LatCoord (r/refined double (r/OpenClosedInterval -90.0 90.0))
        LngCoord (r/OpenClosedIntervalOf double -180.0 180.0)
        GeoPoint {:lat LatCoord :lng LngCoord}
        Route (r/BoundedListOf GeoPoint 2 50)

        input [{:lat 48.8529 :lng 2.3499}
               {:lat 51.5085 :lng -0.0762}
               {:lat 40.0086 :lng 28.9802}]]

    (t/deftest refined-with-built-in-predicates
      (ok! Route input))

    (t/deftest refined-with-built-in-pred-generics
      (let [InZurich {:lat (r/refined double (r/OpenInterval 47.34 47.39))
                      :lng (r/refined double (r/OpenInterval 8.51 8.57))}

            InRome {:lat (r/refined double (r/OpenInterval 41.87 41.93))
                    :lng (r/refined double (r/OpenInterval 12.46 12.51))}

            RouteFromZurich (r/refined Route (r/First InZurich))
            RouteToRome (r/refined Route (r/Last InRome))
            RouteFromZurichToRome (r/refined Route (r/And (r/First InZurich) (r/Last InRome)))

            FromZurichToRome (r/And (r/First InZurich) (r/Last InRome))
            RouteFromZurichToRomeWithLess3Hops (r/refined Route (r/And FromZurichToRome (r/BoundedSize 2 5)))]
        (ok! RouteFromZurichToRomeWithLess3Hops input)))))

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
  (not-ok! r/NonEmptyStr "")) 

(t/deftest validate-urls
  (ok! r/Uri "https://attendify.com")
  (ok! r/Uri "ftp://attendify.com/long-file-name.txt")
  (not-ok! r/Uri "attendify com")
  (not-ok! r/Uri "ftp://"))

(t/deftest range-length-string
  (ok! (r/BoundedLengthStr 1 10) "a")
  (ok! (r/BoundedLengthStr 1 10) "abcdeabcde")
  (ok! (r/BoundedLengthStr 1 10) "abcde     ")
  (not-ok! (r/BoundedLengthStr 1 10) "")
  (not-ok! (r/BoundedLengthStr 1 10) "abcdeabcdeabcde")
  (not-ok! (r/BoundedLengthStr 1 10) "abcdeabcde     ")
  (ok! (r/BoundedLengthStr 1 10 true) "abcdeabcde     ")
  (not-ok! (r/BoundedLengthStr 1 10 true) " "))

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

(def Code (r/Dispatch
           :codeType
           "unlock" UnlockCode
           "discount" DiscountCode
           "secret" SecretCode
           "downstream" (r/Dispatch
                         :fromDownstream
                         false {:fromDownstream (s/eq false)}
                         true {:fromDownstream (s/eq true)})
           "customSlice" (assoc (r/Dispatch
                                 '(:name)
                                 (fn [{:keys [name]}] (inc (count name)))
                                 1 {:name r/NonEmptyStr}
                                 2 {:name r/NonEmptyStr})
                                :codeType
                                (s/eq "customSlice"))))

(def CounterWithElse (r/Dispatch
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
                   (r/Dispatch
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
