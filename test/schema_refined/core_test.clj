(ns schema-refined.core-test
  (:require [schema-refined.core :as r]
            [clojure.test :as t]
            [schema.core :as s]))

(defmacro ok! [dt value]
  `(t/is (nil? (s/check ~dt ~value))))

(defmacro not-ok! [dt value]
  `(t/is (some? (s/check ~dt ~value))))

(t/deftest validate-non-empty-values
  (ok! r/NonEmptyStr "doom")
  (ok! (r/NonEmptyList s/Num) [1 2 3])
  (ok! (r/NonEmptyMap s/Keyword s/Str) {:boom "Doom"})
  (ok! (r/NonEmptyList r/NonEmptyStr) ["a" "b" "c"])
  (ok! (r/NonEmptyList (r/NonEmptyList r/NonEmptyStr)) [["a"] ["b" "c"] ["c" "d"]])
  (ok! (r/NonEmptyOrderedSet s/Num) [1 2 3])
  (not-ok! (r/NonEmptyList s/Num) [])
  (not-ok! (r/NonEmptyList s/Num) '())
  (not-ok! (r/NonEmptyOrderedSet s/Num) [])
  (not-ok! r/NonEmptyStr nil)
  (not-ok! r/NonEmptyStr '())
  (not-ok! r/NonEmptyStr ""))

(t/deftest validate-orderedset-contains-unique-elements
  (ok! (r/OrderedSet s/Num) [])
  (ok! (r/OrderedSet s/Num) [1 2 3])
  (not-ok! (r/OrderedSet s/Num) [1 1 1]))

(t/deftest validate-typed-ranges
  (ok! r/Scale 0.0)
  (ok! r/Scale 1.0)
  (ok! r/Scale 0.5)
  (not-ok! r/Scale -1.0)
  (not-ok! r/Scale 10)
  (not-ok! r/Scale 0)
  (not-ok! r/Scale 1)) 

(t/deftest validate-urls
  (ok! r/Uri "https://attendify.com")
  (ok! r/Uri "ftp://attendify.com/long-file-name.txt")
  (not-ok! r/Uri "attendify com")
  (not-ok! r/Uri "ftp://"))

(t/deftest validate-coordinates
  (ok! r/Coordinate 37.5)
  (ok! r/GeoPoint {:lat 37.5 :lng -77.0})
  (not-ok! r/Coordinate 360.0))

(t/deftest range-length-string
  (ok! (r/RangeLengthStr 1 10) "a")
  (ok! (r/RangeLengthStr 1 10) "abcdeabcde")
  (ok! (r/RangeLengthStr 1 10) "abcde     ")
  (not-ok! (r/RangeLengthStr 1 10) "")
  (not-ok! (r/RangeLengthStr 1 10) "abcdeabcdeabcde")
  (not-ok! (r/RangeLengthStr 1 10) "abcdeabcde     ")
  (ok! (r/RangeLengthStr 1 10 true) "abcdeabcde     ")
  (not-ok! (r/RangeLengthStr 1 10 true) " "))

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
                         :discountPercent (r/TypedRange int 0 100)))

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
