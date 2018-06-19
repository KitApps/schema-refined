(ns schema-refined.core
  (:require [schema.core :as s]
            [schema.spec.core :as schema-spec]
            [schema.spec.variant :as schema-variant]
            [schema.utils :as schema-utils]
            [clojure.string :as cstr]
            [potemkin.collections :refer [def-map-type]])
  (:refer-clojure :exclude [boolean?])
  (:import (java.net URI URISyntaxException URL MalformedURLException)))

;;
;; helpers & basic definitions
;;

(defn boolean?
  "Backported boolean? from Clojure 1.9"
  [x]
  (instance? Boolean x))

(defn starts-with?
  "True if s starts with substr. Backported from Clojure 1.8"
  [^CharSequence s ^String substr]
  (.startsWith (.toString s) substr))

(defn ends-with?
  "True if s ends with substr. Backported from Clojure 1.8"
  [^CharSequence s ^String substr]
  (.endsWith (.toString s) substr))

(defn includes?
  "True if s includes substr. Backported from Clojure 1.8"
  [^CharSequence s ^CharSequence substr]
  (.contains (.toString s) substr))

(defn schema? [dt]
  (satisfies? s/Schema dt))

(defprotocol Predicate
  (predicate-apply [this value]))

(defprotocol PredicateShow
  (predicate-show [this sym]))

(defn predicate? [p]
  (satisfies? Predicate p))

(defn predicate->str
  ([pred] (predicate->str pred "v" false))
  ([pred sym bounded?]
   {:pre [(predicate? pred)]}
   (let [pred-str (if (satisfies? PredicateShow pred)
                    (predicate-show pred sym)
                    (str pred))]
     (cond->> pred-str
       (and bounded? (not (starts-with? pred-str "(")))
       (format "(%s)")))))

(defn predicate-print-method [pred ^java.io.Writer writer]
  (.write writer (format "#Predicate{%s}" (predicate->str pred))))

(defrecord FunctionPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (pred value))
  PredicateShow
  (predicate-show [_ sym]
    (format "(%s %s)" (schema-utils/fn-name pred) sym)))

(defmethod print-method FunctionPredicate
  [rs ^java.io.Writer writer]
  (predicate-print-method rs writer))

(defrecord SchemaPredicate [schema]
  Predicate
  (predicate-apply [_ value]
    (nil? (s/check schema value)))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s: %s" sym schema)))

(defmethod print-method SchemaPredicate
  [rs ^java.io.Writer writer]
  (predicate-print-method rs writer))

(defrecord RefinedSchema [schema pred]
  s/Schema
  (spec [this]
    (schema-variant/variant-spec
     schema-spec/+no-precondition+
     [{:schema schema}]
     nil
     (schema-spec/precondition
      this
      (partial predicate-apply pred)
      #(list (symbol (schema-utils/fn-name pred)) %))))
  (explain [_] (list 'refined (s/explain schema) (symbol (schema-utils/fn-name pred)))))

;; Use common representation in the following format:
;;
;;   #Refined{v: T | (P v)}
;;
;; where T is a type (schema) and (P v) is the respresentation of
;; appropriate predicate.
(defmethod print-method RefinedSchema
  [^RefinedSchema rs ^java.io.Writer writer]
  (let [schema (:schema rs)
        schema-name (if (fn? schema?)
                      (schema-utils/fn-name schema)
                      schema)
        f (format "#Refined{v: %s | %s}" schema-name (predicate->str (:pred rs)))]
    (.write writer f)))

(defn coerce
  "Turn function or schema to appropriate predicates"
  [pred]
  {:pre [(or (predicate? pred)
             (ifn? pred)
             (schema? pred))]}
  (cond
    (predicate? pred)
    pred

    (schema? pred)
    (SchemaPredicate. pred)

    (ifn? pred)
    (FunctionPredicate. pred)))

(defn refined
  "Takes type (schema) and a predicate, creating a type that
   should satisfy both basic type and predicate. Note, that predicate might be
   specified as Predicate (protocol), simple function from `dt` type
   to boolean or another type (schema)"
  [dt pred]
  {:pre [(schema? dt)]}
  (RefinedSchema. dt (coerce pred)))

;;
;; boolean operations
;;

(defrecord NotPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (not (predicate-apply pred value)))
  PredicateShow
  (predicate-show [_ sym]
    (format "(not %s)" (predicate->str pred sym true))))

(defn Not [p]
  (NotPredicate. (coerce p)))

(defmethod print-method NotPredicate
  [p ^java.io.Writer writer]
  (predicate-print-method p writer))

(defrecord AndPredicate [p1 p2]
  Predicate
  (predicate-apply [_ value]
    (and (predicate-apply p1 value) (predicate-apply p2 value)))
  PredicateShow
  (predicate-show [_ sym]
    (format "(and %s %s)"
            (predicate->str p1 sym true)
            (predicate->str p2 sym true))))

;; xxx: we can support > 2 arguments here
(defn And
  "Creates predicate that ensures both predicates given are safisfied"
  [p1 p2]
  (AndPredicate. (coerce p1) (coerce p2)))

(defmethod print-method AndPredicate
  [p ^java.io.Writer writer]
  (predicate-print-method p writer))

(defrecord OrPredicate [p1 p2]
  Predicate
  (predicate-apply [_ value]
    (or (predicate-apply p1 value) (predicate-apply p2 value)))
  PredicateShow
  (predicate-show [_ sym]
    (format "(or %s %s)"
            (predicate->str p1 sym true)
            (predicate->str p2 sym true))))

(defn Or
  "Creates the predicate that ensures at least one predicate is satisfied"
  [p1 p2]
  (OrPredicate. (coerce p1) (coerce p2)))

(defmethod print-method OrPredicate
  [p ^java.io.Writer writer]
  (predicate-print-method p writer))

(defrecord OnPredicate [on-fn pred]
  Predicate
  (predicate-apply [_ value]
    (predicate-apply pred (on-fn value)))
  PredicateShow
  (predicate-show [_ sym]
    (let [sym' (format "(%s %s)" (schema-utils/fn-name on-fn) sym)]
      (predicate->str pred sym' false))))

(defn On
  "Creates the predicate to ensure that the result of applying function
   `on-fn` to the value satisfies the predicate `pred`"
  [on-fn pred]
  {:pre [(ifn? on-fn)]}
  (OnPredicate. on-fn (coerce pred)))

(defmethod print-method OnPredicate
  [p ^java.io.Writer writer]
  (predicate-print-method p writer))

;;
;; ordering predicates
;;

(defrecord EqualPredicate [n]
  Predicate
  (predicate-apply [_ value]
    (= value n))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s = %s" sym n)))

(defmethod print-method EqualPredicate
  [p writer]
  (predicate-print-method p writer))

(defn Equal
  "A value that must be = n"
  [n]
  (EqualPredicate. n))

(defrecord LessPredicate [n]
  Predicate
  (predicate-apply [_ value]
    (< value n))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s < %s" sym n)))

(defmethod print-method LessPredicate
  [p writer]
  (predicate-print-method p writer))

(defn Less
  "A value that must be < n"
  [n]
  (LessPredicate. n))

(defrecord LessOrEqualPredicate [n]
  Predicate
  (predicate-apply [_ value]
    (<= value n))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s ≤ %s" sym n)))

(defmethod print-method LessOrEqualPredicate
  [p writer]
  (predicate-print-method p writer))

(defn LessOrEqual
  "A value that must be < n"
  [n]
  (LessOrEqualPredicate. n))

(defrecord GreaterPredicate [n]
  Predicate
  (predicate-apply [_ value]
    (< n value))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s < %s" n sym)))

(defmethod print-method GreaterPredicate
  [p writer]
  (predicate-print-method p writer))

(defn Greater
  "A value that must be > n"
  [n]
  (GreaterPredicate. n))

(defrecord GreaterOrEqualPredicate [n]
  Predicate
  (predicate-apply [_ value]
    (<= n value))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s ≤ %s" n sym)))

(defmethod print-method GreaterOrEqualPredicate
  [p writer]
  (predicate-print-method p writer))

(defn GreaterOrEqual
  "A value that must be >= n"
  [n]
  (GreaterOrEqualPredicate. n))

(defrecord OpenIntervalPredicate [a b]
  Predicate
  (predicate-apply [_ value]
    (< a value b))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s ∈ (%s, %s)" sym a b)))

(defn OpenInterval
  "a < value < b"
  [a b]
  {:pre [(< a b)]}
  (OpenIntervalPredicate. a b))

(defrecord ClosedIntervalPredicate [a b]
  Predicate
  (predicate-apply [_ value]
    (<= a value b))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s ∈ [%s, %s]" sym a b)))

(defn ClosedInterval
  "a <= value <= b"
  [a b]
  {:pre [(<= a b)]}
  (ClosedIntervalPredicate. a b))

(defrecord OpenClosedIntervalPredicate [a b]
  Predicate
  (predicate-apply [_ value]
    (and (< a value) (<= value b)))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s ∈ (%s, %s]" sym a b)))

(defn OpenClosedInterval
  "a < value <= b"
  [a b]
  {:pre [(< a b)]}
  (OpenClosedIntervalPredicate. a b))

(defrecord ClosedOpenIntervalPredicate [a b]
  Predicate
  (predicate-apply [_ value]
    (and (<= a value) (< value b)))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s ∈ [%s, %s)" sym a b)))

(defn ClosedOpenInterval
  "a <= value < b"
  [a b]
  {:pre [(< a b)]}
  (ClosedOpenIntervalPredicate. a b))

(defn Epsilon [center radius]
  (OpenInterval (- center radius) (+ center radius)))

;;
;; numeric predicates
;;

(def Even (FunctionPredicate. even?))

(def Odd (FunctionPredicate. odd?))

(defrecord ModuloPredicate [div o]
  Predicate
  (predicate-apply [_ value]
    (= o (mod value div)))
  PredicateShow
  (predicate-show [_ sym]
    (format "%s mod %s = %s" sym div o)))

(defn Modulo
  "The value modulus by div = o"
  [div o]
  (ModuloPredicate. div o))

(defn DivisibleBy [n]
  (Modulo n 0))

(defn NonDivisibleBy [n]
  (Not (DivisibleBy n)))

;;
;; numeric types
;;

(defn PositiveOf [dt]
  {:pre [(schema? dt)]}
  (refined dt (Greater 0)))

(defn NegativeOf [dt]
  {:pre [(schema? dt)]}
  (refined dt (Less 0)))

(defn NonNegativeOf [dt]
  {:pre [(schema? dt)]}
  (refined dt (GreaterOrEqual 0)))

(defn NonPositiveOf [dt]
  {:pre [(schema? dt)]}
  (refined dt (LessOrEqual 0)))

(def PositiveInt (PositiveOf s/Int))

(def NegativeInt (NegativeOf s/Int))

(def NonNegativeInt (NonNegativeOf s/Int))

(def NonPositiveInt (NonPositiveOf s/Int))

(def PositiveDouble (PositiveOf double))

(def NegativeDouble (NegativeOf double))

(def NonNegativeDouble (NonNegativeOf double))

(def NonPositiveDouble (NonPositiveOf double))

(defn EpsilonOf [dt a b]
  {:pre [(schema? dt)]}
  (refined dt (OpenInterval a b)))

;;
;; ordering types
;;

(defn OpenIntervalOf
  "a < value < b"
  [dt a b]
  {:pre [(schema? dt)]}
  (refined dt (OpenInterval a b)))

(defn ClosedIntervalOf
  "a <= value <= b"
  [dt a b]
  {:pre [(schema? dt)]}
  (refined dt (ClosedInterval a b)))

(defn OpenClosedIntervalOf
  "a < value <= b"
  [dt a b]
  {:pre [(schema? dt)]}
  (refined dt (OpenClosedInterval a b)))

(defn ClosedOpenIntervalOf
  "a <= value < b"
  [dt a b]
  {:pre [(schema? dt)]}
  (refined dt (ClosedOpenInterval a b)))

;;
;; strings & chars
;;

(def NonEmptyStr
  (refined s/Str (Not (FunctionPredicate. cstr/blank?))))

(defn BoundedSizeStr
  ([min max] (BoundedSizeStr min max false))
  ([min max trimmed?]
   {:pre [(<= min max)
          (boolean? trimmed?)]}
   (let [count-chars (if-not trimmed?
                       count
                       #(count (cstr/trim %1)))]
     (refined s/Str (On count-chars (ClosedInterval min max))))))

(def DigitChar #"^[0-9]$")

(def ASCIILetterChar #"^[a-zA-Z]$")

(def ASCIILetterOrDigitChar #"^[0-9a-zA-Z]$")

(def BitChar #"^[0|1]$")

(def BitStr #"^[0|1]*$")

(defn parsable-int? [s]
  (try
    (Integer/parseInt s)
    true
    (catch NumberFormatException _ false)))

(def IntStr (refined NonEmptyStr parsable-int?))

(defn parsable-float? [s]
  (try
    (Float/parseFloat s)
    true
    (catch NumberFormatException _ false)))

(def FloatStr (refined NonEmptyStr parsable-float?))

(defn parsable-uri? [uri]
  (try
    (URI. uri)
    true
    (catch URISyntaxException _ false)))

(def Uri (FunctionPredicate. parsable-uri?))

(def UriStr (refined NonEmptyStr Uri))

(defn parsable-url? [url]
  (try
    (URL. url)
    true
    (catch MalformedURLException _ false)))

(def Url (FunctionPredicate. parsable-url?))

(def UrlStr (refined NonEmptyStr Url))

;;
;; string predicates
;;

(defn StartsWith [prefix]
  (FunctionPredicate. #(starts-with? % prefix)))

(defn StartsWithStr [prefix]
  (refined s/Str (StartsWith prefix)))

(defn EndsWith [suffix]
  (FunctionPredicate. #(ends-with? % suffix)))

(defn EndsWithStr [suffix]
  (refined s/Str (EndsWith suffix)))

(defn Includes [substr]
  (FunctionPredicate. #(includes? % substr)))

(defn IncludesStr [substr]
  (refined s/Str (Includes substr)))

(def LowerCased
  (FunctionPredicate. #(= %1 (cstr/lower-case %1))))

(def LowerCasedStr
  (refined s/Str LowerCased))

(def UpperCased
  (FunctionPredicate. #(= %1 (cstr/upper-case %1))))

(def UpperCasedStr
  (refined s/Str UpperCased))

;;
;; collection predicates
;;

(def Empty
  (reify
    Predicate
    (predicate-apply [_ value]
      (empty? value))
    PredicateShow
    (predicate-show [_ sym]
      (format "%s = ∅" sym))))

(def NonEmpty
  (reify
    Predicate
    (predicate-apply [_ value]
      (not (empty? value)))
    PredicateShow
    (predicate-show [_ sym]
      (format "%s ≠ ∅" sym))))

(defn BoundedSize [left right]
  {:pre [(integer? left)
         (integer? right)
         (pos? left)
         (pos? right)]}
  (On count (ClosedInterval left right)))

(defrecord DistinctByPredicate [f]
  Predicate
  (predicate-apply [_ value]
    (distinct? (map f value)))
  PredicateShow
  (predicate-show [_ sym]
    (if (= identity f)
      (format "(distinct? %s)" sym)
      (format "(distinct-by? %s %s)" (schema-utils/fn-name f) sym))))

(defmethod print-method DistinctByPredicate
  [p ^java.io.Writer writer]
  (predicate-print-method p writer))

(def Distinct
  (DistinctByPredicate. identity))

(defn DistinctBy [f]
  {:pre [(ifn? f)]}
  (DistinctByPredicate. f))

(defrecord ForallPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (every? (partial predicate-apply pred) value))
  PredicateShow
  (predicate-show [_ sym]
    (let [sym' (str sym "'")]
      (format "∀%s ∊ %s: %s" sym' sym (predicate->str pred sym' false)))))

(defmethod print-method ForallPredicate
  [p ^java.io.Writer writer]
  (predicate-print-method p writer))

(defn Forall [p]
  (ForallPredicate. (coerce p)))

(defrecord ExistsPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (not (nil? (some? (partial predicate-apply pred) value))))
  PredicateShow
  (predicate-show [_ sym]
    (let [sym' (str sym "'")]
      (format "∃%s ∊ %s: %s" sym' sym (predicate->str pred sym' false)))))

(defmethod print-method ExistsPredicate
  [p ^java.io.Writer writer]
  (predicate-print-method p writer))

(defn Exists [p]
  (ExistsPredicate. (coerce p)))

(defn First [p]
  (On first (coerce p)))

(defn Second [p]
  (On second (coerce p)))

(defrecord IndexPredicate [n pred]
  Predicate
  (predicate-apply [_ value]
    (predicate-apply pred (nth value n)))
  PredicateShow
  (predicate-show [_ sym]
    (let [sym' (str sym "'")]
      (format "%s = %s[%s]: %s" sym' sym n (predicate->str pred sym' false)))))

(defmethod print-method IndexPredicate
  [p ^java.io.Writer writer]
  (predicate-print-method p writer))

(defn Index [n p]
  {:pre [(integer? n)]}
  (IndexPredicate. n (coerce p)))

(defn Rest [p]
  (On rest (Forall p)))

(defn Last [p]
  (On last (coerce p)))

(defn Butlast [p]
  (On butlast (Forall p)))

(defrecord PairwisePredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (->> (map vector value (rest value))
         (every? (partial predicate-apply pred))))
  PredicateShow
  (predicate-show [_ sym]
    (let [sym' (format "[%s[i], %s[i+1]]" sym sym)]
      (format "∀i ∊ [0, (dec (count %s))): %s"
              sym
              (predicate->str pred sym' false)))))

(defmethod print-method PairwisePredicate
  [p ^java.io.Writer writer]
  (predicate-print-method p writer))

(defn Pairwise [p]
  (PairwisePredicate. (coerce p)))

;;
;; more ordering predicates
;;

(defn AscendingOn [f]
  {:pre [(ifn? f)]}
  (Pairwise (fn [[a b]]
              (<= (compare (f a) (f b)) 0))))

(defn DescendingOn [f]
  {:pre [(ifn? f)]}
  (Pairwise (fn [[a b]]
              (<= 0 (compare (f a) (f b))))))

(defn AscendingBy [f]
  {:pre [(ifn? f)]}
  (Pairwise #(<= (f (first %1) (second %1)) 0)))

(defn DescendingBy [f]
  {:pre [(ifn? f)]}
  (Pairwise #(<= 0 (f (first %1) (second %1)))))

(def Ascending
  (AscendingBy compare))

(def Descending
  (DescendingBy compare))

;;
;; collection types
;;

(def EmptyList (refined [] Empty))

(def EmptySet (refined #{} Empty))

(def EmptyMap (refined {} Empty))

(defn NonEmptyListOf [dt]
  {:pre [(schema? dt)]}
  (refined [dt] NonEmpty))

(defn NonEmptyMapOf [key-dt value-dt]
  {:pre [(schema? key-dt)
         (schema? value-dt)]}
  (refined {key-dt value-dt} NonEmpty))

(defn NonEmptySetOf [dt]
  {:pre [(schema? dt)]}
  (refined #{dt} NonEmpty))

(defn BoundedListOf
  ([dt size] (BoundedListOf dt size size))
  ([dt left right]
   {:pre [(schema? dt)
          (<= 0 left right)]}
   (refined [dt] (BoundedSize left right))))

(defn BoundedSetOf
  ([dt size] (BoundedSetOf dt size size))
  ([dt left right]
   {:pre [(schema? dt)
          (<= 0 left right)]}
   (refined #{dt} (BoundedSize left right))))

(defn BoundedMapOf
  ([key-dt value-dt size] (BoundedMapOf key-dt value-dt size size))
  ([key-dt value-dt left right]
   {:pre [(schema? key-dt)
          (schema? value-dt)
          (<= 0 left right)]}
   (refined {key-dt value-dt} (BoundedSize left right))))

(defn SingleValueListOf [dt]
  {:pre [(schema? dt)]}
  (BoundedListOf dt 1))

(defn SingleValueSetOf [dt]
  {:pre [(schema? dt)]}
  (BoundedSetOf dt 1))

(defn SingleValueMapOf [key-dt value-dt]
  {:pre [(schema? key-dt)
         (schema? value-dt)]}
  (BoundedMapOf key-dt value-dt 1))

(defn DistinctListOf [dt]
  {:pre [(schema? dt)]}
  (refined [dt] Distinct))

(defn NonEmptyDistinctListOf [dt]
  {:pre [(schema? dt)]}
  (refined (DistinctListOf dt) NonEmpty))

;;
;; maps
;;

(defn AtLeastMap [dt]
  {:pre [(map? dt)]}
  (assoc dt s/Any s/Any))

(defn NonStrictMap [dt]
  {:pre [(map? dt)]}
  (->> dt
       (map
        (fn [[k v]]
          [(s/optional-key k) (s/maybe v)]))
       (into {})))

;;
;; simple sum type
;;

(defn dispatch-on
  "Define a conditional schema by specifying determinant function
   (most likely a keyword) followed by the list of potential values
   and appropriate schemas. Throws if the result of determinant
   function does not confirm any listed value (the same as conditional
   does when no match found). In case subtypes are maps, please consider
   using Struct and StructDispatch, that would give you flexibility to deal
   with constrains (guards).

   Last pair treats :else value the same way conditional does.
   Has optional last symbol parameter to be returned in error if none of
   conditions match.

   Quick example:

   (def Point (BoundedListOf double 2))
   (def Dot (SingleValueListOf Point))
   (def Line (BoundedListOf Point 2))
   (def Triangle (s/constrained (BoundedListOf Point 3) #(not (singular? %))))
   (def RandomShape (NonEmptyListOf Point))

   (def Polygon
     (dispatch-on count
       1 Dot
       2 Line
       3 Triangle
       :else RandomShape))"
  [key-fn & subtypes]
  {:pre [(not (empty? subtypes))
         (or (even? (count subtypes))
             (and (symbol? (last subtypes))
                  (>= (count subtypes) 3)))]}
  (let [pairs (partition 2 subtypes)
        [last-key last-type] (last pairs)
        all-pairs (concat (mapcat (fn [[value type]]
                                    [#(= value (key-fn %)) type])
                                  (butlast pairs))
                          [(if (= :else last-key)
                             :else
                             #(= last-key (key-fn %)))
                           last-type]
                          (if (odd? (count subtypes))
                            [(last subtypes)]
                            []))]
    (apply s/conditional all-pairs)))

;;
;; guarded structs
;;

(defprotocol Guardable
  (append-guard [this guard])
  (get-guards [this]))

(defn cleanup-guards [guards k]
  (remove #(contains? (:slice-set %) k) guards))

(def-map-type StructMap [data guards mta]
  (meta [_] mta)
  (with-meta [_ m] (StructMap. data guards m))
  (keys [_] (keys data))
  (assoc [_ k v] (StructMap. (assoc data k v) guards mta))
  (dissoc [_ k] (StructMap. (dissoc data k) (cleanup-guards guards k) mta))
  (get [_ k default-value] (get data k default-value)))

(extend-type StructMap
  Guardable
  (append-guard [^StructMap this guard]
    (StructMap. (.data this) (conj (.guards this) guard) (.mta this)))
  (get-guards [^StructMap this] (.guards this))
  s/Schema
  (spec [this] this)
  (explain [^StructMap this]
    (cons 'guarded-struct (map s/explain (.data this))))
  schema-spec/CoreSpec
  (subschemas [^StructMap this]
    [(.data this)])
  (checker [^StructMap this params]
    (fn [x]
      (let [main-checker (schema-spec/sub-checker {:schema (.data this)} params)
            tx (main-checker x)]
        (if (schema-utils/error? tx)
          tx
          (reduce (fn [_ {:keys [slice guard name]}]
                    (let [x' (select-keys x slice)
                          next-schema (s/pred guard (or name 'not-complaint-with-guard))
                          checker (schema-spec/sub-checker {:schema next-schema} params)
                          tx' (checker x')]
                      (when (schema-utils/error? tx')
                        (reduced tx'))))
                  nil
                  (get-guards this)))))))

(defn guards->str [guards]
  (if (empty? guards)
    ""
    (->> guards
         (map (fn [{:keys [name slice]}]
                (format "    <%s> over %s" name (pr-str slice))))
         (cstr/join "\n")
         (format "\n  Guarded with\n%s"))))

(defmethod print-method StructMap
  [^StructMap struct ^java.io.Writer writer]
  (let [all-guards (get-guards struct)
        f (format "#<StructMap %s%s>"
                  (.data struct)
                  (guards->str all-guards))]
    (.write writer f)))

(defn map->struct [data]
  (StructMap. data [] nil))

(defn Struct
  "Defines map-like schema that you can further restrict with guards still having
   the flexibility to add new fields or remove existing."
  [& key-values]
  {:pre [(even? (count key-values))]}
  (map->struct (apply hash-map key-values)))

(defn guard
  "Restrict given Struct or StructDispatch the same way s/contrained does, but gives you
   flexibility to transform structs whenever necessary by adding or removing fields (using
   `assoc` and `dissoc` as you would do with the plain map). Note, that `dissoc`
   operation cleans up guard when key under the question is mentioned in `keys-slice`
   (that's actually the only reason you need to specify a slice of keys in advance,
   as there is no way to compute them prior to executing checker function)."
  ([struct keys-slice guard-fn]
   (guard struct keys-slice guard-fn nil))
  ([struct keys-slice guard-fn guard-name]
   {:pre [(satisfies? Guardable struct)
          (ifn? guard-fn)
          (not (empty? keys-slice))
          (or (nil? guard-name) (symbol? guard-name))]}
   (let [new-guard {:slice keys-slice
                    :slice-set (set keys-slice)
                    :guard guard-fn
                    :name guard-name}]
     (append-guard struct new-guard))))

(defn apply-struct-updates-to [updates base]
  (reduce
   (fn [state [op & args]]
     (case op
       :assoc (assoc state (first args) (second args))
       :dissoc (dissoc state (first args))))
   base
   updates))

(defn append-guards-to [guards schema]
  (reduce
   (fn [state guard]
     (append-guard state guard))
   schema
   guards))

(def-map-type StructDispatchMap [keys-slice
                                 downstream-slice
                                 dispatch-fn
                                 options
                                 guards
                                 updates
                                 mta]
  (meta [_] mta)
  (with-meta [_ m] (StructDispatchMap.
                    keys-slice
                    downstream-slice
                    dispatch-fn
                    options
                    guards
                    updates
                    m))
  (keys [_] (keys (apply-struct-updates-to updates {})))
  (assoc [_ k v] (StructDispatchMap.
                  keys-slice
                  downstream-slice
                  dispatch-fn
                  options
                  guards
                  (conj updates [:assoc k v])
                  mta))
  (dissoc [_ k]
          (cond
            (contains? keys-slice k)
            (throw (IllegalArgumentException.
                    (str "You are trying to dissoc key '"
                         k
                         "' that is used in dispatch function. "
                         "Even tho' it's doable theoratically, we are kindly encourage you "
                         "avoid such kind of manipulations. It's gonna be a mess.")))

            (contains? downstream-slice k)
            (throw (IllegalArgumentException.
                    (str "Meh. Would not work. One of the options provided actually "
                         "relies on the key '" k "'. Sorry, but I cannot take a risk here.")))

            :else
            (StructDispatchMap.
             keys-slice
             downstream-slice
             dispatch-fn
             options
             guards
             (conj updates [:dissoc k])
             mta)))
  (get [_ k default-value] (get (apply-struct-updates-to updates {}) k default-value)))

(defmethod print-method StructDispatchMap
  [^StructDispatchMap struct ^java.io.Writer writer]
  (let [options (->> (.options struct)
                     (map (fn [[value option]]
                            (format "    %s => %s" value option)))
                     (cstr/join "\n"))
        all-guards (get-guards ^Guardable struct)
        guarded (guards->str all-guards)
        f (format "#<StructDispatch on %s:\n%s%s>"
                  (.dispatch-fn struct)
                  options
                  guarded)]
    (.write writer f)))

(extend-type StructDispatchMap
  Guardable
  (append-guard [^StructDispatchMap this guard]
    (StructDispatchMap.
     (.keys-slice this)
     (.downstream-slice this)
     (.dispatch-fn this)
     (.options this)
     (conj (.guards this) guard)
     (.updates this)
     (.mta this)))
  (get-guards [^StructDispatchMap this] (.guards this))
  s/Schema
  (spec [this] this)
  (explain [^StructDispatchMap this]
    (cons 'struct-dispatch (map s/explain (map second (.options this)))))
  schema-spec/CoreSpec
  (subschemas [^StructDispatchMap this]
    (map second (.options this)))
  (checker [^StructDispatchMap this params]
    (fn [x]
      (let [dispatch-value ((.dispatch-fn this) (select-keys x (.keys-slice this)))
            dispatch-schema (or (->> (.options this)
                                     (filter #(= dispatch-value (first %)))
                                     first)
                                ;; use `:else` branch when set
                                (let [[k v] (last (.options this))]
                                  (when (= :else k) [:else v])))]
        (if (nil? dispatch-schema)
          (schema-utils/error (format "Dispatch value '%s' not found among options %s"
                                      dispatch-value
                                      (mapv first (.options this))))
          (let [dispatch-schema' (->> dispatch-schema
                                      second
                                      (append-guards-to (get-guards this))
                                      (apply-struct-updates-to (.updates this)))
                checker (schema-spec/sub-checker {:schema dispatch-schema'} params)]
            (checker x)))))))

(defn StructDispatch
  "Works the same way as `dispatch-on` but creates a data structure similar to struct
   that might be updated with assoc/dissoc and guarded using `guard` function to created
   delayed contrains.

   If dispatch function is not a keyword (read 'field') you need to specify keys slice
   to prevent dissoc fields necessary to make a dispatch further. Each suboption should be
   either map, StructMap or StructDispatch, map would be converted to Struct.

   Putting last option with ':else' as a dispatch result would match anything if the
   appropriate value was not found earlier."
  [& args]
  (let [fa (first args)
        [keys-slice dispatch-fn rest-args]
        (if (keyword? fa)
          [(set [fa]) fa (rest args)]
          [(set fa) (second args) (drop 2 args)])]
    (when (empty? rest-args)
      (throw (IllegalArgumentException. "no options provided")))

    (when (odd? (count rest-args))
      (throw (IllegalArgumentException. "dispatch argument could not be paired")))

    (let [options (->> rest-args
                       (partition 2)
                       (map (fn [[k v]]
                              (cond
                                (instance? StructDispatchMap v) [k v]
                                (instance? StructMap v) [k v]
                                (map? v) [k (map->struct v)]
                                (satisfies? s/Schema v) [k v]
                                :else (throw
                                       (IllegalArgumentException.
                                        (format (str "Invalid dispatch subtype given for <%s>: %s\n"
                                                     "Should be one of the following: "
                                                     "StructMap, StructDispatch, map or any Schema")
                                                k
                                                v)))))))
          overlap (->> options
                       (map first)
                       (frequencies)
                       (filter (fn [[k n]] (< 1 n)))
                       (map first))
          _ (when-not (empty? overlap)
              (throw
               (IllegalArgumentException.
                (format "Some of the dispatch options listed more than once: %s"
                        overlap))))
          downstream-slice (->> options
                                (mapcat (fn [[k v]]
                                          (if-not (instance? StructDispatchMap v)
                                            []
                                            (into (.keys-slice ^StructDispatchMap v)
                                                  (.downstream-slice ^StructDispatchMap v)))))
                                (set))]
      (StructDispatchMap. keys-slice downstream-slice dispatch-fn options [] [] nil))))
