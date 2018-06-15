(ns schema-refined.core
  (:require [schema.core :as s]
            [schema.spec.core :as schema-spec]
            [schema.spec.variant :as schema-variant]
            [schema.utils :as schema-utils]
            [clojure.string :as cstr]
            [potemkin.collections :refer [def-map-type]])
  (:import (java.net URI URISyntaxException URL MalformedURLException)))

;;
;; helpers & basic definitions
;;

(defn schema? [dt]
  (satisfies? s/Schema dt))

(defprotocol Predicate
  (predicate-apply [this value])
  ;; xxx: implement to have better error messages
  #_(predicate-exlain [this])
  ;; xxx: this might be a separate protocol
  ;; in case we want to have some kind of
  ;; default error messages (obviously we do)
  #_(predicate-error [this value]))

(defn predicate? [p]
  (satisfies? Predicate p))

(defrecord FunctionPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (pred value)))

(defrecord SchemaPredicate [schema]
  Predicate
  (predicate-apply [_ value]
    (nil? (s/check schema value))))

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

(defn refined
  "Takes type (schema) and a predicate, creating a type that
   should satisfy both basic type and predicate. Note, that predicate might be
   specified as Predicate (protocol), simple function from `dt` type
   to boolean or another type (schema)"
  [dt pred]
  {:pre [(schema? dt)
         (or (predicate? pred)
             (ifn? pred)
             (schema? pred))]}
  (let [p (cond
            (predicate? pred)
            pred

            (ifn? pred)
            (FunctionPredicate. pred)

            (schema? pred)
            (SchemaPredicate. pred))]
    (RefinedSchema. dt p)))

;;
;; boolean operations
;;

(defrecord NotPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (not (pred value))))

(defn Not [p]
  {:pre [(predicate? p)]}
  (NotPredicate. p))

(defrecord AndPredicate [p1 p2]
  Predicate
  (predicate-apply [_ value]
    (and (p1 value) (p2 value))))

;; xxx: we can support > 2 arguments here
(defn And
  "Creates predicate that ensures both predicates given are safisfied"
  [p1 p2]
  {:pre [(predicate? p1)
         (predicate? p2)]}
  (AndPredicate. p1 p2))

(defrecord OrPredicate [p1 p2]
  Predicate
  (predicate-apply [_ value]
    (or (p1 value) (p2 value))))

(defn Or
  "Creates the predicate that ensures at least one predicate is satisfied"
  [p1 p2]
  {:pre [(predicate? p1)
         (predicate? p2)]}
  (OrPredicate. p1 p2))

(defrecord OnPredicate [on-fn pred]
  Predicate
  (predicate-apply [_ value]
    (pred (on-fn value))))

(defn On
  "Creates the predicate to ensure that the result of applying function
   `on-fn` to the value satisfies the predicate `pred`"
  [on-fn pred]
  {:pre [(ifn? on-fn)
         (predicate? pred)]}
  (OnPredicate. on-fn pred))

;;
;; ordering predicates
;;

(defrecord EqualPredicate [n])

(defn Equal
  "A value that must be = n"
  [n]
  (FunctionPredicate. #(= % n)))

(defrecord LessPredicate [n])

(defn Less
  "A value that must be < n"
  [n]
  (FunctionPredicate. #(< % n)))

(defn LessOrEqual
  "A value that must be < n"
  [n]
  (Or (Less n) (Equal n)))

(defrecord GreaterPredicate [n])

(defn Greater
  "A value that must be > n"
  [n]
  (FunctionPredicate. #(> % n)))

(defn GreaterOrEqual
  "A value that must be >= n"
  [n]
  (Or (Greater n) (Equal n)))

(def Ascending
  (reify Predicate))

(def Descending
  (reify Predicate))

;; xxx: reimplement intervals with records
(defn OpenInterval
  "a < value < b"
  [a b]
  {:pre [(< a b)]}
  (FunctionPredicate. #(< a % b)))

(defn ClosedInterval
  "a <= value <= b"
  [a b]
  {:pre [(<= a b)]}
  (FunctionPredicate. #(<= a % b)))

(defn OpenClosedInterval
  "a < value <= b"
  [a b]
  {:pre [(< a b)]}
  (FunctionPredicate. #(and (< a %1) (<= %1 b))))

(defn ClosedOpenInterval
  "a <= value < b"
  [a b]
  {:pre [(< a b)]}
  (FunctionPredicate. #(and (<= a %1) (< %1 b))))

(defn Epsilon [center radius]
  (OpenInterval (- center radius) (+ center radius)))

;;
;; numeric predicates
;;

(def Even (FunctionPredicate. even?))

(def Odd (FunctionPredicate. odd?))

(defn Modulo
  "The value modulus by div = o"
  [div o]
  (FunctionPredicate. #(= o (mod % num))))

(defn Divisible [n]
  (Modulo n 0))

(defn NonDivisible [n]
  (Not (Divisible n)))

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
  (s/constrained
   s/Str
   #(not (cstr/blank? %))
   'should-not-be-blank))

;; :thinking: can be implemented with AND, not s/constrained
(defn BoundedLengthStr
  ([min max] (BoundedLengthStr min max false))
  ([min max trimmed?]
   (s/constrained
    s/Str
    #(<= min (count (if trimmed? (cstr/trim %1) %1)) max)
    'string-length-should-conform-boundaries)))

(def DigitChar #"^[0-9]$")

(def ASCIILetterChar #"^[a-z]$")

(def ASCIILetterOrDigitChar #"^[0-9a-z]$")

(def BitChar #"^[0|1]$")

(def BitStr #"[0|1]*")

(def IntStr
  (s/constrained
   NonEmptyStr
   (fn [str]
     (try
       (Integer/parseInt str)
       true
       (catch NumberFormatException _ false)))
   'should-be-parsable-int))

(def FloatStr
  (s/constrained
   NonEmptyStr
   (fn [str]
     (try
       (Float/parseFloat str)
       true
       (catch NumberFormatException _ false)))
   'should-be-parsable-float))

(def Uri
  (s/constrained
   NonEmptyStr
   (fn [uri]
     (try
       (URI. uri)
       true
       (catch URISyntaxException _ false)))
   'should-be-parsable-uri))

(def Url
  (s/constrained
   NonEmptyStr
   (fn [url]
     (try
       (URL. url)
       true
       (catch MalformedURLException _ false)))
   'should-be-parsable-url))

;;
;; string predicates
;;

(defn StartsWith [prefix]
  (s/constrained
   s/Str
   #(cstr/starts-with? % prefix)))

(defn EndsWith [suffix]
  (s/constrained
   s/Str
   #(cstr/ends-with? % suffix)))

(defn Includes [substr]
  (s/constrained
   s/Str
   #(cstr/includes? % substr)))

(def LowerCased
  (s/constrained
   s/Str
   #(= %1 (cstr/lower-case %1))
   'should-be-lower-cased))

(def UpperCased
  (s/constrained
   s/Str
   #(= %1 (cstr/upper-case %1))
   'should-be-upper-cased))

;;
;; collection predicates
;;

;; xxx: reimplement Empty/NonEmpty with reify
(def Empty
  (FunctionPredicate. empty?))

(def NonEmpty
  (FunctionPredicate. #(not (empty? %))))

(defn BoundedSize [left right]
  {:pre [(int? left)
         (int? right)
         (pos? left)
         (pos? right)]}
  (On count (ClosedInterval left right)))

;; xxx: use reify
(def UniqueItems
  (FunctionPredicate. #(= (count %1) (set (count %1)))))

(defrecord ForallPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (every? pred value)))

(defn Forall [p]
  {:pre [(predicate? p)]}
  (ForallPredicate. p))

(defrecord ExistsPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (not (nil? (some? pred value)))))

(defn Exists [p]
  {:pre [(predicate? p)]}
  (ExistsPredicate. p))

(defrecord FirstPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (pred (first value))))

;; head in clojure
(defn First [p]
  {:pre [(predicate? p)]}
  (FirstPredicate. p))

(defrecord SecondPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (pred (second value))))

(defn Second [p]
  {:pre [(predicate? p)]}
  (SecondPredicate. p))

(defrecord IndexPredicate [n pred]
  Predicate
  (predicate-apply [_ value]
    (pred (nth value n))))

(defn Index [n p]
  {:pre [(int? n)
         (predicate? p)]}
  (IndexPredicate. n p))

(defrecord RestPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (every? pred (rest value))))

;; tail in clojure
(defn Rest [p]
  {:pre [(predicate? p)]}
  (RestPredicate. p))

(defrecord LastPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (pred (last value))))

(defn Last [p]
  {:pre [(predicate? p)]}
  (LastPredicate. p))

(defrecord ButlastPredicate [pred]
  Predicate
  (predicate-apply [_ value]
    (every? pred (butlast value))))

(defn Butlast [p]
  {:pre [(predicate? p)]}
  (ButlastPredicate. p))

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
   {:pre [(schema? dt)]}
   (refined [dt] (BoundedSize left right))))

(defn BoundedSetOf
  ([dt size] (BoundedSetOf dt size size))
  ([dt left right]
   {:pre [(schema? dt)]}
   (refined #{dt} (BoundedSize left right))))

(defn BoundedMapOf
  ([key-dt value-dt size] (BoundedMapOf key-dt value-dt size size))
  ([key-dt value-dt left right]
   {:pre [(schema? key-dt)
          (schema? value-dt)]}
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

(defn UniqueItemsListOf [dt]
  {:pre [(schema? dt)]}
  (refined [dt] UniqueItems))

(defn NonEmptyUniqueItemsListOf [dt]
  {:pre [(schema? dt)]}
  (refined (UniqueItemsListOf dt) NonEmpty))

;;
;; maps
;;

(defn AtLeast [dt]
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
   using Struct and Dispatch, that would give you flexibility to deal
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

(def-map-type StructDispatch [keys-slice
                              downstream-slice
                              dispatch-fn
                              options
                              guards
                              updates
                              mta]
  (meta [_] mta)
  (with-meta [_ m] (StructDispatch.
                    keys-slice
                    downstream-slice
                    dispatch-fn
                    options
                    guards
                    updates
                    m))
  (keys [_] (keys (apply-struct-updates-to updates {})))
  (assoc [_ k v] (StructDispatch.
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
            (StructDispatch.
             keys-slice
             downstream-slice
             dispatch-fn
             options
             guards
             (conj updates [:dissoc k])
             mta)))
  (get [_ k default-value] (get (apply-struct-updates-to updates {}) k default-value)))

(defmethod print-method StructDispatch
  [^StructDispatch struct ^java.io.Writer writer]
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

(extend-type StructDispatch
  Guardable
  (append-guard [^StructDispatch this guard]
    (StructDispatch.
     (.keys-slice this)
     (.downstream-slice this)
     (.dispatch-fn this)
     (.options this)
     (conj (.guards this) guard)
     (.updates this)
     (.mta this)))
  (get-guards [^StructDispatch this] (.guards this))
  s/Schema
  (spec [this] this)
  (explain [^StructDispatch this]
    (cons 'struct-dispatch (map s/explain (map second (.options this)))))
  schema-spec/CoreSpec
  (subschemas [^StructDispatch this]
    (map second (.options this)))
  (checker [^StructDispatch this params]
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

(defn Dispatch
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
                                (instance? StructDispatch v) [k v]
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
                                          (if-not (instance? StructDispatch v)
                                            []
                                            (into (.keys-slice ^StructDispatch v)
                                                  (.downstream-slice ^StructDispatch v)))))
                                (set))]
      (StructDispatch. keys-slice downstream-slice dispatch-fn options [] [] nil))))
