(ns schema-refined.clojure
  (:require [schema.core :as s]
            [schema.spec.core :as schema-spec]
            [schema.utils :as schema-utils]
            [clojure.string :as cstr]))

;;
;; guarded structs
;;

;; based on potemkin's def-map-type
(defprotocol RefinedMapType
  (empty* [m])
  (get* [m k default])
  (assoc* [m k v])
  (dissoc* [m k])
  (keys* [m])
  (with-meta* [o mta])
  (meta* [o]))

(defmacro -def-map-type
  {:style/indent [2 :form :form [1]]}
  [name params & body]
  `(deftype ~name ~params
     schema_refined.clojure.RefinedMapType
     ~@body

     clojure.lang.MapEquivalence

     clojure.lang.IPersistentCollection
     (equiv [this# x#]
       (and (or (instance? java.util.Map x#) (map? x#))
            (= x# (into {} this#))))
     (cons [this# o#]
       (cond
         (map? o#)
         (reduce #(apply assoc %1 %2) this# o#)

         (instance? java.util.Map o#)
         (reduce #(apply assoc %1 %2) this# (into {} o#))

         :else
         (if-let [[k# v#] (seq o#)]
           (assoc this# k# v#)
           this#)))

     clojure.lang.IObj
     (withMeta [this# m#]
       (schema-refined.clojure/with-meta* this# m#))
     (meta [this#]
       (schema-refined.clojure/meta* this#))

     clojure.lang.Counted
     (count [this#]
       (count (schema-refined.clojure/keys* this#)))

     clojure.lang.Seqable
     (seq [this#]
       (seq
         (map
           #(.entryAt this# %)
           (schema-refined.clojure/keys* this#))))

     clojure.core.protocols.CollReduce
     (coll-reduce [this# f#]
       (reduce f# (seq this#)))
     (coll-reduce [this# f# v#]
       (reduce f# v# (seq this#)))

     clojure.lang.IHashEq
     (hasheq [this#]
       (hash-unordered-coll (or (seq this#) ())))

     Object
     (hashCode [this#]
       (reduce
        (fn [acc# [k# v#]]
          (unchecked-add acc# (bit-xor (clojure.lang.Util/hash k#)
                                       (clojure.lang.Util/hash v#))))
        0
        (seq this#)))
     (equals [this# x#]
       (or (identical? this# x#)
           (and
            (or (instance? java.util.Map x#) (map? x#))
            (= x# (into {} this#)))))
     (toString [this#]
       (str (into {} this#)))

     clojure.lang.ILookup
     (valAt [this# k#]
       (.valAt this# k# nil))
     (valAt [this# k# default#]
       (schema-refined.clojure/get* this# k# default#))

     clojure.lang.Associative
     (containsKey [this# k#]
       (contains? (.keySet this#) k#))
     (entryAt [this# k#]
       (when (contains? (.keySet this#) k#)
         (clojure.lang.MapEntry. k# (get this# k#))))
     (assoc [this# k# v#]
       (schema-refined.clojure/assoc* this# k# v#))
     (empty [this#]
       (schema-refined.clojure/empty* this#))

     java.util.Map
     (get [this# k#]
       (.valAt this# k#))
     (isEmpty [this#]
       (empty? this#))
     (size [this#]
       (count this#))
     (keySet [this#]
       (set (schema-refined.clojure/keys* this#)))
     (put [_ _ _]
       (throw (UnsupportedOperationException.)))
     (putAll [_ _]
       (throw (UnsupportedOperationException.)))
     (clear [_]
       (throw (UnsupportedOperationException.)))
     (remove [_ _]
       (throw (UnsupportedOperationException.)))
     (values [this#]
       (->> this# seq (map second)))
     (entrySet [this#]
       (->> this# seq set))

     java.util.Iterator
     (iterator [this#]
       (clojure.lang.SeqIterator. this#))

     clojure.lang.IPersistentMap
     (assocEx [this# k# v#]
       (if (contains? this# k#)
         (throw (Exception. "Key or value already present"))
         (assoc this# k# v#)))
     (without [this# k#]
       (schema-refined.clojure/dissoc* this# k#))

     clojure.lang.IFn
     (invoke [this# k#] (get this# k#))
     (invoke [this# k# not-found#] (get this# k# not-found#))))

(defprotocol Guardable
  (append-guard [this guard])
  (get-guards [this]))

(defn cleanup-guards [guards k]
  (remove #(contains? (:slice-set %) k) guards))

(-def-map-type StructMap [data guards mta]
  (empty* [_] (StructMap. {} [] {}))
  (get* [_ k default-value] (get data k default-value))
  (assoc* [_ k v] (StructMap. (assoc data k v) guards mta))
  (dissoc* [_ k] (StructMap. (dissoc data k) (cleanup-guards guards k) mta))
  (keys* [_] (keys data))
  (meta* [_] mta)
  (with-meta* [_ m] (StructMap. data guards m)))

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

(-def-map-type StructDispatchMap [keys-slice
                                  downstream-slice
                                  dispatch-fn
                                  options
                                  guards
                                  updates
                                  mta]
  (empty* [_] (StructDispatchMap. [] [] (constantly ::empty) [[::empty {}]] [] [] nil))
  (get* [_ k default-value] (get (apply-struct-updates-to updates {}) k default-value))
  (assoc* [_ k v] (StructDispatchMap.
                    keys-slice
                    downstream-slice
                    dispatch-fn
                    options
                    guards
                    (conj updates [:assoc k v])
                    mta))
  (dissoc* [_ k]
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
  (keys* [_] (keys (apply-struct-updates-to updates {})))
  (meta* [_] mta)
  (with-meta* [_ m] (StructDispatchMap.
                      keys-slice
                      downstream-slice
                      dispatch-fn
                      options
                      guards
                      updates
                      m)))

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
