(ns schema-refined.cljs
  (:require [schema-refined.core :as sr]))

;; based on potemkin's def-map-type
;; but for cljs

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
     RefinedMapType
     ~@body

     cljs.core.ICollection
     (-conj [this# o#]
       (cond
         (map? o#)
         (reduce #(apply assoc %1 %2) this# o#)

         (instance? js/Map o#)
         (reduce #(apply assoc %1 %2) this# (into {} o#))

         :else
         (if-let [[k# v#] (seq o#)]
           (assoc this# k# v#)
           this#)))

     cljs.core.IWithMeta
     (-with-meta [this# m#]
       (schema-refined.cljs/with-meta* this# m#))

     cljs.core.IMeta
     (-meta [this#]
       (schema-refined.cljs/meta* this#))

     cljs.core.ICounted
     (-count [this#]
       (count (schema-refined.cljs/keys* this#)))

     cljs.core.ISeqable
     (-seq [this#]
       (seq
         (map
           #(vector % (get this# %)
                    (schema-refined.cljs/keys* this#)))))

     cljs.core.IReduce
     (-reduce [this# f#]
       (reduce f# (seq this#)))
     (-reduce [this# f# v#]
       (reduce f# v# (seq this#)))

     cljs.core.IHash
     (-hash [this#]
       (reduce
         (fn [acc# [k# v#]]
           (unchecked-add acc# (bit-xor (cljs.core/-hash k#) (cljs.core/-hash v#))))
         0
         (seq this#)))

     cljs.core.IEquiv
     (-equiv [this# x#]
       (and
         (or (instance? js/Map x#) (map? x#))
         (= x# (into {} this#))))

     js/Object
     (toString [this#]
       (str (into {} this#)))
     (equiv [this# x#]
       (or (identical? this# x#)
           (cljs.core/-equiv this# x#)))

     ;; js/Map
     (get [this# k#]
       (cljs.core/-lookup this# k#))
     (size [this#]
       (count this#))
     (keys [this#]
       (schema-refined.cljs/keys* this#))
     (set [~'_ ~'_ ~'_]
       (throw (UnsupportedOperationException.)))
     (delete [~'_ ~'_]
       (throw (UnsupportedOperationException.)))
     (values [this#]
       (->> this# seq (map second)))
     (entries [this#]
       (seq this#))

     cljs.core.ILookup
     (-lookup [this# k#]
       (cljs.core/-lookup this# k# nil))
     (-lookup [this# k# default#]
       (schema-refined.cljs/get* this# k# default#))

     cljs.core.IAssociative
     (-contains-key? [this# k#]
       (contains? (set (keys this#)) k#))
     (-assoc [this# k# v#]
       (schema-refined.cljs/assoc* this# k# v#))

     cljs.core.IEmptyableCollection
     (-empty [this#]
       (schema-refined.cljs/empty* this#))

     cljs.core.IIterable
     (-iterator [this#]
       (cljs.core/seq-iter this#))

     cljs.core.IMap
     (-dissoc [this# k#]
       (schema-refined.cljs/dissoc* this# k#))

     cljs.core.IFn
     (-invoke [this# k#] (get this# k#))
     (-invoke [this# k# not-found#] (get this# k# not-found#))))
