(ns schema-refined.macros)

(defmacro ok! [dt value]
  `(cljs.test/is (nil? (schema.core/check ~dt ~value))))

(defmacro not-ok! [dt value]
  `(cljs.test/is (some? (schema.core/check ~dt ~value))))
