(ns schema-refined.runner
  (:require [doo.runner :refer-macros [doo-tests]]
            [schema-refined.core-test]))

(doo-tests 'schema-refined.core-test)
