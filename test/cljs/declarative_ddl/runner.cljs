(ns declarative-ddl.runner
    (:require [doo.runner :refer-macros [doo-tests]]
              [declarative-ddl.validation.core-test]))

(doo-tests 'declarative-ddl.validation.core-test)
