(ns declarative-ddl.clj-utils.core)

(defn undasherize [s]
  (clojure.string/replace s "-" "_"))

(defn remove-key-from-kv-pair [[_ v]]
  v)
