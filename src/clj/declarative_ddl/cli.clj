(ns declarative-ddl.cli
  (:require [clojure.edn :as edn]
            [clojure.tools.cli :as cli]
            [clojure.pprint :as pprint]
            [declarative-ddl.migrator.core :as migrator])
  #_(:import #_[ch.qos.logback Logger Level]
           [org.slf4j Logger LoggerFactory Level]))

(def cli-options [["-d" "--dir DIRECTORY" "Directory"]
                  ["-b" "--db-url DB_URL" "Database URL"]
                  ["-e" "--execute" "Execute the DDL"]])

(defn usage [options-summary]
  (->> ["This is my program. There are many like it, but this one is mine."
        ""
        "Usage: declarative-ddl [options] action"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  make-migration   Create the next migration file"
        "  migrate          Migrate through most current migration"
        ""
        "Please refer to the manual page for more information."]
       (clojure.string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (clojure.string/join \newline errors)))


(defn validate-args [args]
  (let [{:keys [options arguments errors summary]} (cli/parse-opts args cli-options)
        #_ (pprint/pprint {:options options
                          :arguments arguments
                          :errors errors
                          :summary summary})]
    (cond
      (:help options)
      {:exit-message (usage summary) :ok? true}
      errors
      {:exit-message (error-msg errors)}
      (and (= 1 (count arguments))
           (#{"make-migration" "migrate"} (first arguments)))
      {:action (first arguments) :options options}
      :else
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn -main [& args]
  (try
    ;; (-> #_(Logger/getRootLogger)
    ;;     (LoggerFactory/getLogger Logger/ROOT_LOGGER_NAME)
    ;;     (.setLevel Level/WARN))
    ;; (-> (LoggerFactory/getLogger "declarative-ddl")
    ;;     (.setLevel Level/ALL))
    (let [{:keys [action options exit-message ok?]} (validate-args args)
          #_ (clojure.pprint/pprint
             {:action action
              :options options
              :exit-message exit-message
              :ok? ok?
              :execute (:execute options)
              :not-execute (not (:execute options))})
          #_ (pprint/pprint entities)]
     (if exit-message
       (exit (if ok? 0 1) exit-message)
       (let [dddl-dir (str (:dir options) "/resources/declarative-ddl")
             entities-filepath (str dddl-dir "/entities.edn")
             entities (-> entities-filepath slurp edn/read-string)
             migrations-dir-path (str dddl-dir "/migrations")]
        (case action
          "make-migration"
          (migrator/make-migration-file! entities migrations-dir-path)
          "migrate"
          (migrator/migrate!
           (:db-url options)
           migrations-dir-path
           :dry-run (not (:execute options)))
          (throw (ex-info "invalid action" {:action action}))))))
   (catch Throwable exc
     (.printStackTrace exc))))
