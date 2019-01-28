(ns declarative-ddl.migrator.core
  (:require [cognitect.transit :as transit]
            [clojure.test :as test]
            [diff-as-list.core :as dal]
            [clojure.pprint :as pp]
            [clojure.string :as string]
            [declarative-ddl.entities-schemas :as entities-schemas]
            [clojure.spec.alpha :as spec]
            [java-time :as time]
            ;; [disreguard.config :as config]
            [conman.core :as conman]
            [clojure.java.jdbc :as jdbc])
  (:import [java.time ZoneId]))

(defn edn-write [obj fpath]
  (as-> obj $
    (with-out-str (pp/pprint $))
    (spit fpath $)))

(defn edn-read [fpath]
  (-> fpath
      clojure.java.io/reader
      java.io.PushbackReader.
      clojure.edn/read))

(defn fnames-in-dir [dir]
  (as-> dir $
    (file-seq $)
    (filter #(.isFile %) $)
    (map #(.getName %) $)
    (sort $)))

(defn- undasherize [s]
  (string/replace s "-" "_"))

(defn remove-key-from-kv-pair [[_ v]]
  v)

(defn field-type-to-ddl [field-in]
  (let [type-specific-ddl (case (:type field-in)
                            :character
                            (str "VARCHAR(" (:max-length field-in) ")")
                            :foreign-key
                            (str "INTEGER REFERENCES " (-> (:references field-in) name undasherize))
                            :boolean
                            "BOOLEAN"
                            :date-time
                            "TIMESTAMP WITH TIME ZONE")
        unique-ddl (when (and (contains? field-in :unique)
                            (:unique field-in))
                     "UNIQUE")
        null-ddl (if (and (contains? field-in :null)
                          (:null field-in))
                   "NULL"
                   "NOT NULL")
        default-ddl (when (contains? field-in :default)
                      (let [default-val-initial (:default field-in)
                            default-val (case default-val-initial
                                          :current-date-time "CURRENT_TIMESTAMP"
                                          default-val-initial)]
                        (str "DEFAULT(" default-val ")")))
        combined-ddl (remove nil? [type-specific-ddl unique-ddl null-ddl default-ddl])]
    (as-> combined-ddl $
      (interpose " " $)
      (apply str $))))

(defn create-table-field [field-in]
  (let [field-name (-> (:name field-in)
                       (#(if (= (:type field-in) :foreign-key)
                          (str % "-id")
                          %))
                       undasherize)]
   (str "    " field-name " " (field-type-to-ddl field-in))))

(defn create-table [table-in]
  (let [fields-sql (as-> (:fields table-in) $
                    (map remove-key-from-kv-pair $)
                    (map create-table-field $)
                    (interpose ",\n" $))

        sql-vec (concat ["CREATE TABLE " (undasherize (:name table-in)) " (\n"]
                        ["    id SERIAL PRIMARY KEY,\n"]
                        fields-sql
                        ["\n);"])]
      (apply str sql-vec)))

(defn value-difference [val-diff]
  (cond
    (empty? (:path val-diff))
    (cond
      (nil? (:val-1 val-diff))
      (as-> (:val-2 val-diff) $
        (map remove-key-from-kv-pair $)
        (map create-table $)
        (interpose "\n" $)
        (apply str $))
      :else
      (throw (Exception. "this case has not yet been implemented - value of val-1")))
    :else
    (throw (Exception. "this case has not yet been implemented - size of path"))))

(defn diff-to-ddl [diff-in]
  (as-> (map value-difference (:value-differences diff-in)) $
    (interpose "\n" $)
    (apply str $)))

;; regarding (dissoc :choices) - postgres can do a check constraint on the data in
;; a character field, but for simplicity sake for now, I will ignore that feature
;; and leave it to the user of auto-admin to make that restriction
(defn xform-fields-for-diff [fields-vec]
  (reduce
   (fn [accum field]
     (let [new-field (-> field (dissoc :choices))]
       (assoc accum (keyword (:name field)) new-field)))
   {}
   fields-vec))

;; (spec/fdef xform-fields-to-pure-map
;;            :args (spec/cat :field-vec (spec/coll-of :entities-schemas/field)))



(defn xform-entities-for-diff [entities-vec]
  (reduce
   (fn [accum entity]
     (let [new-entity (-> entity
                          (assoc :fields (xform-fields-for-diff (:fields entity)))
                          (dissoc :repr))]
       (assoc accum (keyword (:name new-entity)) new-entity)))
   {}
   entities-vec))

(defn make-migration [entities-old-diff-list entities-new]
  (let [entities-patched (reduce #(dal/patch %1 %2) nil entities-old-diff-list)
        entities-for-diff (xform-entities-for-diff entities-new)]
    (dal/diffl entities-patched entities-for-diff)))


(def migrations-dir-name "migrations-dddl")
(def migrations-dir (clojure.java.io/file migrations-dir-name))

(defn get-all-migration-file-names []
  (fnames-in-dir migrations-dir))

;; (def ^:dynamic entities nil)


(defn make-migration-file! [entities-in]
  (let [;; entities-in entities
        ensure-mig-dir-exists-res
        (when-not (.exists migrations-dir)
          (.mkdir migrations-dir)
          (when-not (.exists migrations-dir)
            (throw (Exception. "should exist"))))
        existing-mig-fpaths (map #(str migrations-dir-name "/" %) (get-all-migration-file-names))
        diffs (map edn-read existing-mig-fpaths)
        next-diff (make-migration diffs entities-in)
        mig-num (-> (count existing-mig-fpaths) inc)
        mig-time-str (as-> (time/zoned-date-time) $
                       (.withZoneSameInstant $ (ZoneId/of "UTC"))
                       (time/format "yyyy-MM-dd'T'HH-mm-ss-SSSX" $))
        mig-fname (format "%04d-migration-%s.edn" mig-num mig-time-str)
        file-out-path (str migrations-dir-name "/" mig-fname)]
    (edn-write next-diff file-out-path)))

(def migration-table-ddl
  (clojure.string/join
   "\n"
   ["CREATE TABLE migrations ("
    "    number INTEGER PRIMARY KEY,"
    "    time_applied TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT CURRENT_TIMESTAMP"
    ");"]))

(defn does-migration-table-exist? [db-conn]
  (-> (jdbc/query db-conn ["SELECT to_regclass('public.migrations');"])
      first
      :to_regclass
      (#(not (nil? %)))))

(defn get-last-migration-number [db-conn]
  (if (not (does-migration-table-exist? db-conn))
    nil
    (-> (jdbc/query db-conn ["SELECT MAX(number) FROM migrations"])
        first
        :max)))

(defn get-migration-number-from-filename [migration-fname]
  (-> (subs migration-fname 0 4) Integer/parseInt))


(defn migrate! [db-url]
  (let [
        ;; env (cprop.core/load-config
        ;;      :merge
        ;;      [(cprop.source/from-system-props)
        ;;       (cprop.source/from-env)])
        db-conn (conman/connect! {:jdbc-url db-url})
        all-mig-file-names (get-all-migration-file-names)]
    (if (empty? all-mig-file-names)
      ;; TODO - better logging
      (println "nothing to do here.  No migration files")
      (try
        (jdbc/with-db-transaction [t-conn db-conn]
          (let [last-mig-num (get-last-migration-number t-conn)
                db-says-initial? (nil? last-mig-num)
                files-say-initial?
                (let [first-mig-num-from-file
                      (get-migration-number-from-filename (first all-mig-file-names))]
                  (= first-mig-num-from-file 1))
                both-say-initial? (= db-says-initial? files-say-initial?)]
            (if (not both-say-initial?)
              (throw (Exception. (str "conflicting results on whether this is the initial migration or not. db-says-initial? = " db-says-initial? ", files-say-initial? = " files-say-initial?)))
              (let [remaining-mig-file-names (if (nil? last-mig-num)
                                               all-mig-file-names
                                               (filter #(> last-mig-num (get-migration-number-from-filename %)) all-mig-file-names))
                    init-result (when (nil? last-mig-num)
                                  (jdbc/execute! t-conn [migration-table-ddl]))]
                (doseq [remaining-mig-file-name remaining-mig-file-names]
                  (let [mig-file-path (str migrations-dir-name "/" remaining-mig-file-name)
                        mig-number (get-migration-number-from-filename remaining-mig-file-name)
                        mig-diff (edn-read mig-file-path)
                        the-ddl (diff-to-ddl mig-diff)
                        _ (println the-ddl)
                        ddl-exec-res (jdbc/execute! t-conn the-ddl)
                        migrations-table-append-res
                        (jdbc/insert! t-conn :migrations {:number mig-number})]))))))
        (finally
          (conman/disconnect! db-conn))))))

;; (defn migrate-tester []
;;   (let [migration-diff (make-migration [] entities-for-testing)
;;         the-ddl (diff-to-ddl migration-diff)]
;;     (print the-ddl)))
