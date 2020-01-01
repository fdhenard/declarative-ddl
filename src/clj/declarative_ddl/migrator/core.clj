(ns declarative-ddl.migrator.core
  (:require [clojure.pprint :as pp]
            [clojure.string :as string]
            [clojure.java.jdbc :as jdbc]
            [clojure.spec.alpha :as spec]
            [java-time :as time]
            [conman.core :as conman]
            [diff-as-list.core :as dal]
            [declarative-ddl.cljc.core :as dddl-cljc]
            [declarative-ddl.cljc.utils.core :as cljc-utils]
            [declarative-ddl.entities-schemas :as entities-schemas])
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
                            "TIMESTAMP WITH TIME ZONE"
                            :numeric
                            (str "NUMERIC(" (:total-length field-in) ", " (:decimal-places field-in) ")")
                            :date
                            "DATE"
                            :int
                            "INTEGER"
                            )
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
                                          :current-time "CURRENT_TIMESTAMP"
                                          default-val-initial)]
                        (str "DEFAULT(" default-val ")")))
        combined-ddl (remove nil? [type-specific-ddl unique-ddl null-ddl default-ddl])]
    (as-> combined-ddl $
      (interpose " " $)
      (apply str $))))

(defn get-field-name [field-in]
  (-> (:name field-in)
      (#(if (= (:type field-in) :foreign-key)
          (str % "-id")
          %))
      undasherize))

(defn create-table-field [field-in]
  (let [field-validated (spec/assert ::entities-schemas/field field-in)
        field-name (get-field-name field-validated)]
    (str field-name " " (field-type-to-ddl field-validated))))

(defn create-table [table-in]
  (let [fields-sql (as-> (:fields table-in) $
                    (map remove-key-from-kv-pair $)
                    (map #(str "    " (create-table-field %)) $)
                    (interpose ",\n" $))

        sql-vec (concat ["CREATE TABLE " (undasherize (:name table-in)) " (\n"]
                        ["    id SERIAL PRIMARY KEY,\n"]
                        fields-sql
                        ["\n);"])]
      (apply str sql-vec)))

(defn value-difference-to-ddl [val-diff]
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


(defprotocol GetTableNameKeywordAble
  (get-table-name-kw [this]))

(defprotocol Groupable
  (get-group [this]))

(defprotocol PostgresDdlAddDropable
  (get-pg-ddl-add [this])
  (get-pg-ddl-drop [this]))

(defprotocol PostgresDdlAble
  (get-pg-ddl [this forward-or-backward]))

(defprotocol GetTableDefinitionAble
  (get-table-definition [this]))

(defprotocol GetDdlTableNameAble
  (get-ddl-table-name [this]))

(defn get-pg-ddl-from-pg-add-dropable [change-rec forward-or-backward]
  (if (not (satisfies? PostgresDdlAddDropable change-rec))
    (throw (RuntimeException. "cannot get add or drop from non PostgresDdlAddDropable"))
    (let [action-map
          {[:forward :one] get-pg-ddl-add
           [:forward :two] get-pg-ddl-drop
           [:backward :one] get-pg-ddl-drop
           [:backward :two] get-pg-ddl-add}
          missing-in (-> change-rec :diff :missing-in)
          action-fn (get action-map [forward-or-backward missing-in])]
      (action-fn change-rec))))


(defrecord TableAddRemove [diff]
  Groupable
  (get-group [this] :top-level)

  GetTableDefinitionAble
  (get-table-definition [this]
    (-> this :diff :value))

  GetDdlTableNameAble
  (get-ddl-table-name [this]
    (-> this get-table-definition :name undasherize))
  
  PostgresDdlAddDropable
  (get-pg-ddl-add [this]
    (create-table (get-table-definition this)))
  (get-pg-ddl-drop [this]
    (str "DROP TABLE " (get-ddl-table-name this) ";"))

  PostgresDdlAble
  (get-pg-ddl [this forward-or-backward]
    (get-pg-ddl-from-pg-add-dropable this forward-or-backward)))

(defprotocol DdlFieldNameable
  (get-ddl-field-name [this]))

(defprotocol GetFieldDefinitionAble
  (get-field-def [this]))

(defrecord FieldAddRemove [diff]
  GetTableNameKeywordAble
  (get-table-name-kw [this]
    (-> this :diff :path first))
  
  Groupable
  (get-group [this]
    [:alter-table (get-table-name-kw this)])

  GetFieldDefinitionAble
  (get-field-def [this]
    (-> this :diff :value))

  DdlFieldNameable
  (get-ddl-field-name [this]
    (get-field-name (get-field-def this)))
  
  PostgresDdlAddDropable
  (get-pg-ddl-add [this]
    (str "ADD COLUMN "
         (get-ddl-field-name this) " "
         (field-type-to-ddl (get-field-def this))))
  (get-pg-ddl-drop [this]
    (str "DROP COLUMN " (get-ddl-field-name this)))

  PostgresDdlAble
  (get-pg-ddl [this forward-or-backward]
    (get-pg-ddl-from-pg-add-dropable this forward-or-backward)))

(defprotocol ConstraintNameable
  (get-constraint-name [this]))

(defrecord ConstraintUniqueAddRemove [diff]
  GetTableNameKeywordAble
  (get-table-name-kw [this]
    (-> this :diff :path first))
  
  Groupable
  (get-group [this]
    [:alter-table (get-table-name-kw this)])

  DdlFieldNameable
  (get-ddl-field-name [this]
    (-> this :diff :path (get 2) name undasherize))

  ConstraintNameable
  (get-constraint-name [this]
    (let [ddl-table-name (-> this get-table-name-kw name undasherize)]
      (str ddl-table-name "_" (get-ddl-field-name this) "_unique")))

  PostgresDdlAddDropable
  (get-pg-ddl-add [this]
    (str "ADD CONSTRAINT " (get-constraint-name this)
         " UNIQUE (" (get-ddl-field-name this) ")"))
  (get-pg-ddl-drop [this]
    (str "DROP CONSTRAINT " (get-constraint-name this)))

  PostgresDdlAble
  (get-pg-ddl [this forward-or-backward]
    (get-pg-ddl-from-pg-add-dropable this forward-or-backward)))

(defn make-change-record [diff]
  (let [diff-path (:path diff)]
   (cond
     (= 1 (count diff-path))
     (->TableAddRemove diff)
     (and (= 3 (count diff-path))
          (= :fields (get diff-path 1)))
     (->FieldAddRemove diff)
     (and (= 4 (count diff-path))
          (= :unique (get diff-path 3))
          (:value diff))
     (->ConstraintUniqueAddRemove diff)
     :else
     (throw (Exception. "this case has not been implemented - make-addition-ddl - size of path")))))

(defrecord AlterTable [table-name-kw change-records]
  GetDdlTableNameAble
  (get-ddl-table-name [this]
    (-> table-name-kw name undasherize))
  
  PostgresDdlAble
  (get-pg-ddl [this forward-or-backward]
    (let [field-ddls (->> change-records
                          (map #(get-pg-ddl % forward-or-backward))
                          (map #(str "    " %))
                          (interpose ",\n"))
          ddl-vec (concat ["ALTER TABLE " (get-ddl-table-name this) "\n"]
                          field-ddls
                          [";"])
          result (apply str ddl-vec)]
      result)))

(defn make-grouped-change-record [[grouping change-records]]
  (let [group-category (first grouping)]
   (cond
     (= group-category :alter-table)
     (->AlterTable (second grouping) change-records)
     :default
     (throw (RuntimeException. (str "don't know how to make a change record for group category = " group-category))))))


(defn add-rems-to-ddl [diff-in]
  (let [missing-in-1 (:keys-missing-in-1 diff-in)
        missing-in-2 (:keys-missing-in-2 diff-in)
        missing-in-1 (map #(assoc % :missing-in :one) missing-in-1)
        missing-in-2 (map #(assoc % :missing-in :two) missing-in-2)
        all-add-rems (concat missing-in-1 missing-in-2)
        as-change-records (map #(make-change-record %) all-add-rems)
        grouped-by (group-by get-group as-change-records)
        top-level-changes (:top-level grouped-by)
        top-level-ddl (map #(get-pg-ddl % :forward) top-level-changes)
        remaining-grouped (dissoc grouped-by :top-level)
        grouped-change-records (map make-grouped-change-record remaining-grouped)
        grouped-change-ddls (map #(get-pg-ddl % :forward) grouped-change-records)]
    (concat top-level-ddl grouped-change-ddls)))


(defn diff-to-ddl [diff-in]
  (let [val-diff-ddl (map value-difference-to-ddl (:value-differences diff-in))
        diffs-xformed (add-rems-to-ddl diff-in)
        result-ddl-seq (concat val-diff-ddl diffs-xformed)]
    (as-> result-ddl-seq $
      (interpose "\n" $)
      (apply str $))))

(defn make-migration [entities-old-diff-list entities-new]
  (let [entities-patched (reduce #(dal/patch %1 %2) nil entities-old-diff-list)
        entities-for-diff (dddl-cljc/xform-entities-for-diff entities-new)]
    (dal/diffl entities-patched entities-for-diff)))


(def migrations-dir-name "migrations-dddl")
(def migrations-dir (clojure.java.io/file migrations-dir-name))

(defn get-all-migration-file-names []
  (fnames-in-dir migrations-dir))

;; (def ^:dynamic entities nil)
(spec/check-asserts true)

(defn make-migration-file! [entities-in]
  (let [entities-validated (spec/assert ::entities-schemas/entities entities-in)
        ensure-mig-dir-exists-res
        (when-not (.exists migrations-dir)
          (.mkdir migrations-dir)
          (when-not (.exists migrations-dir)
            (throw (Exception. "should exist"))))
        existing-mig-fpaths (map #(str migrations-dir-name "/" %) (get-all-migration-file-names))
        diffs (map edn-read existing-mig-fpaths)
        next-diff (make-migration diffs entities-validated)]
    (if (and (empty? (:keys-missing-in-1 next-diff))
             (empty? (:keys-missing-in-2 next-diff))
             (empty? (:value-differences next-diff)))
      (throw (Exception. "no changes in entities"))
      (let [mig-num (-> (count existing-mig-fpaths) inc)
            mig-time-str (as-> (time/zoned-date-time) $
                           (.withZoneSameInstant $ (ZoneId/of "UTC"))
                           (time/format "yyyy-MM-dd'T'HH-mm-ss-SSSX" $))
            mig-fname (format "%04d-migration-%s.edn" mig-num mig-time-str)
            file-out-path (str migrations-dir-name "/" mig-fname)]
        (edn-write next-diff file-out-path)))))

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


(defn migrate! [db-url & {:keys [dry-run]
                          :or {dry-run false}}]
  (let [db-conn (conman/connect! {:jdbc-url db-url})
        all-mig-file-names (get-all-migration-file-names)
        ;; _ (println "all-mig-file-names:" all-mig-file-names)
        ]
    (if (empty? all-mig-file-names)
      ;; TODO - better logging
      (println "nothing to do here.  No migration files")
      (try
        (jdbc/with-db-transaction [t-conn db-conn]
          (let [last-mig-num (get-last-migration-number t-conn)
                ;; _ (println "last-mig-num:" last-mig-num)
                ;; db-says-initial? (nil? last-mig-num)
                ;; files-say-initial?
                ;; (let [mig-num-from-last-file
                ;;       (get-migration-number-from-filename (last all-mig-file-names))
                ;;       ;; _ (println "mig-num-from-last-file: " mig-num-from-last-file)
                ;;       ]
                ;;   (= mig-num-from-last-file 1))
                ;; both-say-initial? (= db-says-initial? files-say-initial?)
                ]
            ;; if (not both-say-initial?)
            ;; (throw (Exception. (str "conflicting results on whether this is the initial migration or not. db-says-initial? = " db-says-initial? ", files-say-initial? = " files-say-initial?)))
            (let [remaining-mig-file-names (if (nil? last-mig-num)
                                             all-mig-file-names
                                             (filter #(> (get-migration-number-from-filename %) last-mig-num) all-mig-file-names))
                  _ (println "remaining-mig-file-names:" remaining-mig-file-names)
                  init-result (when (nil? last-mig-num)
                                (jdbc/execute! t-conn [migration-table-ddl]))]
              (doseq [remaining-mig-file-name remaining-mig-file-names]
                (let [mig-file-path (str migrations-dir-name "/" remaining-mig-file-name)
                      mig-number (get-migration-number-from-filename remaining-mig-file-name)
                      mig-diff (edn-read mig-file-path)
                      the-ddl (diff-to-ddl mig-diff)
                      _ (println (str "the-ddl:\n" the-ddl))]
                  (if dry-run
                    (println "dry run only; DDL not exectuted.")
                    (let [ddl-exec-res (jdbc/execute! t-conn the-ddl)
                          migrations-table-append-res (jdbc/insert! t-conn :migrations {:number mig-number})])))))))
        (finally
          (conman/disconnect! db-conn))))))
