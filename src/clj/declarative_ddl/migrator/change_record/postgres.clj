(ns declarative-ddl.migrator.change-record.postgres
  (:require [clojure.spec.alpha :as spec]
            [camel-snake-kebab.core :as csk]
            [declarative-ddl.entities.core :as entities]
            [declarative-ddl.clj-utils.core :as clj-utils]
            [declarative-ddl.migrator.change-record.core :as change-record]))



(defn field-type-to-ddl [field-in]
  (let [type-specific-ddl (case (:type field-in)
                            :character
                            (str "VARCHAR(" (:max-length field-in) ")")
                            :foreign-key
                            (str "INTEGER REFERENCES " (-> (:references field-in) name csk/->snake_case))
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


(defn create-table-field [field-in]
  (let [field-validated (spec/assert ::entities/field field-in)
        field-ddl-name (entities/get-field-ddl-name field-validated)]
    (str field-ddl-name " " (field-type-to-ddl field-validated))))

(defn create-table [table-in]
  (let [fields-sql (as-> (:fields table-in) $
                    (map clj-utils/remove-key-from-kv-pair $)
                    (map #(str "    " (create-table-field %)) $)
                    (interpose ",\n" $))

        sql-vec (concat ["CREATE TABLE " (-> table-in :name csk/->snake_case)
                         " (\n"]
                        ["    id SERIAL PRIMARY KEY,\n"]
                        fields-sql
                        ["\n);"])]
      (apply str sql-vec)))


(defrecord PostgresMultiTableAddDrop [diff]
  change-record/DdlAddDropGenable
  (get-ddl-add [this]
    (let [sql-seq (map #(create-table %) (change-record/get-table-definitions this))]
      (apply str sql-seq)))
  (get-ddl-drop [this]
    (let [sql-seq (map #(str "DROP TABLE " %) (change-record/get-table-ddl-names this))]
      (apply str sql-seq))))

(derive PostgresMultiTableAddDrop ::change-record/MultiTableAddDrop)



(defrecord PostgresTableAddDrop [diff]
  change-record/DdlAddDropGenable
  (get-ddl-add [this]
    (create-table (change-record/get-table-definition this)))
  (get-ddl-drop [this]
    (str "DROP TABLE " (change-record/get-table-ddl-name this) ";")))

(derive PostgresTableAddDrop ::change-record/TableAddDrop)



(defrecord PostgresFieldAddDrop [diff]
  change-record/DdlAddDropGenable
  (get-ddl-add [this]
    (str "ADD COLUMN "
         (change-record/get-field-ddl-name this) " "
         (field-type-to-ddl (change-record/get-field-definition this))))
  (get-ddl-drop [this]
    (str "DROP COLUMN " (change-record/get-field-ddl-name this))))

(derive PostgresFieldAddDrop ::change-record/FieldAddDrop)



(defrecord PostgresUniqueConstraintAddDrop [diff]
  change-record/DdlAddDropGenable
  (get-ddl-add [this]
    (str "ADD CONSTRAINT " (change-record/get-unique-constraint-ddl-name this)
         " UNIQUE (" (change-record/get-field-ddl-name this) ")"))
  (get-ddl-drop [this]
    (str "DROP CONSTRAINT " (change-record/get-unique-constraint-ddl-name this))))

(derive PostgresUniqueConstraintAddDrop
        ::change-record/UniqueConstraintAddDrop)


(defrecord PostgresNotNullConstraintAddDrop [diff]
  change-record/DdlAddDropGenable
  (get-ddl-add [this]
    (str "ALTER COLUMN " (change-record/get-field-ddl-name this) " SET NOT NULL"))
  (get-ddl-drop [this]
    (str "ALTER COLUMN " (change-record/get-field-ddl-name this) " DROP NOT NULL")))

(derive PostgresNotNullConstraintAddDrop ::change-record/NotNullConstraintAddDrop)




(defrecord PostgresAlterTable [table-name-kw change-records]
  change-record/OtherDdlGenable
  (get-ddl [this forward-or-backward]
    (let [field-ddls (->> change-records
                          (map #(change-record/get-ddl-from-change-rec
                                 %
                                 forward-or-backward))
                          (map #(str "    " %))
                          (interpose ",\n"))
          table-ddl-name (change-record/table-name-kw->ddl table-name-kw)
          ddl-vec (concat ["ALTER TABLE " table-ddl-name "\n"]
                          field-ddls [";"])
          result (apply str ddl-vec)]
      result)))

(derive PostgresAlterTable ::change-record/AlterTable)
