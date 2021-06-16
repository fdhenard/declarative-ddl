(ns declarative-ddl.migrator.core
  (:require [clojure.edn :as edn]
            [clojure.pprint :as pp]
            [clojure.java.io :as io]
            [clojure.string :as string]
            [clojure.java.jdbc :as jdbc]
            [clojure.spec.alpha :as spec]
            [java-time :as time]
            [conman.core :as conman]
            [diff-as-list.core :as dal]
            [declarative-ddl.cljc.core :as dddl-cljc]
            [declarative-ddl.entities.core :as entities]
            [declarative-ddl.migrator.change-record.core :as change-record]
            [declarative-ddl.migrator.change-record.factories :as change-rec-factories])
  (:import [java.time ZoneId]))

(defn edn-write [obj fpath]
  (as-> obj $
    (with-out-str (pp/pprint $))
    (spit fpath $)))

(defn edn-read [fpath]
  (-> fpath
      io/reader
      java.io.PushbackReader.
      edn/read))

(defn fnames-in-dir [dir]
  (as-> dir $
    (file-seq $)
    (filter #(.isFile %) $)
    (map #(.getName %) $)
    (sort $)))


(defn diffs->ddl [diff-items]
  (let [as-change-recs
        (map #(change-rec-factories/make-change-record % :postgres) diff-items)
        grouped-by (group-by change-record/get-group as-change-recs)
        top-level-changes (:top-level grouped-by)
        top-level-ddl
        (map #(change-record/get-ddl-from-change-rec % :forward)
             top-level-changes)
        remaining-grouped (dissoc grouped-by :top-level)
        grouped-change-recs
        (map #(change-rec-factories/make-grouped-change-record
               %
               :postgres)
             remaining-grouped)
        grouped-change-ddls
        (map #(change-record/get-ddl-from-change-rec % :forward)
             grouped-change-recs)]
    (concat top-level-ddl grouped-change-ddls)))


(defn dal-out->ddl [diff-in]
  (let [diff-records (map dal/diff-map->record (:differences diff-in))
        result-ddl-seq (diffs->ddl diff-records)]
    (as-> result-ddl-seq $
      (interpose "\n" $)
      (apply str $))))

(defn make-migration [entities-old-diff-list entities-new]
  (let [#_ (clojure.pprint/pprint entities-old-diff-list)
        entities-patched (reduce #(dal/patch %1 %2) nil entities-old-diff-list)
        entities-for-diff (dddl-cljc/xform-entities-for-diff entities-new)]
    (dal/diffl entities-patched entities-for-diff)))


;; (def migrations-dir-name "migrations-dddl")
;; (def migrations-dir (clojure.java.io/file migrations-dir-name))

(defn get-all-migration-file-names [migrations-dir]
  (fnames-in-dir migrations-dir))

;; (def ^:dynamic entities nil)
(spec/check-asserts true)

(defn make-migration-file! [entities-in migrations-dir-path]
  (let [migrations-dir (clojure.java.io/file migrations-dir-path)
        entities-validated (spec/assert ::entities/entities entities-in)
        _ (when-not (.exists migrations-dir)
            (.mkdir migrations-dir)
            (when-not (.exists migrations-dir)
              (throw (ex-info "should exist" {}))))
        existing-mig-fpaths
        (map #(str migrations-dir-path "/" %)
             (get-all-migration-file-names migrations-dir))
        diffs (map edn-read existing-mig-fpaths)
        next-diff (make-migration diffs entities-validated)
        #_ (clojure.pprint/pprint next-diff)]
    (if (empty? (:differences next-diff))
      (println "no changes in entities")
      (let [mig-num (-> (count existing-mig-fpaths) inc)
            mig-time-str (as-> (time/zoned-date-time) $
                           (.withZoneSameInstant $ (ZoneId/of "UTC"))
                           (time/format "yyyy-MM-dd'T'HH-mm-ss-SSSX" $))
            mig-fname (format "%04d-migration-%s.edn" mig-num mig-time-str)
            file-out-path (str migrations-dir-path "/" mig-fname)]
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
  (when (does-migration-table-exist? db-conn)
    (-> (jdbc/query db-conn ["SELECT MAX(number) FROM migrations"])
        first
        :max)))

(defn get-migration-number-from-filename [migration-fname]
  (-> (subs migration-fname 0 4) Integer/parseInt))


(defn migrate! [db-url
                migrations-dir-path
                & {:keys [dry-run]
                   :or {dry-run false}}]
  (let [db-conn (conman/connect! {:jdbc-url db-url})
        migrations-dir (clojure.java.io/file migrations-dir-path)
        all-mig-file-names (get-all-migration-file-names migrations-dir)
        #_ (println "all-mig-file-names:" all-mig-file-names)]
    (if (empty? all-mig-file-names)
      ;; TODO - better logging
      (println "nothing to do here.  No migration files")
      (try
        (jdbc/with-db-transaction [t-conn db-conn]
          (let [last-mig-num (get-last-migration-number t-conn)
                remaining-mig-file-names (if (nil? last-mig-num)
                                           all-mig-file-names
                                           (filter #(> (get-migration-number-from-filename %) last-mig-num) all-mig-file-names))
                _ (println "remaining-mig-file-names:" remaining-mig-file-names)
                _ (when (and (not dry-run)
                             (not (does-migration-table-exist? db-conn)))
                    (jdbc/execute! t-conn [migration-table-ddl]))]
            (doseq [remaining-mig-file-name remaining-mig-file-names]
              (let [mig-file-path (str migrations-dir-path "/" remaining-mig-file-name)
                    mig-number (get-migration-number-from-filename remaining-mig-file-name)
                    mig-diff (edn-read mig-file-path)
                    the-ddl (dal-out->ddl mig-diff)
                    _ (println (str "the-ddl:\n" the-ddl))]
                (if dry-run
                  (println "dry run only; DDL not exectuted.")
                  (let [ddl-exec-res (jdbc/execute! t-conn the-ddl)
                        migrations-table-append-res (jdbc/insert! t-conn :migrations {:number mig-number})
                        _ (println "Migration executed (not a dry run)")
                        _ (pp/pprint {:execute-results
                                      {:ddl-exec-res ddl-exec-res
                                       :migrations-table_append-res migrations-table-append-res}})]))))))
        (finally
          (conman/disconnect! db-conn))))))
