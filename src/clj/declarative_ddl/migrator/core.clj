(ns declarative-ddl.migrator.core
  (:require [clojure.pprint :as pp]
            [clojure.java.jdbc :as jdbc]
            [clojure.spec.alpha :as spec]
            [java-time :as time]
            [conman.core :as conman]
            [diff-as-list.core :as dal]
            [declarative-ddl.cljc.core :as dddl-cljc]
            [declarative-ddl.entities.core :as entities]
            [declarative-ddl.clj-utils.core :as clj-utils]
            [declarative-ddl.cljc.utils.core :as cljc-utils]
            [declarative-ddl.migrator.change-record.postgres :as postgres]
            [declarative-ddl.migrator.change-record.core :as change-record]
            [declarative-ddl.migrator.change-record.factories :as change-rec-factories])
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

(defn value-difference-to-ddl [val-diff]
  (cond
    (empty? (:path val-diff))
    (cond
      (nil? (:val-1 val-diff))
      (as-> (:val-2 val-diff) $
        (map clj-utils/remove-key-from-kv-pair $)
        (map postgres/create-table $)
        (interpose "\n" $)
        (apply str $))
      :else
      (throw (Exception. "this case has not yet been implemented - value of val-1")))
    :else
    (throw (Exception. "this case has not yet been implemented - size of path"))))

(defn add-rems-to-ddl [diff-in]
  (let [missing-in-1 (:keys-missing-in-1 diff-in)
        missing-in-2 (:keys-missing-in-2 diff-in)
        missing-in-1 (map #(assoc % :missing-in :one) missing-in-1)
        missing-in-2 (map #(assoc % :missing-in :two) missing-in-2)
        all-add-rems (concat missing-in-1 missing-in-2)
        as-change-records (map
                           #(change-rec-factories/make-change-record % :postgres)
                           all-add-rems)
        grouped-by (group-by change-record/get-group as-change-records)
        top-level-changes (:top-level grouped-by)
        top-level-ddl (map #(change-record/get-ddl-from-change-rec % :forward) top-level-changes)
        remaining-grouped (dissoc grouped-by :top-level)
        grouped-change-records (map
                                #(change-rec-factories/make-grouped-change-record
                                  %
                                  :postgres)
                                remaining-grouped)
        grouped-change-ddls (map #(change-record/get-ddl % :forward) grouped-change-records)]
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
  (let [entities-validated (spec/assert ::entities/entities entities-in)
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
