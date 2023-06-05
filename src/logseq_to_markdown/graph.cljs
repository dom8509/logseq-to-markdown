(ns logseq-to-markdown.graph
  (:require [logseq-to-markdown.fs :as fs]
            [logseq-to-markdown.config :as config]
            [clojure.string :as s]
            [datascript.transit :as dt]
            [datascript.core :as d]))

;; graph utils
(def logseq-data-path (atom {}))

(defn get-graph-paths
  []
  (let [dir (fs/expand-home ".logseq/graphs")]
    (->> (fs/entries dir)
         (filter #(re-find #".transit$" %))
         (map #(str dir "/" %)))))

(defn full-path->graph
  [path]
  (second (re-find #"\+\+([^\+]+).transit$" path)))

(defn get-graph-path
  [graph]
  (some #(when (= graph (full-path->graph %)) %)
        (get-graph-paths)))

(defn determine-logseq-data-path
  [graph]
  (let [logseq-graph-file (get-graph-path graph)
        logseq-graph-path (s/replace (s/replace (s/replace logseq-graph-file #"^.*logseq_local_" "") #".transit$" "") "++" "/")]
    (reset! logseq-data-path logseq-graph-path)))

(defn get-logseq-data-path
  []
  @logseq-data-path)

(def graph-db (atom {}))

(defn load-graph-db
  [graph]
  (when-let [file (or (get-graph-path graph)
                      ;; graph is a path
                      graph)]
    (when (fs/exists? file)
      (println "Loading graph.")
      (reset! graph-db (-> file fs/slurp dt/read-transit-str))))
  @graph-db)

(defn get-graph-db
  []
  @graph-db)

(defn page-exists?
  [link]
  (let [query '[:find ?p
                :in $ ?link
                :where
                [?p :block/original-name ?link]
                [?p :block/properties ?pr]
                [(get ?pr :public) ?t]
                [(= true ?t)]]
        query-res (d/q query (get-graph-db) link)]
    (not-empty query-res)))

(defn get-ref-block
  [block-ref-id]
  (let [query '[:find ?c
                :in $ ?block-ref-id
                :where
                [?p :block/properties ?pr]
                [(get ?pr :id) ?t]
                [(= ?block-ref-id ?t)]
                [?p :block/content ?c]]
        query-res (d/q query (get-graph-db) block-ref-id)]
    (if (> (count query-res) 0)
      (nth (map #(get % 0) query-res) 0)
      ())))

(defn get-namespace-pages
  [namespace]
  (let [query '[:find ?n
                :in $ ?namespace
                :where
                [?p :block/original-name ?namespace]
                [?c :block/namespace ?p]
                [?c :block/original-name ?n]
                [?c :block/properties ?pr]
                [(get ?pr :public) ?t]
                [(= true ?t)]]
        query-res (d/q query (get-graph-db) namespace)]
    (if (> (count query-res) 0)
      (map #(get % 0) query-res)
      ())))

(defn get-all-public-and-private-pages
  [graph-db]
  (let [query '[:find (pull ?p [*])
                :where
                [?p :block/created-at]
                [?p :block/updated-at]]]
    (d/q query graph-db)))

(defn get-all-public-pages
  [graph-db]
  (let [query '[:find (pull ?p [*])
                :where
                [?p :block/properties ?pr]
                [(get ?pr :public) ?t]
                [(= true ?t)]
                [?p :block/created-at]
                [?p :block/updated-at]]]
    (d/q query graph-db)))

(defn get-all-pages
  [graph-db]
  (if (config/entry :export-all)
    (get-all-public-and-private-pages graph-db)
    (get-all-public-pages graph-db)))

(declare get-block-tree)

(defn get-child-blocks
  [graph-db parent-block-id level]
  (let [query '[:find (pull ?b [*])
                :in $ ?parent-block-id
                :where
                [?b :block/left ?parent-block-id]
                [?b :block/parent ?parent-block-id]]
        query-res (d/q query graph-db parent-block-id)]
    (when (> (count query-res) 0)
      (let [data (nth (map #(get % 0) query-res) 0)
            current-block-id (get data :db/id)]
        (cons {:level level
               :data data
               :children (get-child-blocks graph-db current-block-id (+ level 1))}
              (get-block-tree graph-db parent-block-id current-block-id level))))))

(defn get-block-tree
  [graph-db parent-block-id prev-block-id level]
  (let [query '[:find (pull ?b [*])
                :in $ ?prev-block-id ?parent-block-id
                :where
                [?b :block/left ?prev-block-id]
                [?b :block/parent ?parent-block-id]]
        query-res (d/q query graph-db prev-block-id parent-block-id)]
    (when (> (count query-res) 0)
      (let [data (nth (map #(get % 0) query-res) 0)
            current-block-id (get data :db/id)]
        (cons {:level level
               :data data
               :children (get-child-blocks graph-db current-block-id (+ level 1))}
              (get-block-tree graph-db parent-block-id current-block-id level))))))