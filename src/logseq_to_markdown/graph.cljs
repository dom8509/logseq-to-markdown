(ns logseq-to-markdown.graph
  (:require [logseq-to-markdown.fs :as fs]
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
  ;; if data path is unknown get path 
  ;; from current block and store as atom
  [graph-db public-pages]
  (when (and (empty? @logseq-data-path) (not-empty public-pages))
    (let [block (first public-pages)
          block-id (get block :db/id)
          query '[:find ?n
                  :in $ ?block-id
                  :where
                  [?block-id :block/file ?f]
                  [?f :file/path ?n]]
          query-res (d/q query graph-db block-id)
          logseq-filename (first (map #(get % 0) query-res))
          logseq-filename-tokens (s/split logseq-filename "/")
          logseq-file-path (s/join "/" (subvec logseq-filename-tokens 0 (- (count logseq-filename-tokens) 2)))]
      (reset! logseq-data-path logseq-file-path))))

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

(defn get-all-public-pages
  [graph-db]
  (let [query '[:find (pull ?p [*])
                :where
                [?p :block/properties ?pr]
                [(get ?pr :public) ?t]
                [(= true ?t)]
                [?p :block/name ?n]]]
    (d/q query graph-db)))

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