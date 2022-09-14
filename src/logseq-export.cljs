(ns logseq-export
  "Script that clones and queries any two graphs given their git urls. Takes an
  optional third arg which is the datascript query to run. The first time a new
  git repo is given, the script clones it and creates a transit cache db for it.
  Subsequent script invocations read from the transit cache"
  (:require ["fs" :as fs]
            ["path" :as path]
            ["os" :as os]
            [clojure.edn :as edn]
            [clojure.string :as s]
            [datascript.transit :as dt]
            [datascript.core :as d]
            [nbb.core :as nbb]
            [cljs-time.coerce :as tc]
            [cljs-time.core :as t])
  (:refer-clojure :exclude [exists?]))

;; fs utils
(defn expand-home
  [path]
  (path/join (os/homedir) path))

(defn entries
  [dir]
  (fs/readdirSync dir))

(defn slurp
  [file]
  (println (str "Loading file " file))
  (fs/readFileSync file #js {:encoding "utf-8"}))

(defn exists?
  [file]
  (fs/existsSync file))

(defn directory?
  [file]
  (.isDirectory (fs/lstatSync file)))

;; config file
(def config-file "./config.edn")

(def exporter-config
  ;; TODO load file only once
  (when (exists? config-file)
    (edn/read-string (slurp config-file))))

(def verbose-mode?
  (get exporter-config :verbose))

;; graph utils
(defn get-graph-paths
  []
  (let [dir (expand-home ".logseq/graphs")]
    (->> (entries dir)
         (filter #(re-find #".transit$" %))
         (map #(str dir "/" %)))))

(defn full-path->graph
  [path]
  (second (re-find #"\+\+([^\+]+).transit$" path)))

(defn get-graph-path
  [graph]
  (some #(when (= graph (full-path->graph %)) %)
        (get-graph-paths)))

(defn- get-graph-db
  [graph]
  (when-let [file (or (get-graph-path graph)
                      ;; graph is a path
                      graph)]
    (when (exists? file)
      (-> file slurp dt/read-transit-str))))

(defn- format-date
  [date]
  (if (< date 10)
    (str "0" date)
    (str date)))

(defn- hugo-date
  [timestamp]
  (let [date (tc/from-long timestamp)
        year (t/year date)
        month (format-date (t/month date))
        day (format-date (t/day date))]
    (str year "-" month "-" day)))

(defn- setup-outdir
  []
  (let [output-dir (get exporter-config :output-dir)
        subfolders ["pages" "assets"]]
    (when (exists? output-dir)
      (fs/rmSync output-dir #js {:recursive true}))
    (fs/mkdirSync output-dir)
    (dorun
     (map #(fs/mkdirSync (str output-dir "/" %)) subfolders))))

(defn- store-page
  [page-data]
  (let [output-dir-base (get exporter-config :output-dir)
        output-dir (str output-dir-base "/pages/" (get page-data :namespace))
        full-file-name (str output-dir "/" (get page-data :filename))]
    (fs/mkdirSync output-dir #js {:recursive true})
    (fs/writeFileSync full-file-name (get page-data :data))))

(defn- convert-filename
  [filename]
  (let [replace-pattern #"([\u2700-\u27BF]|[\uE000-\uF8FF]|\uD83C[\uDC00-\uDFFF]|\uD83D[\uDC00-\uDFFF]|[\u2011-\u26FF]|\uD83E[\uDD10-\uDDFF])"]
    (s/replace filename replace-pattern "")))

(defn- parse-property-value-list
  [property-value]
  (let [string-value? (string? property-value)
        object-value? (try
                        (and (not string-value?) (> (count property-value) 0))
                        (catch :default _ false))
        iteratable-value? (and object-value? (> (count property-value) 1))
        value-lines (or
                     (and
                      iteratable-value?
                      (let [property-lines (map #(str "- " %) property-value)
                            property-data (s/join "\n" property-lines)]
                        (str "\n" property-data)))
                     (or
                      (and
                       string-value?
                       (str property-value))
                      (and
                       object-value?
                       (first property-value))
                      (str property-value)))]
    (str value-lines)))

(defn- parse-meta-data
  [page]
  (let [original-name (get page :block/original-name)
        trim-namespaces? (get exporter-config :trim-namespaces)
        namespace? (s/includes? original-name "/")
        namespace (let [tokens (s/split original-name "/")]
                                     (s/join "/" (subvec tokens 0 (- (count tokens) 1))))
        title (or (and trim-namespaces? namespace? (last (s/split original-name "/"))) original-name)
        file (str (convert-filename (or (and namespace? (last (s/split original-name "/"))) original-name)) ".md")
        excluded-properties (get exporter-config :excluded-properties)
        properties (into {} (filter #(not (contains? excluded-properties (first %))) (get page :block/properties)))
        tags (get properties :tags)
        categories (get properties :categories)
        created-at (hugo-date (get page :block/created-at))
        updated-at (hugo-date (get page :block/updated-at))
        page-data (s/join ""
                          ["---\n"
                           (str "title: " title "\n")
                           (when namespace? (str "namespace: " namespace "\n"))
                           (str "tags: " (parse-property-value-list tags) "\n")
                           (str "categories: " (parse-property-value-list categories) "\n")
                           (str "date: " created-at "\n")
                           (str "lastMod: " updated-at "\n")
                           "---\n"])]
    (when verbose-mode?
      (println "======================================")
      (println (str "Title: " title))
      (println (str "Namespace?: " namespace?))
      (println (str "Namespace: " namespace))
      (println (str "File: " file))
      (println (str "Excluded Properties: " excluded-properties))
      (println (str "Properties: " properties))
      (println (str "Tags: " tags))
      (println (str "Categories: " categories))
      (println (str "Created at: " created-at))
      (println (str "Updated at: " updated-at)))
    {:filename file
     :namespace namespace
     :data page-data}))

;; Parse the text of the :block/content and convert it into markdown
(defn- parse-text
  [block]
  (let [current-block-data (get block :data)]
    (when (not (and (get current-block-data :block/pre-block?) (= (get block :level) 1)))
      (let [prefix (if (and (get exporter-config :keep-bullets) (not-empty (get current-block-data :block/content)))
                     (str (apply str (concat (repeat (* (- (get block :level) 1) 1) " "))) "+ ")
                     (str ""))]
        (str prefix (get current-block-data :block/content) "\n")))))

;; Iterate over every block and parse the :block/content
(defn- parse-block-content
  [block-tree]
  (when (not-empty block-tree)
    (let [current-block (first block-tree)]
      (str (parse-text current-block)
           (parse-block-content (get current-block :children))
           (parse-block-content (rest block-tree))))))

(declare get-block-tree)

(defn- get-child-blocks
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

(defn- get-block-tree
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

(defn- parse-page-blocks
  [graph-db page]
  (let [meta-data (parse-meta-data page)
        first-block-id (get page :db/id)
        block-tree (get-block-tree graph-db first-block-id first-block-id 1)
        content-data (parse-block-content block-tree)
        page-data (str
                   (get meta-data :data)
                   content-data)]
    {:filename (get meta-data :filename)
     :namespace (get meta-data :namespace)
     :data page-data}))

(defn- get-all-public-pages
  [graph-db]
  (let [query '[:find (pull ?p [*])
                :where
                [?p :block/properties ?pr]
                [(get ?pr :public) ?t]
                [(= true ?t)]
                [?p :block/name ?n]]]
    (d/q query graph-db)))

(defn -main
  [args]
  (if-not (= 1 (count args))
    (println "Usage: $0 GRAPH")
    ;; Load the logseq graph
    (let [graph-name (get (vec args) 0)
          graph-db (or (get-graph-db graph-name)
                       (throw (ex-info "No graph found" {:graph graph-name})))]
      (println (str "Graph " graph-name " loaded successfully."))
      (setup-outdir)
      (let [public-pages (map #(get % 0) (get-all-public-pages graph-db))]
        (dorun
         (for [public-page public-pages]
           (let [page-data (parse-page-blocks graph-db public-page)]
             (store-page page-data))))))))

(when (= nbb/*file* (:file (meta #'-main)))
  (-main *command-line-args*))