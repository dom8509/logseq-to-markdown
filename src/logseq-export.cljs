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
  (fs/readFileSync file))

(defn exists?
  [file]
  (fs/existsSync file))

(defn directory?
  [file]
  (.isDirectory (fs/lstatSync file)))

;; config file
(def config-file "./config.edn")

(def exporter-config
  (when (exists? config-file)
    (println (str "Loading config file " config-file " ..."))
    (edn/read-string (.readFileSync fs config-file "utf8"))))

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

(defn- store-page
  [page-blocks]
  ;; (println page-blocks)
  )

(defn- convert-filename
  [filename]
  (let [replace-pattern #"([\u2700-\u27BF]|[\uE000-\uF8FF]|\uD83C[\uDC00-\uDFFF]|\uD83D[\uDC00-\uDFFF]|[\u2011-\u26FF]|\uD83E[\uDD10-\uDDFF])"]
    (s/replace filename replace-pattern "")))

(defn- parse-property-value
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

(defn- parse-property
  [property]
  (str (s/replace (str (key property)) ":" "") ": " (parse-property-value (val property))))

(defn- parse-meta-data
  [graph-db, page]
  (let [original-name (get page :block/original-name)
        namespace? (and (true? (get exporter-config :trim-namespaces)) (s/includes? original-name "/"))
        title (or (and namespace? (last (s/split original-name "/"))) original-name)
        namespace (when namespace? (let [tokens (s/split original-name "/")]
                                     (s/join "/" (subvec tokens 0 (- (count tokens) 1)))))
        file (convert-filename title)
        excluded-properties (get exporter-config :excluded-properties)
        properties (into {} (filter #(not (contains? excluded-properties (first %))) (get page :block/properties)))
        properties? (> (count properties) 0)
        tags (get properties :tags)
        categories (get properties :categories)
        created-at (hugo-date (get page :block/created-at))
        updated-at (hugo-date (get page :block/updated-at))
        property-data-lines (into [] (map #(parse-property %) properties))
        property-data-delimiter (str "---")
        property-data (and properties? (s/join "\n" (cons property-data-delimiter (conj property-data-lines, property-data-delimiter))))]
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
    (println (str "Updated at: " updated-at))
    (str property-data)))

(defn- parse-page
  [graph-db, page]
  (let [query '[:find (pull ?b [*])
                :in $ ?page-id
                :where
                [?b :block/left ?page-id]]
        first-block (d/q query graph-db (get page :db/id))]
    ;; (println first-block)
    ))

(defn- parse-page-blocks
  [graph-db, page]
  (str
   (parse-meta-data graph-db page)
   (parse-page graph-db page)))

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
      (let [public-pages (map #(get % 0) (get-all-public-pages graph-db))]
        (dorun
         (for [public-page public-pages]
           (let [page-data (parse-page-blocks graph-db public-page)]
             (store-page page-data)
             (println "Page Data: \n" page-data))))))))

(when (= nbb/*file* (:file (meta #'-main)))
  (-main *command-line-args*))