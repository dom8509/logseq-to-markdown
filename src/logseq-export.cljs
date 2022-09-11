(ns logseq-export
  "Script that clones and queries any two graphs given their git urls. Takes an
  optional third arg which is the datascript query to run. The first time a new
  git repo is given, the script clones it and creates a transit cache db for it.
  Subsequent script invocations read from the transit cache"
  (:require ["fs" :as fs]
            ["path" :as path]
            ["os" :as os]
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
  (fs/readFileSync file))

(defn exists?
  [file]
  (fs/existsSync file))

(defn directory?
  [file]
  (.isDirectory (fs/lstatSync file)))

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
    (str year "-" month "-" day))
  )

(defn- store-page
  [page-blocks]
  (println page-blocks))

(defn- parse-meta-data
  [graph-db, page]
  (let [title (get page :block/original-name)
        excluded-properties {:public :filters}
        ;; properties (reduce #(contains ) {} (get page :block/properties))
        properties (get page :block/properties)
        tags (get properties :tags)
        categories (get properties :categories)
        created-at (hugo-date (get page :block/created-at))
        updated-at (hugo-date (get page :block/updated-at))]
    (println title)
    (println properties)
    (println excluded-properties)
    (println created-at)
    ;; Todo: Delete public & filters properties
    ;; Convert to yml
  ;;     let ret = `---`;
  ;; for (let [prop, value] of Object.entries(propList)) {
  ;;   if (Array.isArray(value)) {
  ;;     ret += `\n${prop}:`;
  ;;     value.forEach((element) => (ret += `\n- ${element}`));
  ;;   } else {
  ;;     ret += `\n${prop}: ${value}`;
  ;;   }
  ;; }
  ;; ret += "\n---";
  ;; return ret;
    ))

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
             (store-page page-data))))))))

(when (= nbb/*file* (:file (meta #'-main)))
  (-main *command-line-args*))