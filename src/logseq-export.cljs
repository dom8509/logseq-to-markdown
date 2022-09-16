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

(def exporter-config-data (atom {}))

(def exporter-config
  (when (empty? @exporter-config-data)
    (if (exists? config-file)
      (let [data (edn/read-string (slurp config-file))]
        (reset! exporter-config-data data))
      (println (str "Config file " config-file " not found.")))))

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

(def logseq-data-path (atom {}))

(defn determine-logset-data-path
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

(def get-logseq-data-path @logseq-data-path)

(defn- copy-asset
  [link]
  (let [src-path (str @logseq-data-path link)
        dst-path (str (get exporter-config :output-dir) link)]
    (fs/copyFileSync src-path dst-path)))

(defn get-graph-path
  [graph]
  (some #(when (= graph (full-path->graph %)) %)
        (get-graph-paths)))

(def graph-db (atom {:loaded false}))

(defn- get-graph-db
  [graph]
  (when (not (get @graph-db :loaded))
    (when-let [file (or (get-graph-path graph)
                      ;; graph is a path
                        graph)]
      (when (exists? file)
        (println "Loading graph.")
        (reset! graph-db {:loaded true :db (-> file slurp dt/read-transit-str)}))))
  (get @graph-db :db))

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
                       (str "\n- " property-value))
                      (and
                       object-value?
                       (str "\n- " (first property-value)))
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

(defn- parse-block-refs
  [text]
  (let [pattern #"\(\(([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\)\)"
        block-ref-text (re-find pattern text)
        alias-pattern #"\[([^\[]*?)\]\([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\)"
        alias-text (re-find alias-pattern text)]
    (if (empty? block-ref-text)
      (if (empty? alias-text)
        (str text)
        (str (last alias-text)))
      (let [block-ref-id (last block-ref-text)
            query '[:find ?c
                    :in $ ?block-ref-id
                    :where
                    [?p :block/properties ?pr]
                    [(get ?pr :id) ?t]
                    [(= ?block-ref-id ?t)]
                    [?p :block/content ?c]]
            query-res (d/q query (get-graph-db) block-ref-id)]
        (if (> (count query-res) 0)
          (let [data (nth (map #(get % 0) query-res) 0)
                id-pattern (re-pattern (str "id:: " block-ref-id))]
            (s/replace data id-pattern ""))
          (str text))))))

(defn- parse-image
  [text]
  (let [pattern #"!\[.*?\]\((.*?)\)"
        image-text (re-find pattern text)]
    (if (empty? image-text)
      (str text)
      (let [link (nth image-text 1)
            converted-link (s/replace link #"\.\.\/" "/")
            converted-text (s/replace text #"\.\.\/" "/")]
        (if (not (or (s/includes? link "http") (s/includes? link "pdf")))
          (do
            (copy-asset converted-link)
            (str converted-text))
          (str text))))))

(def diagram-code (atom {:header-found false :type ""}))

(defn- parse-diagram-as-code
  [text]
  (let [header-pattern #"{{renderer code_diagram,(.*?)}}"
        header-res (re-find header-pattern text)
        body-pattern #"(?s)```([a-z]*)\n(.*)```"
        body-res (re-find body-pattern text)]
    (if (empty? header-res)
      (if (empty? body-res)
        (do
          (reset! diagram-code {:header-found false :type ""})
          (str text))
        (do
          (str
           "{{<diagram name=\"code_diagram\" type=\"" (:type @diagram-code) "\">}}\n"
           (last body-res)
           "{{</diagram>}}")))
      (do
        (reset! diagram-code {:header-found true :type (last header-res)})
        (str "")))))

(defn- parse-excalidraw-diagram
  [text]
  (let [pattern #"\[\[draws/(.*?)\]\]"
        res (re-find pattern text)]
    (if (empty? res)
      (str text)
      (let [diagram-name (first (s/split (last res) "."))
            diagram-file (str @logseq-data-path "/draws/" (last res))
            diagram-content (slurp diagram-file)]
        (str "{{<diagram name=\"" diagram-name "\" type=\"excalidraw\">}}\n"
             diagram-content "\n"
             "{{</diagram>}}")))))

(defn- parse-links
  [text]
  (let [link-pattern #"\[\[(.*?)\]\]"
        link-res (re-find link-pattern text)
        desc-link-pattern #"\[([a-zA-Z ]*?)\]\(\[\[(.*?)\]\]\)"
        desc-link-res (re-find desc-link-pattern text)
        namespace-pattern #"\[\[([^\/]*\/).*\]\]"
        namespace-res (re-find namespace-pattern text)]
    (if (empty? desc-link-res)
      (if (empty? link-res)
        (str text)
        (let [namespace-link? (not-empty namespace-res)
              link-text (or (and namespace-link? (get exporter-config :trim-namespaces) (last (s/split (last link-res) "/"))) (last link-res))]
          (str "[[[" link-text "]]]" "({{< ref \"/pages/" (convert-filename (last link-res)) "\" >}})")))
      (str "[" (nth desc-link-res 1) "]" "({{< ref \"/pages/" (convert-filename (last desc-link-res)) "\" >}})"))))

;; TODO parse-namespaces
(defn- parse-namespaces
  [text]
  (str text))

;; TODO parse-embeds
(defn- parse-embeds
  [text]
  (str text))

(defn- parse-video
  [text]
  (let [pattern #"{{(?:video|youtube) (.*?)}}"
        res (re-find pattern text)]
    (if (empty? res)
      (str text)
      (let [title-pattern #"(youtu(?:.*\/v\/|.*v\=|\.be\/))([A-Za-z0-9_\-]{11})"
            title (re-find title-pattern text)]
        (if (empty? title)
          (str text)
          (str "{{< youtube " (last title) " >}}"))))))

;; TODO parse-markers
(defn- parse-markers
  [text]
  (str text))

(defn- parse-highlights
  [text]
  (let [pattern #"(==(.*?)==)"]
    (s/replace text pattern "{{< logseq/mark >}}$2{{< / logseq/mark >}}")))

(defn- parse-org-cmd
  [text]
  (let [pattern #"(?sm)#\+BEGIN_([A-Z]*)[^\n]*\n(.*)#\+END_[^\n]*"
        res (re-find pattern text)]
    (if (empty? res)
      (str text)
      (let [cmd (nth res 1)
            value (nth res 2)]
        (str "{{< logseq/org" cmd " >}}" value "{{< / logseq/org" cmd " >}}\n")))))

(defn- rm-logbook-data
  [text]
  (let [pattern #"(?s)(:LOGBOOK:.*:END:)"
        res (re-find pattern text)]
    (if (empty? res)
      (str text)
      (str ""))))

(defn- rm-page-properties
  [text]
  (let [pattern #"([A-Za-z0-9_\-]+::.*)"
        res (re-find pattern text)]
    (if (empty? res)
      (str text)
      (str (rm-page-properties (s/replace text (first res) ""))))))

(defn- rm-width-height
  [text]
  (let [pattern #"{:height\s*[0-9]*,\s*:width\s*[0-9]*}"]
    (s/replace text pattern "")))

(defn- rm-brackets
  [text]
  (let [pattern #"(?:\[\[|\]\])"
        res (re-find pattern text)]
    (if (or (empty? res) (false? (get exporter-config :rm-brackets)))
      (str text)
      (str (rm-brackets (s/replace text pattern ""))))))

;; Parse the text of the :block/content and convert it into markdown
(defn- parse-text
  [block]
  (let [current-block-data (get block :data)]
    (when (not (and (get current-block-data :block/pre-block?) (= (get block :level) 1)))
      (let [prefix (if (and (get exporter-config :keep-bullets) (not-empty (get current-block-data :block/content)))
                     (str (apply str (concat (repeat (* (- (get block :level) 1) 1) " "))) "+ ")
                     (str ""))
            block-content (get current-block-data :block/content)
            marker? (not (nil? (get current-block-data :block/marker)))]
        (when (or (not marker?) (true? (get exporter-config :export-tasks)))
          (let [res-line (s/trim-newline (->> (str block-content)
                                              (parse-block-refs)
                                              (parse-image)
                                              (parse-diagram-as-code)
                                              (parse-excalidraw-diagram)
                                              (parse-links)
                                              (parse-namespaces)
                                              (parse-embeds)
                                              (parse-video)
                                              (parse-markers)
                                              (parse-highlights)
                                              (parse-org-cmd)
                                              (rm-logbook-data)
                                              (rm-page-properties)
                                              (rm-width-height)
                                              (rm-brackets)
                                              (str prefix)))]
            (when (not= res-line "")
              (str res-line "\n\n"))))))))

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
        (determine-logset-data-path graph-db public-pages)
        (dorun
         (for [public-page public-pages]
           (let [page-data (parse-page-blocks graph-db public-page)]
             (store-page page-data))))))))

(when (= nbb/*file* (:file (meta #'-main)))
  (-main *command-line-args*))