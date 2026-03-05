(ns logseq-to-markdown.parser
  (:require [clojure.string :as s]
            [logseq-to-markdown.config :as config]
            [logseq-to-markdown.fs :as fs]
            [logseq-to-markdown.utils :as utils]
            [logseq-to-markdown.graph :as graph]
            [logseq-to-markdown.renderer.echarts :as echarts]
            [logseq-to-markdown.renderer.kroki :as kroki]))

(defn extract-inline-tags
  "Extract all inline tags from text. Returns a vector of tag names.
   Matches patterns like #[[tag-name]] and #tagname"
  [text]
  (let [link-pattern #"(?:#\[\[(.*?)\]\])|(?<!#)#(\w+)"
        matches (re-seq link-pattern text)]
    (reduce (fn [tags match]
              (let [tag (or (second match) (nth match 2))]
                (if tag (conj tags tag) tags)))
            []
            matches)))

(defn parse-property-value-list
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

(defn parse-meta-data
  ([page]
   (parse-meta-data page []))
  ([page inline-tags]
   (let [original-name (or (get page :block/original-name) "")
         trim-namespaces? (config/entry :trim-namespaces)
         namespace? (s/includes? original-name "/")
         namespace (let [tokens (s/split original-name "/")]
                     (s/join "/" (subvec tokens 0 (- (count tokens) 1))))
         title (or (and trim-namespaces? namespace? (last (s/split original-name "/"))) original-name)
         base-name (fs/->filename (or (and namespace? (last (s/split original-name "/"))) original-name))
         ;; Avoid Hugo reserved filenames that have special meaning in content directories
         safe-name (if (#{"index" "_index"} base-name) (str base-name "_page") base-name)
         file (str safe-name ".md")
         excluded-properties (config/entry :excluded-properties)
         properties (into {} (filter #(not (contains? excluded-properties (first %))) (get page :block/properties)))
         tags (get properties :tags)
         categories (get properties :categories)
         ;; Merge inline tags with property tags, removing duplicates
         merged-tags (if (and (seq inline-tags) (not (nil? tags)))
                       (distinct (concat (if (vector? tags) tags [tags]) inline-tags))
                       (or (and (seq inline-tags) inline-tags) tags))
         created-at (utils/->hugo-date (get page :block/created-at) (config/entry :time-pattern))
         updated-at (utils/->hugo-date (get page :block/updated-at) (config/entry :time-pattern))
         page-data (s/join ""
                           ["---\n"
                            (str "title: \"" (s/replace title "\"" "\\\"") "\"\n")
                            (when namespace? (str "namespace: " namespace "\n"))
                            (str "tags: " (parse-property-value-list merged-tags) "\n")
                            (str "categories: " (parse-property-value-list categories) "\n")
                            (str "date: " created-at "\n")
                            (str "lastMod: " updated-at "\n")
                            "---\n"])]
     (when (config/entry :verbose)
       (println "======================================")
       (println (str "Title: " title))
       (println (str "Namespace?: " namespace?))
       (println (str "Namespace: " namespace))
       (println (str "File: " file))
       (println (str "Excluded Properties: " excluded-properties))
       (println (str "Properties: " properties))
       (println (str "Tags: " tags))
       (println (str "Inline Tags: " inline-tags))
       (println (str "Merged Tags: " merged-tags))
       (println (str "Categories: " categories))
       (println (str "Created at: " created-at))
       (println (str "Updated at: " updated-at)))
     {:filename file
      :namespace namespace
      :data page-data})))

(defn parse-block-refs
  [text]
  (let [pattern #"\(\(([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12})\)\)"
        block-ref-text (re-find pattern text)
        alias-pattern #"\[([^\[]*?)\]\([0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}\)"
        alias-text (re-find alias-pattern text)]
    (if (empty? block-ref-text)
      (if (empty? alias-text)
        text
        (str (last alias-text)))
      (let [block-ref-id (last block-ref-text)
            data (graph/get-ref-block block-ref-id)]
        (if (seq data)
          (let [id-pattern (re-pattern (str "id:: " block-ref-id))]
            (s/replace data id-pattern ""))
          text)))))

(defn parse-image
  [text]
  (let [pattern #"!\[.*?\]\((.*?)\)"
        image-text (re-find pattern text)]
    (if (empty? image-text)
      text
      (let [link (nth image-text 1)
            converted-link (s/replace link #"\.\.\/" "/")
            converted-text (s/replace text #"\.\.\/" "/")]
        (if (not (or (s/includes? link "http") (s/includes? link "pdf")))
          (do
            (fs/copy-file->try
             (str (graph/get-logseq-data-path) converted-link)
             (str (config/entry :outputdir) converted-link))
            (str converted-text))
          text)))))

(def diagram-code (atom {:header-found false :type ""}))
(def diagram-code-count (atom 0))

(defn parse-diagram-as-code
  [text]
  (let [header-pattern #"{{renderer code_diagram,(.*?)}}"
        header-res (re-find header-pattern text)
        body-pattern #"(?s)```([a-z]*)\n(.*)```"
        body-res (re-find body-pattern text)]
    (if (empty? header-res)
      (if (or (empty? body-res) (false? (:header-found @diagram-code)))
        (do
          (reset! diagram-code {:header-found false :type ""})
          text)
        (if (config/entry :prerender-diagrams)
          (let [diagram-file (str "code_diagram_" @diagram-code-count ".svg")]
            (kroki/render-image (last body-res) (:type @diagram-code) diagram-file)
            (swap! diagram-code-count inc)
            (str "{{< svg_image \"/assets/" diagram-file "\" >}}"))
          (let [res-str (str
                         "{{<kroki_diagram name=\"code_diagram_" @diagram-code-count "\" type=\"" (:type @diagram-code) "\">}}\n"
                         (last body-res)
                         "{{</kroki_diagram>}}")]
            (swap! diagram-code-count inc)
            res-str)))
      (do
        (reset! diagram-code {:header-found true :type (last header-res)})
        (str "")))))

(def echart-code (atom {:header-found false :width 0 :height 0}))
(def echart-code-count (atom 0))

(defn parse-echart
  [text]
  (let [header-pattern #"{{renderer :logseq-echarts,\s*(.*?)px,\s*(.*?)px}}"
        header-res (re-find header-pattern text)
        body-pattern #"(?s)```json\n(.*)```"
        body-res (re-find body-pattern text)]
    (if (empty? header-res)
      (if (or (empty? body-res) (false? (:header-found @echart-code)))
        (do
          (reset! echart-code {:header-found false :type ""})
          text)
        (if (config/entry :prerender-diagrams)
          (let [diagram-file (str "echart_diagram_" @echart-code-count ".png")]
            (echarts/render-image (last body-res) (:width @echart-code) (:height @echart-code) diagram-file)
            (swap! echart-code-count inc)
            (str "![" diagram-file "](/assets/" diagram-file ")"))
          (let [res-str (str
                         "{{<echart_diagram name=\"echart_diagram_" @echart-code-count "\">}}\n"
                         (last body-res)
                         "{{</echart_diagram>}}")]
            (swap! echart-code-count inc)
            res-str)))
      (do

        (reset! echart-code {:header-found true
                             :width (int (nth header-res (- (count header-res) 2)))
                             :height (int (last header-res))})
        (str "")))))

(defn parse-excalidraw-diagram
  [text]
  (let [pattern #"\[\[draws/(.*?)\]\]"
        res (re-find pattern text)]
    (if (empty? res)
      text
      (let [diagram-name (first (s/split (last res) "."))
            diagram-file (str (graph/get-logseq-data-path) "/draws/" (last res))
            diagram-content (fs/slurp diagram-file)]
        (if (config/entry :prerender-diagrams)
          (let [diagram-file (str diagram-name ".svg")]
            (kroki/render-image diagram-content "excalidraw" diagram-file)
            (str "{{< svg_image \"/assets/" diagram-file "\" >}}"))
          (str "{{<kroki_diagram name=\"" diagram-name "\" type=\"excalidraw\">}}\n"
               diagram-content "\n"
               "{{</kroki_diagram>}}"))))))

(defn parse-links
  [text]
    (let [link-pattern #"(?:#\[\[(.*?)\]\])|(?<!#)#(\w+)"
        link-res (re-seq link-pattern text)
        desc-link-pattern #"\[(.*?)\]\(\[\[(.*?)\]\]\)"
        desc-link-res (re-seq desc-link-pattern text)
        ;; Plain [[Page Name]] links (not preceded by #)
        plain-link-pattern #"(?<!#)\[\[(.*?)\]\]"
        plain-link-res (re-seq plain-link-pattern text)]
    (if (not (empty? desc-link-res))
      (reduce #(let [current-text (first %2)
                     current-link (last %2)
                     link-text (nth %2 1)
                     replaced-str (or (and (graph/page-exists? current-link) (str "[" link-text "]({{< ref \"/pages/" (fs/->filename current-link) "\" >}})"))
                                      (str link-text))]
                 (s/replace %1 current-text replaced-str))
              text
              desc-link-res)
      (let [;; First, convert #[[Page]] and #tag links
            text-after-hash-links
            (if (empty? link-res)
              text
              (reduce
               #(let [current-text (first %2)
                      current-link (or (second %2) (nth %2 2))
                      namespace-pattern #"\[\[([^\/]*\/).*\]\]"
                      namespace-res (re-find namespace-pattern text)
                      namespace-link? (not-empty namespace-res)
                      link-text (or (and namespace-link? (config/entry :trim-namespaces)
                                         (last (s/split current-link "/"))) current-link)
                      replaced-str (or (and (graph/page-exists? current-link) (str "#[" link-text "]({{< ref \"/pages/" (fs/->filename current-link) "\" >}})"))
                                       (str "#" link-text))]
                  (s/replace %1 current-text replaced-str))
               text
               link-res))]
        ;; Then, convert remaining plain [[Page]] links
        (if (empty? plain-link-res)
          text-after-hash-links
          (reduce
           #(let [current-text (first %2)
                  current-link (second %2)
                  replaced-str (or (and (graph/page-exists? current-link) (str "[" current-link "]({{< ref \"/pages/" (fs/->filename current-link) "\" >}})"))
                                   current-link)]
              (s/replace %1 current-text replaced-str))
           text-after-hash-links
           plain-link-res))))))

(defn parse-namespaces
  [level text]
  (let [pattern #"{{namespace\s([^}]+)}}"
        res (re-find pattern text)]
    (if (empty? res)
      text
      (let [namespace-name (last res)
            data (graph/get-namespace-pages namespace-name)]
        (if (seq data)
          (let [heading (str "***Namespace "
                             (if (graph/page-exists? namespace-name)
                               (str "[" namespace-name "]({{< ref \"/pages/" (fs/->filename namespace-name) "\" >}})***\n")
                               (str namespace-name "***\n")))
                prefix (if (config/entry :keep-bullets)
                         (str (apply str (concat (repeat (* level 1) "\t"))) "+ ")
                         (str (apply str (concat (repeat (* (- level 1) 1) "\t"))) "+ "))
                content (reduce #(str %1 prefix "[" %2 "]({{< ref \"/pages/" (fs/->filename %2) "\" >}})\n") "" data)]
            (str heading content))
          text)))))

;; TODO parse-embeds
(defn parse-embeds
  [text]
  text)

(defn parse-video
  [text]
  (let [pattern #"{{(?:video|youtube) (.*?)}}"
        res (re-find pattern text)]
    (if (empty? res)
      text
      (let [title-pattern #"(youtu(?:.*\/v\/|.*v\=|\.be\/))([A-Za-z0-9_\-]{11})"
            title (re-find title-pattern text)]
        (if (empty? title)
          text
          (str "{{< youtube " (last title) " >}}"))))))

;; TODO parse-markers
(defn parse-markers
  [text]
  text)

(defn parse-highlights
  [text]
  (let [pattern #"(==(.*?)==)"]
    (s/replace text pattern "{{< logseq/mark >}}$2{{< / logseq/mark >}}")))

(defn- parse-queries
  [text]
  (let [pattern #"query-table:: (.*)"
        res (re-find pattern text)]
    (if (empty? res)
      text
      (let [qery-text (utils/trim-newlines (s/replace text (nth res 0) ""))]
        (when (config/entry :verbose)
          (println "Query found:")
          (println qery-text))
        text))))

(defn parse-org-cmd
  [text]
  (let [pattern #"(?sm)#\+BEGIN_([A-Z]*)[^\n]*\n(.*)#\+END_[^\n]*"
        res (re-find pattern text)]
    (if (empty? res)
      text
      (let [cmd (nth res 1)
            value (nth res 2)]
        (str "{{< logseq/org" cmd " >}}" value "{{< / logseq/org" cmd " >}}\n")))))

(defn rm-logbook-data
  [text]
  (let [pattern #"(?s)(:LOGBOOK:.*:END:)"
        res (re-find pattern text)]
    (if (empty? res)
      text
      (str ""))))

(defn rm-page-properties
  [text]
  (let [pattern #"([A-Za-z0-9_\-]+::.*)"
        res (re-find pattern text)]
    (if (empty? res)
      text
      (str (rm-page-properties (s/replace text (first res) ""))))))

(defn rm-width-height
  [text]
  (let [pattern #"{:height\s*[0-9]*,\s*:width\s*[0-9]*}"]
    (s/replace text pattern "")))

(defn rm-brackets
  [text]
  (let [pattern #"(?:\[\[|\]\])"
        res (re-find pattern text)]
    (if (or (empty? res) (false? (config/entry :rm-brackets)))
      text
      (str (rm-brackets (s/replace text pattern ""))))))

;; Parse the text of the :block/content and convert it into markdown
(defn parse-text
  [block]
  (let [current-block-data (:data block)
        block-level (:level block)]
    (when (not (and (:block/pre-block? current-block-data) (= block-level 1)))
      (let [prefix (if (and (:keep-bullets config/entry)
                            (not-empty (:block/content current-block-data)))
                     (str (apply str (concat (repeat (* (- block-level 1) 1) "\t"))) "+ ")
                     (if (and (not= block-level 1)
                              (not-empty (:block/content current-block-data)))
                       (str (apply str (concat (repeat (* (- block-level 2) 1) "\t"))) "+ ")
                       (str "")))
            block-content (get current-block-data :block/content)
            marker? (not (nil? (get current-block-data :block/marker)))]
        (when (or (not marker?) (true? (config/entry :export-tasks)))
          (let [res-line (s/trim-newline (->> (str block-content)
                                              (parse-block-refs)
                                              (parse-image)
                                              (parse-diagram-as-code)
                                              (parse-echart)
                                              (parse-excalidraw-diagram)
                                              (parse-links)
                                              (parse-namespaces block-level)
                                              (parse-embeds)
                                              (parse-video)
                                              (parse-markers)
                                              (parse-highlights)
                                              (parse-queries)
                                              (parse-org-cmd)
                                              (rm-logbook-data)
                                              (rm-page-properties)
                                              (rm-width-height)
                                              (rm-brackets)
                                              (str prefix)))]
            (when (not= res-line "")
              (str res-line "\n\n"))))))))

;; Helper function to parse block content while collecting inline tags
(defn parse-block-content-with-tags
  [block-tree collected-tags]
  (if (empty? block-tree)
    ["" collected-tags]
    (let [current-block (first block-tree)
          block-content (get-in current-block [:data :block/content])
          parsed-text (parse-text current-block)
          ;; Extract inline tags from the block content
          block-tags (if block-content (extract-inline-tags block-content) [])
          updated-tags (concat collected-tags block-tags)
          [children-text children-tags] (parse-block-content-with-tags (get current-block :children) updated-tags)
          [rest-text rest-tags] (parse-block-content-with-tags (rest block-tree) children-tags)
          combined-text (str parsed-text children-text rest-text)]
      [combined-text rest-tags])))

;; Iterate over every block and parse the :block/content, collecting inline tags
(defn parse-block-content
  ([block-tree]
   (first (parse-block-content-with-tags block-tree [])))
  ([block-tree inline-tags]
   (first (parse-block-content-with-tags block-tree inline-tags))))

(defn parse-page-blocks
  [graph-db page]
  (let [first-block-id (get page :db/id)
        block-tree (graph/get-block-tree graph-db first-block-id first-block-id 1)
        ;; Parse content and collect inline tags
        [content-data inline-tags] (parse-block-content-with-tags block-tree [])
        ;; Generate metadata with inline tags merged in
        meta-data (parse-meta-data page inline-tags)
        page-data (str
                   (get meta-data :data)
                   content-data)]
    {:filename (get meta-data :filename)
     :namespace (get meta-data :namespace)
     :data page-data}))
