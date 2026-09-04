(ns logseq-to-markdown.links
  (:require [clojure.string :as s]
            [logseq-to-markdown.config :as config]
            [logseq-to-markdown.fs :as fs]
            [logseq-to-markdown.graph :as graph]))

(defn- link-text
  [page-name]
  (if (and (config/entry :trim-namespaces)
           (s/includes? page-name "/"))
    (last (s/split page-name "/"))
    page-name))

(defn- render-page-link
  [prefix label page-name]
  (if (graph/page-exists? page-name)
    (str prefix "[" label "]({{< ref \"/pages/"
         (fs/->filename page-name) "\" >}})")
    (str prefix label)))

(defn parse-links
  [text]
  (let [desc-link-pattern #"\[(.*?)\]\(\[\[(.*?)\]\]\)"
        hashtag-page-pattern #"#\[\[(.*?)\]\]"
        page-link-pattern #"(?<!#)\[\[(.*?)\]\]"
        ;; A bare hashtag starts at the beginning of a line or after a
        ;; delimiter. This avoids treating URL fragments as Logseq tags.
        hashtag-pattern #"(^|[\s(\[{>\"'])#([^\s#\[\](){}.,;:!?]+)"]
    (-> text
        (s/replace desc-link-pattern
                   (fn [[_ label page-name]]
                     (render-page-link "" label page-name)))
        (s/replace hashtag-page-pattern
                   (fn [[_ page-name]]
                     (render-page-link "#" (link-text page-name) page-name)))
        (s/replace page-link-pattern
                   (fn [[_ page-name]]
                     (render-page-link "" (link-text page-name) page-name)))
        (s/replace hashtag-pattern
                   (fn [[_ delimiter page-name]]
                     (str delimiter
                          (render-page-link "#" (link-text page-name) page-name)))))))
