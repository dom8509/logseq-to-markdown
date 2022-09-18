(ns logseq-to-markdown.config
  (:refer-clojure :exclude [set]))

(def exporter-config-data (atom {}))

(defn set
  [config-data]
  (reset! exporter-config-data config-data))

(defn entry
  [key]
  (get @exporter-config-data key))
    