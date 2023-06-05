(ns logseq-to-markdown.nbb
  "Script that exports your local Logseq graph to (Hugo) 
   Markdown files."
  (:require [nbb.core :as nbb]
            [logseq-to-markdown.args :as args]
            [logseq-to-markdown.fs :as fs]
            [logseq-to-markdown.config :as config]
            [logseq-to-markdown.graph :as graph]
            [logseq-to-markdown.parser :as parser]))

(defn exit
  [msg]
  (println msg))

(defn -main
  [args]
  (let [{:keys [graph-name options exit-message]} (args/validate-args args)]
    (println "Running logseq-to-markdown version 0.4.1")
    (println "args: " args)
    (if exit-message
      (exit exit-message)
      (let [graph-db (or (graph/load-graph-db graph-name)
                         (throw (ex-info "No graph found" {:graph graph-name})))]
        (println (str "Graph " graph-name " loaded successfully."))
        (config/set options)
        (fs/setup-outdir)
        (println (str "Exporting data to " (config/entry :outputdir) " ..."))
        (let [page-map (graph/get-all-pages graph-db)
              pages (map #(get % 0) page-map)]
          (graph/determine-logseq-data-path graph-name)
          (dorun
           (for [page pages]
             (let [page-data (parser/parse-page-blocks graph-db page)]
               (fs/store-page page-data)))))
        (println "finished!")))))

(when (= nbb/*file* (:file (meta #'-main)))
  (-main (js->clj (.slice js/process.argv 2))))