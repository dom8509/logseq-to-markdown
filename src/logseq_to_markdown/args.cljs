(ns logseq-to-markdown.args
  (:require [clojure.tools.cli :refer [parse-opts]]
            [clojure.string :as s]))

(def cli-options
  [["-o" "--outputdir" "Output Directory"
    :default "./out"
    :required "PATH"]
   ["-e" "--excluded-properties" "Comma separated list of properties that should be ignored"
    :multi true
    :default #{:filters :public}
    :required "PROPERTY_LIST"
    :parse-fn #(set (map (fn [x] (keyword (s/trim x))) (s/split % ",")))
    :update-fn concat]
   ["-n" "--trim-namespaces" "Trim Namespace Names"
    :default false]
   ["-b" "--keep-bullets" "Keep Outliner Bullets"
    :default false]
   ["-t" "--export-tasks" "Export Logseq Tasks"
    :default false]
   ["-a" "--export-all" "Export all Logseq Pages"
    :default false]
   ["-r" "--rm-brackets" "Remove Link Brackets"
    :default false]
   ["-p" "--prerender-diagrams" "Prerender Diagrams and export images"
    :default false]
   ["-d" "--delete-outputdir" "Delete output directory before exporting data"
    :default false]
   [nil "--time-pattern" "Template Pattern for Time Strings"
    :default "yyyy-MM-dd"
    :required "PATTERN"]
   ["-v" "--verbose" "Verbose Output"
    :default false]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Export your local Logseq Graph to (Hugo) Markdown files."
        ""
        "Usage: logseq-to-markdown [options] graph"
        ""
        "Options:"
        options-summary
        ""
        "Graph: Name of the Logseq Graph"
        ""
        "Please refer to the manual page for more information."]
       (s/join \newline)))

(defn error-msg
  [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (s/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with an error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      ;; custom validation on arguments
      (>= (count arguments) 1)
      {:graph-name (first arguments) :options options}
      :else ; failed custom validation => exit with usage summary
      {:exit-message (usage summary)})))