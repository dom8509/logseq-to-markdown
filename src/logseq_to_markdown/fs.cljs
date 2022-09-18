#_{:clj-kondo/ignore [:namespace-name-mismatch]}
(ns logseq-to-markdown.fs
  (:require ["fs" :as fs]
            ["path" :as path]
            ["os" :as os]
            [clojure.string :as s]
            [logseq-to-markdown.config :as config])
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
  (fs/readFileSync file #js {:encoding "utf-8"}))

(defn exists?
  [file]
  (fs/existsSync file))

(defn copy-file
  [src dst]
  (fs/copyFileSync src dst))

(defn ->filename
  [filename]
  (let [replace-pattern #"([\u2700-\u27BF]|[\uE000-\uF8FF]|\uD83C[\uDC00-\uDFFF]|\uD83D[\uDC00-\uDFFF]|[\u2011-\u26FF]|\uD83E[\uDD10-\uDDFF])"]
    (s/replace filename replace-pattern "")))

(defn setup-outdir
  []
  (let [output-dir (config/entry :outputdir)
        subfolders ["pages" "assets"]]
    (when (exists? output-dir)
      (fs/rmSync output-dir #js {:recursive true}))
    (fs/mkdirSync output-dir)
    (dorun
     (map #(fs/mkdirSync (str output-dir "/" %)) subfolders))))

(defn store-page
  [page-data]
  (let [output-dir-base (config/entry :outputdir)
        output-dir (str output-dir-base "/pages/" (get page-data :namespace))
        full-file-name (str output-dir "/" (get page-data :filename))]
    (fs/mkdirSync output-dir #js {:recursive true})
    (fs/writeFileSync full-file-name (get page-data :data))))