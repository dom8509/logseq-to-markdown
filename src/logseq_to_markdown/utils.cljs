(ns logseq-to-markdown.utils
  (:require [cljs-time.coerce :as tc]
            [cljs-time.format :as ft]
            [clojure.string :as s]))

(defn ->hugo-date
  [timestamp pattern]
  (let [date (tc/from-long timestamp)
        custom-formatter (ft/formatter pattern)]
    (ft/unparse custom-formatter date)))

(defn trim-newlines
  "Trim newlines on the begining and end of the strim"
  [text]
  (s/reverse (s/trim-newline (s/reverse (s/trim-newline text)))))

(defn string-to-json
  [str]
  (s/replace (s/replace (s/replace (s/replace str "\n" "") " " "") "'" "\"") #"([A-Za-z0-9]+):" "\"$1\":"))