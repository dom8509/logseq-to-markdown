(ns logseq-to-markdown.utils
  (:require [cljs-time.coerce :as tc]
            [cljs-time.core :as t]
            [clojure.string :as s]))

(defn- format-date
  [date]
  (if (< date 10)
    (str "0" date)
    (str date)))

(defn ->hugo-date
  [timestamp]
  (let [date (tc/from-long timestamp)
        year (t/year date)
        month (format-date (t/month date))
        day (format-date (t/day date))]
    (str year "-" month "-" day)))

(defn trim-newlines
  "Trim newlines on the begining and end of the strim"
  [text]
  (s/reverse (s/trim-newline (s/reverse (s/trim-newline text)))))

(defn string-to-json
  [str]
  (s/replace (s/replace (s/replace (s/replace str "\n" "") " " "") "'" "\"") #"([A-Za-z0-9]+):" "\"$1\":"))