(ns logseq-to-markdown.utils
  (:require [cljs-time.coerce :as tc]
            [cljs-time.core :as t]))

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