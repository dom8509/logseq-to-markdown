(ns logseq-to-markdown.renderer.kroki
  (:require ["node-fetch$default" :as fetch]
            ["pako" :as pako]
            [clojure.string :as s]
            [promesa.core :as p]
            [logseq-to-markdown.fs :as fs]))

(defn- download-image
  [url filename]
  (p/let [result (p/-> (fetch url #js {:method "GET" :redirect "follow" :responseType "arrayBuffer"})
                       (.blob)
                       (.text))]
    (fs/store-asset result filename)))

(defn- replace-chars
  [text]
  (s/replace (s/replace text "+" "-") "/" "_"))

(defn render-image
  [code type filename]
  (let [buffer (.from js/Buffer code "utf-8")
        compressed (.deflate pako buffer {:level 9})
        url (replace-chars (.toString (.from js/Buffer compressed) "base64"))]
    (download-image (str "http://kroki.io/" type "/svg/" url) filename)))