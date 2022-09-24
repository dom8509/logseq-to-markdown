(ns logseq-to-markdown.renderer.kroki
  (:require ["http" :as http]
            ["pako" :as pako]
            ["fs" :as fs]
            ;; [clojure.core.async :refer [<! go]]
            [clojure.string :as s]))

;; (defn get-request 
;;   [url]
;;   (go (<! (http/get url))))

;; (defn read-response [response-chan]
;;   (go (let [resp (<! response-chan)]
;;         (println resp))))  ; <- the only place where you can "touch" resp!

(defn- download-image
  [url]
  ;; (read-response (get-request url))
  ;; (let [response (http/get url (fn [response] 
  ;;                                (println (str "Body 1: " (:body response)))))]
  ;;   (println (str "Body: " (:body response))))
  (let [f (fs/createWriteStream "out_kroki.png")]
    (println (str "f: " f))
    (println url)
    (http/get url
              (fn [response]
                (println (str "response: " (.stringify js/JSON response)))))))

(defn- replace-chars
  [text]
  (s/replace (s/replace text "+" "-") "/" "_"))

(defn render-image
  [code, type]
  (let [buffer (.from js/Buffer code "utf-8")
        compressed (.deflate pako buffer {:level 9})
        url (replace-chars (.toString (.from js/Buffer compressed) "base64"))]
    (download-image (str "http://kroki.io/" type "/svg/" url))
    url))