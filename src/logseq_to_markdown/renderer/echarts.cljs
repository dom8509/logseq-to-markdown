(ns logseq-to-markdown.renderer.echarts
  (:require ["echarts" :as echarts]
            ["canvas" :as canvas]
            [logseq-to-markdown.utils :as utils]
            [logseq-to-markdown.fs :as fs]))

(defn render-image
  [code width height filename]
  (let [jsondata (.parse js/JSON (clj->js (utils/string-to-json code)))
        canvas (canvas/createCanvas width height)
        echart (echarts/init canvas)]
    (.setOption echart jsondata)
    (let [data (.toBuffer canvas "image/png")]
      (fs/store-asset data filename))))