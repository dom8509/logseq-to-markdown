(ns logseq-to-markdown.renderer.echarts)

;; ECharts rendering requires canvas and echarts packages.
;; These are optional dependencies, so we provide a no-op stub
;; when they are not available.
(defn render-image
  [code width height filename]
  (println "Info: ECharts rendering not available (canvas and echarts dependencies required for diagram prerendering)"))