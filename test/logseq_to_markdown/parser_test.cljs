(ns logseq-to-markdown.parser-test
  (:require [cljs.test :refer [deftest is run-tests testing]]
            [logseq-to-markdown.config :as config]
            [logseq-to-markdown.graph :as graph]
            [logseq-to-markdown.links :as links]))

(defn- parse-with-public-pages
  [text pages]
  (with-redefs [graph/page-exists? #(contains? pages %)]
    (links/parse-links text)))

(deftest parses-all-supported-link-formats
  (config/set {:trim-namespaces false})
  (testing "plain, hashtag and labelled links"
    (is (= "[Page]({{< ref \"/pages/Page\" >}})"
           (parse-with-public-pages "[[Page]]" #{"Page"})))
    (is (= "#[Page]({{< ref \"/pages/Page\" >}})"
           (parse-with-public-pages "#[[Page]]" #{"Page"})))
    (is (= "#[tag]({{< ref \"/pages/tag\" >}})"
           (parse-with-public-pages "#tag" #{"tag"})))
    (is (= "[label]({{< ref \"/pages/Page\" >}})"
           (parse-with-public-pages "[label]([[Page]])" #{"Page"})))))

(deftest converts-every-tag-in-a-line
  (config/set {:trim-namespaces false})
  (is (= "#[fi]({{< ref \"/pages/fi\" >}}) and #[en]({{< ref \"/pages/en\" >}})"
         (parse-with-public-pages "#fi and #en" #{"fi" "en"}))))

(deftest supports-common-tag-characters
  (config/set {:trim-namespaces false})
  (is (= "#[foo-bar]({{< ref \"/pages/foo-bar\" >}}) #[grüße]({{< ref \"/pages/grüße\" >}})"
         (parse-with-public-pages "#foo-bar #grüße" #{"foo-bar" "grüße"}))))

(deftest preserves-unpublished-links-and-url-fragments
  (config/set {:trim-namespaces false})
  (is (= "Page #tag https://example.com/#fragment"
         (parse-with-public-pages "[[Page]] #tag https://example.com/#fragment" #{}))))

(deftest trims-namespace-labels-without-changing-targets
  (config/set {:trim-namespaces true})
  (is (= "[Page]({{< ref \"/pages/ns/Page\" >}}) #[tag]({{< ref \"/pages/ns/tag\" >}})"
         (parse-with-public-pages "[[ns/Page]] #[[ns/tag]]" #{"ns/Page" "ns/tag"}))))

(defn -main
  []
  (let [{:keys [fail error]} (run-tests 'logseq-to-markdown.parser-test)]
    (when (pos? (+ fail error))
      (js/process.exit 1))))
