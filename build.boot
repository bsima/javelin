(set-env!
  :dependencies   '[[org.clojure/clojure             "1.7.0"      :scope "provided"]
                    [org.clojure/clojurescript       "1.7.122"    :scope "provided"]
                    [adzerk/bootlaces                "0.1.10"     :scope "test"]
                    [adzerk/boot-cljs                "1.7.48-3"   :scope "test"]
                    [adzerk/boot-test                "1.0.4"      :scope "test"]
                    [tailrecursion/cljs-priority-map "1.0.3"]
                    [org.clojure/data.priority-map   "0.0.2"]
                    [riddley                         "0.1.6"]]
  :resource-paths #{"src" "test"})

(require
  '[clojure.java.io  :as io]
  '[adzerk.bootlaces :refer :all]
  '[adzerk.boot-cljs :refer :all]
  '[adzerk.boot-test :refer :all])

(def +version+ "3.8.4")

(bootlaces! +version+)

(task-options!
  pom  {:project     'hoplon/javelin
        :version     +version+
        :description "Spreadsheet-like dataflow programming in ClojureScript"
        :url         "https://github.com/hoplon/javelin"
        :scm         {:url "https://github.com/hoplon/javelin"}
        :license     {"Eclipse Public License" "http://www.eclipse.org/legal/epl-v10.html"}})

(deftask test-runner
  []
  (let [tmp (tmp-dir!)]
    (with-pre-wrap fileset
      (with-let [fs fileset]
        (empty-dir! tmp)
        (doseq [[path in-file] (map (juxt tmp-path tmp-file) (output-files fs))]
          (let [out-file (doto (io/file tmp path) io/make-parents)]
            (io/copy in-file out-file)))
        (binding [*sh-dir* (.getPath tmp)] (dosh "bash" "run.sh"))))))

(deftask test-cljs
  "Run Javelin tests on ClojureScript"
  [a advanced bool "Test with :advanced optimizations."]
  (merge-env! :resource-paths #{"test"})
  (when advanced (task-options! cljs {:optimizations :advanced}))
  (comp (cljs) (test-runner)))

(deftask test-clj
  "Run Javelin tests on Clojure"
  []
  (merge-env! :resource-paths #{"test"})
  (comp (speak) (test :namespaces '[javelin.core-clj-test])))
