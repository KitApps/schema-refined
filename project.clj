(defproject com.attendify/schema-refined "0.3.0-alpha5"
  :description "Clojure library to keep you away from bugs with precise schemas (refined types with runtime checks)"
  :Url "https://github.com/KitApps/schema-refined"
  :license {:name "The MIT License"
            :url  "http://opensource.org/licenses/MIT"}

  :dependencies [[prismatic/schema "1.1.9"]]

  :plugins [[lein-doo "0.1.8"]
            [lein-cljsbuild "1.1.7"
             :exclusions [org.clojure/clojure]]]

  :clean-targets ^{:protect false} [:target-path "test-out"]

  :cljsbuild {:builds [{:id           "test"
                        :source-paths ["src" "test"]
                        :compiler     {:main                 schema-refined.runner
                                       :output-to            "test-out/schema-refined.test.js"
                                       :optimizations        :none
                                       :target               :nodejs
                                       :source-map           true
                                       :source-map-timestamp true}}]}

  :profiles {:dev  {:dependencies [[org.clojure/clojure "1.7.0"]
                                   [org.clojure/clojurescript "1.10.329"]]}
             :1.8  {:dependencies [[org.clojure/clojure "1.8.0"]]}
             :1.9  {:dependencies [[org.clojure/clojure "1.9.0"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.0-alpha4"]]}}

  :deploy-repositories {"clojars" {:sign-releases false}})
