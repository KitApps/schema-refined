(defproject com.attendify/schema-refined "0.3.0-alpha4"
  :description "Clojure library to keep you away from bugs with precise schemas (refined types with runtime checks)"
  :Url "https://github.com/KitApps/schema-refined"
  :license {:name "The MIT License"
            :url "http://opensource.org/licenses/MIT"}

  :dependencies [[prismatic/schema "1.1.9"]]

  :profiles {:dev {:dependencies [[org.clojure/clojure "1.7.0"]
                                  [potemkin "0.4.5"]]}
             :1.8 {:dependencies [[org.clojure/clojure "1.8.0"]
                                  [potemkin "0.4.5"]]}
             :1.9 {:dependencies [[org.clojure/clojure "1.9.0"]
                                  [potemkin "0.4.5"]]}
             :1.10 {:dependencies [[org.clojure/clojure "1.10.0-alpha4"]
                                   [potemkin "0.4.5"]]}})
