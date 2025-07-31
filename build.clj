(ns build
  (:require [clojure.tools.build.api :as b]))

(def lib 'aeonik/controlmap)
(def version "1.0.0")
(def class-dir "target/classes")
(def uber-file "target/controlmap-standalone.jar")

;; 👇 Just use your real deps.edn without any replacement or overrides
(def basis (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (b/delete {:path "target"}))

(defn uber [_]
  (clean nil)
  (println "Building controlmap standalone JAR...")

  ;; Don't copy individual source files — let compile-clj handle it
  ;; just copy full resources
  (b/copy-dir {:src-dirs ["resources"]
               :target-dir class-dir})

  ;; Compile only the required namespaces from full src tree
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir
                  :compile-opts {:direct-linking true}
                  :ns-compile '[aeonik.controlmap.core
                                aeonik.controlmap.discovery
                                aeonik.controlmap.index]})

  ;; Package as uberjar
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'aeonik.controlmap.core})

  (println "Built:" uber-file))
