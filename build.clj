(ns build
  (:require [clojure.tools.build.api :as b]
            [clojure.java.io :as io]))

(def lib 'aeonik/controlmap)
(def version "1.0.0")
(def class-dir "target/classes")
(def uber-file "target/controlmap-standalone.jar")
(def basis (b/create-basis {:project "deps.edn"}))

(defn clean [_]
  (println "🧼 Cleaning target...")
  (b/delete {:path "target"}))

(defn copy-resources-to-classpath []
  (println "📦 Copying resources into JAR classpath...")
  (b/copy-dir {:src-dirs ["resources"]
               :target-dir class-dir}))

(defn copy-resources-for-filesystem []
  (let [target-dir "target/resources"]
    (println "🗂  Copying resources for runtime (external)...")
    (b/copy-dir {:src-dirs ["resources"]
                 :target-dir target-dir})))

(defn uber [_]
  (clean nil)

  ;; Step 1: copy resources into classpath and for disk access
  (copy-resources-to-classpath)
  (copy-resources-for-filesystem)

  ;; Step 2: compile selected namespaces
  (println "🛠  Compiling namespaces...")
  (b/compile-clj {:basis basis
                  :src-dirs ["src"]
                  :class-dir class-dir
                  :compile-opts {:direct-linking true}
                  :ns-compile '[aeonik.controlmap.core
                                aeonik.controlmap.discovery
                                aeonik.controlmap.index
                                aeonik.controlmap.state
                                aeonik.controlmap.gui
                                aeonik.controlmap.svg]})

  ;; Step 3: build the jar
  (println "📦 Creating uberjar...")
  (b/uber {:class-dir class-dir
           :uber-file uber-file
           :basis basis
           :main 'aeonik.controlmap.gui})

  (println "✅ Build complete!")
  (println "   ├─ JAR:     " uber-file)
  (println "   └─ Assets:  target/resources/"))
