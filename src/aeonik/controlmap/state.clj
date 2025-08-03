(ns aeonik.controlmap.state
  (:require
   [aeonik.controlmap.discovery :as discovery]
   [clojure.java.io :as io]
   [tupelo.parse.xml :as tx]))

(defn load-actionmaps
  "Loads actionmaps XML, first trying discovery, then falling back to resources"
  []
  (if-let [actionmaps-file (discovery/find-actionmaps)]
    (do
      (println "Loading actionmaps from:" (.getAbsolutePath actionmaps-file))
      (with-open [reader (io/reader actionmaps-file)]
        (tx/parse-streaming reader)))
    (do
      (println "No actionmaps found via discovery, using bundled resource")
      (-> "actionmaps.xml"
          io/resource
          io/reader
          tx/parse-streaming))))

(def actionmaps (load-actionmaps))

(def svg-roots
  "Lazy-loaded SVG resources"
  (load-svg-resources))

(defn load-svg-resources
  "Loads all SVG resources into memory, filtering out missing files"
  []
  (let [svg-map (get-product-svg-mapping)]
    (into {}
          (keep (fn [[key fname]]
                  (if-let [resource (io/resource fname)]
                    [fname (-> resource io/reader tx/parse-streaming)]
                    (do
                      (println "Warning: SVG resource not found:" fname)
                      nil)))
                svg-map))))
