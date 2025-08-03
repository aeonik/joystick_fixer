(ns aeonik.controlmap.state
  (:require
   [aeonik.controlmap.discovery :as discovery]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [tupelo.forest :as f]
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

(defn load-svg-resources
  "Loads all SVG resources into memory, filtering out missing files"
  []
  (let [svg-map (discovery/get-product-svg-mapping)]
    (into {}
          (keep (fn [[key fname]]
                  (if-let [resource (io/resource fname)]
                    [fname (-> resource io/reader tx/parse-streaming)]
                    (do
                      (println "Warning: SVG resource not found:" fname)
                      nil)))
                svg-map))))

(defn edn-files->map [dir]
  (->> (file-seq (io/file dir))
       (filter #(and (.isFile %)
                     (str/ends-with? (.getName %) ".edn")))
       (map (fn [f]
              [(keyword (str/replace (.getName f) #"\.edn$" ""))
               (edn/read-string (slurp f))]))
       (into {})))

(def actionmaps (load-actionmaps))

(def joystick-ids (discovery/find-joystick-ids actionmaps))

(def svg-roots (load-svg-resources))

(def svg-edn-files->map "resources/config/svg/")

(defn find-joystick-ids
  "Extracts joystick instance IDs and their corresponding SVGs from actionmaps
  e.g {1 \"svg/alpha_L.svg\"}"
  [actionmaps]
  (let [product-svg-mapping (discovery/get-product-svg-mapping)]
    (f/with-forest (f/new-forest)
      (-> actionmaps
          f/add-tree-enlive
          (f/find-hids [:** {:type "joystick"}])
          (->>
           (keep (fn [hid]
                   (let [node (f/hid->node hid)
                         inst (some-> node :instance parse-long)
                         prod (:Product node)]
                     (when (and inst prod)
                       (when-let [[_ svg]
                                  (some (fn [[re s]]
                                          (when (re-find re prod) [re s]))
                                        product-svg-mapping)]
                         [inst svg])))))
           (into {}))))))

(comment
  (find-joystick-ids actionmaps))
