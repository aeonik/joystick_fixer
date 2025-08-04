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
  "Loads all SVG resources into memory.
  Returns a map of filename strings to Enlive trees:
  {String -> {:tag :svg, :attrs map, :content vector}}
  e.g.
  Example:
  { \"svg/alpha_L.svg\" => {:tag :svg, :attrs {...}, :content [...]}
    \"svg/alpha_R.svg\" => {:tag :svg, :attrs {...}, :content [...]} }"
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

(defn edn-files->map
  "Loads all EDN files in the given directory and returns a map of parsed data.

  Returns:
  {Keyword
   {:text-coordinates
    {Keyword
     {:id String, :x float, :y float}
     :button-rect-dimensions {:width float, :height float, :rx float, :ry float}}}}"
  [dir]
  (->> (file-seq (io/file dir))
       (filter #(and (.isFile %)
                     (str/ends-with? (.getName %) ".edn")))
       (map (fn [f]
              [(keyword (str/replace (.getName f) #"\.edn$" ""))
               (edn/read-string (slurp f))]))
       (into {})))

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

(def actionmaps (load-actionmaps))

(def svg-roots (load-svg-resources))

(def svg-edn-files->map (edn-files->map "resources/config/svg/"))

(def joystick-ids (find-joystick-ids actionmaps))

(defn build-job-context []
  {:joystick-ids joystick-ids
   :svg-roots svg-roots
   :svg-config (-> discovery/config :mapping :svg-generation)
   :config discovery/config
   :actionmaps actionmaps})

(def context (build-job-context))
