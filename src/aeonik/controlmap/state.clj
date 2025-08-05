(ns aeonik.controlmap.state
  (:require
   [aeonik.controlmap.discovery :as discovery]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [net.cgrand.enlive-html :as html]
   [hickory.core :as h]
   [hickory.select :as s]))

(defn load-actionmaps
  "Loads actionmaps XML, first trying discovery, then falling back to resources"
  []
  (if-let [actionmaps-file (discovery/find-actionmaps)]
    (do
      (println "Loading actionmaps from:" (.getAbsolutePath actionmaps-file))
      (with-open [reader (io/reader actionmaps-file)]
        (h/parse (slurp reader))))
    (do
      (println "No actionmaps found via discovery, using bundled resource")
      (-> "actionmaps.xml"
          io/resource
          io/reader
          slurp
          h/parse))))

(defn load-svg-resources
  "Loads all SVG resources into memory.
  Returns a map of keywordized short-names to Enlive trees:
  {:alpha_L {:tag :svg, :attrs {...}, :content [...]}
   :alpha_R {:tag :svg, :attrs {...}, :content [...]}}"
  []
  (let [svg-map (discovery/get-product-svg-mapping)]
    (into {}
          (keep (fn [[_ fname]]
                  (if-let [resource (io/resource (str "svg/" fname ".svg"))]
                    [(keyword fname) (-> resource io/reader slurp h/parse)]
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

(defn extract-joystick-instances [actionmaps]
  (let [options (s/select (s/and (s/tag :options)
                                 (s/attr :type #(= % "joystick"))
                                 (s/attr :product #(not= % nil)))
                          (h/as-hickory actionmaps))

        svg-mapping (discovery/get-product-svg-mapping)]
    (into {}
          (keep (fn [{:keys [attrs]}]
                  (let [instance (some-> (:instance attrs) parse-long)
                        product  (:product attrs)
                        match    (some (fn [[regex name]]
                                         (when (re-find regex product)
                                           [regex name]))
                                       svg-mapping)]
                    (cond
                      (nil? instance)
                      (do
                        (println "⚠️ Warning: Skipping joystick with missing or invalid instance ID. Product:" product)
                        nil)

                      (nil? match)
                      (do
                        (println "⚠️ Warning: No SVG mapping found for joystick product:" product)
                        [instance {:product product
                                   :short-name nil
                                   :match-regex nil}])

                      :else
                      (let [[regex short-name] match]
                        [instance {:product product
                                   :short-name short-name
                                   :match-regex regex}])))))
          options)))

(def actionmaps (load-actionmaps))

(def svg-roots (load-svg-resources))

(def svg-edn-files->map (edn-files->map "resources/config/svg/"))

(def joystick-ids (extract-joystick-instances actionmaps))

(defn get-display-name
  "Useful for the gui"
  [^java.io.File f]
  (let [filename (.getName f)
        short-name (-> filename
                       (str/replace #"^updated_" "")
                       (str/replace #"\.svg$" ""))
        joystick-entry (->> joystick-ids
                            vals
                            (filter #(= (:short-name %) short-name))
                            first)]
    (if joystick-entry
      (str (:match-regex joystick-entry))
      short-name)))

(defn build-job-context []
  {:joystick-ids joystick-ids
   :svg-roots svg-roots ;; legacy, still useful for debug
   :svg-config (-> discovery/config :mapping :svg-generation)
   :svg-edn-files svg-edn-files->map ;; new preferred EDN representation
   :config discovery/config
   :actionmaps actionmaps})

(def context (build-job-context))
