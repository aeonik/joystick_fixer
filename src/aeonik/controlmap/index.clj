(ns aeonik.controlmap.index
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.edn :as edn]
   [aeonik.controlmap.discovery :as disocvery]
   [net.cgrand.enlive-html :as html]
   [aeonik.controlmap.discovery :as discovery]
   [aeonik.controlmap.state :as state]))

;; =============================================================================
;; Configuration Loading
;; =============================================================================

(def ^:private config
  "Application configuration loaded from config.edn"
  (-> "config.edn"
      io/resource
      slurp
      edn/read-string))

;; =============================================================================
;; CSS Management
;; =============================================================================
(defn copy-css-to-output!
  "Copies CSS file from resources to output directory"
  [output-dir]
  (let [css-resource (io/resource "styles.css")
        css-path (str output-dir "/styles.css")]
    (if css-resource
      (do
        (io/make-parents css-path)
        (io/copy (io/input-stream css-resource) (io/file css-path))
        (println "Copied CSS file to:" css-path)
        css-path)
      (do
        (println "Warning: styles.css not found in resources")
        nil))))

;; =============================================================================
;; Joystick Information & Mapping
;; =============================================================================

;; TODO: Use state/context for this, don't hard code this here, see state/joystick-ids
(defn get-joystick-display-info
  "Returns display information for joysticks based on their SVG filenames"
  []
  {"alpha_L"              {:title "Alpha L" :description "Left Alpha joystick mappings"}
   "alpha_R"              {:title "Alpha R" :description "Right Alpha joystick mappings"}
   "panel_1"              {:title "Panel 1" :description "Panel 1 mappings"}
   "panel_2"              {:title "Panel 2" :description "Panel 2 mappings"}
   "panel_3"              {:title "Panel 3" :description "SharKa‑50 panel mappings"}
   "vpc_mongoose_t50cm3"  {:title "VPC Mongoose T50CM3" :description "Mongoose joystick mappings"}})

(defn get-generated-svg-info
  "Returns information about generated SVGs based on instance mapping and config"
  []
  (let [instance-mapping state/joystick-ids
        svg-config (get-in config [:mapping :svg-generation])
        output-dir (:default-output-dir svg-config)
        filename-prefix (:filename-prefix svg-config)
        display-info (get-joystick-display-info)]
    (->> instance-mapping
         (map (fn [[instance svg-path]]
                (let [filename (:short-name svg-path)
                      generated-filename (str filename-prefix filename ".svg")
                      generated-path (str output-dir "/" generated-filename)
                      info (get display-info filename {:title (str "Instance " instance)
                                                       :description "Unknown device"})]
                  {:instance instance
                   :title (:title info)
                   :description (:description info)
                   :svg-path generated-path
                   :filename generated-filename})))
         (sort-by :instance))))

;; =============================================================================
;; HTML Template with Enlive
;; =============================================================================

(defn create-base-template
  "Creates the base HTML structure"
  []
  (html/html
   [:html {:lang "en"}
    [:head
     [:meta {:charset "UTF-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:title "Updated SVG Joystick Mappings – Layout"]
     [:link {:rel "stylesheet" :href "styles.css"}]]
    [:body
     [:h1 "Updated SVG Joystick Mappings"]
     [:div.svg-grid]]]))

(defn generate-html-structure
  "Generates the complete HTML structure with all detected joysticks"
  []
  (let [svg-items (get-generated-svg-info)
        base (create-base-template)]
    (html/at base
             [:.svg-grid]
             (html/content
              (map (fn [{:keys [instance title description filename]}]
                     {:tag :div, :attrs {:class "svg-item"}
                      :content
                      [{:tag :h3, :content [(str title " (Instance " instance ")")]}
                       {:tag :div, :attrs {:class "instance-info"}, :content [description]}
                       {:tag :div, :attrs {:class "svg-container"}
                        :content
                        [{:tag :object, :attrs {:data filename, :type "image/svg+xml"}
                          :content
                          [{:tag :p, :content [(str "SVG not found: " filename)]}]}]}]})
                   svg-items)))))
;; =============================================================================
;; File Generation
;; =============================================================================

(defn generate-index-html!
  "Generates the index.html file with all joystick mappings"
  ([] (generate-index-html! "out/index.html"))
  ([output-path]
   (let [output-file (io/file output-path)
         output-dir (or (.getParent output-file) ".")  ; Default to current directory
         _ (copy-css-to-output! output-dir)  ; Copy CSS from resources
         html-structure (generate-html-structure)
         html-string (str "<!doctype html>\n" (apply str (html/emit* html-structure)))]
     (io/make-parents output-path)
     (spit output-path html-string)
     (println "Generated HTML index:" output-path)
     output-path)))

(defn generate-index-with-output-dir!
  "Generates index.html in the same directory as the SVG outputs"
  []
  (let [output-dir (get-in config [:mapping :svg-generation :default-output-dir])
        index-path (str output-dir "/index.html")]
    (generate-index-html! index-path)))

;; =============================================================================
;; Integration Helpers
;; =============================================================================

(defn check-svg-files-exist
  "Checks which generated SVG files actually exist on disk"
  []
  (let [svg-info (get-generated-svg-info)]
    (map (fn [info]
           (assoc info :exists? (.exists (io/file (:svg-path info)))))
         svg-info)))

(defn print-svg-status!
  "Prints status of all expected SVG files"
  []
  (let [svg-status (check-svg-files-exist)]
    (println "\n=== SVG File Status ===")
    (doseq [{:keys [instance title svg-path exists?]} svg-status]
      (println (format "Instance %d (%s): %s %s"
                       instance title svg-path
                       (if exists? "✓ EXISTS" "✗ MISSING"))))
    (println "=====================\n")))

;; =============================================================================
;; Development Helpers
;; =============================================================================

(comment
  ;; Generate the HTML index
  (generate-index-html!)

  ;; Generate in output directory
  (generate-index-with-output-dir!)

  ;; Check SVG file status
  (print-svg-status!)

  ;; Preview the generated structure
  (get-generated-svg-info)

  ;; Check what the HTML looks like
  (-> (generate-html-structure)
      html/emit*
      (->> (apply str))
      (subs 0 500)))
