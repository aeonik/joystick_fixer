(ns aeonik.controlmap.index
  "HTML and index generation for mapped SVGs"
  (:require
   [aeonik.controlmap.discovery :as discovery]
   [aeonik.controlmap.state :as state]
   [clojure.java.io :as io]
   [clojure.edn :as edn]
   [clojure.string :as str]
   [net.cgrand.enlive-html :as html]))

;; =============================================================================
;; Configuration Management
;; =============================================================================

(defn get-config
  "Loads the application configuration from config.edn (via discovery)."
  []
  (discovery/get-config))

(defn get-svg-config
  "Gets the SVG generation config from the global config."
  []
  (-> (get-config) :mapping :svg-generation))

;; =============================================================================
;; Resource Management (CSS, Output)
;; =============================================================================

(defn copy-resource!
  "Copies a resource from the jar/resources to a target path.
   Returns the output file path or nil if missing."
  [resource out-path]
  (if-let [res (io/resource resource)]
    (do
      (io/make-parents out-path)
      (with-open [in (io/input-stream res)
                  out (io/output-stream out-path)]
        (io/copy in out))
      (println "Copied resource:" out-path)
      out-path)
    (do
      (println (format "Warning: Resource %s not found" resource))
      nil)))

(defn copy-css-to-output!
  "Copies the default styles.css from resources to the output dir."
  [output-dir]
  (copy-resource! "styles.css" (str (io/file output-dir "styles.css"))))

;; =============================================================================
;; Joystick Instance Metadata & Mapping Info
;; =============================================================================

(defn get-context
  "Gets the project context, initializing if needed."
  []
  (state/get-context))

(defn joystick-instance-info
  "Returns a sequence of info maps for each detected joystick (for index/table output)."
  []
  (let [ctx           (get-context)
        {:keys [joystick-ids config]} ctx
        svg-config    (-> config :mapping :svg-generation)
        output-dir    (:default-output-dir svg-config)
        filename-prefix (:filename-prefix svg-config)
        display-names (fn [short-name instance-id]
                        {:title (str (str/capitalize (str/replace (str short-name) #"_" " ")))
                         :description (or (get-in joystick-ids [instance-id :product])
                                          "Unknown device")})]
    (->> joystick-ids
         (map (fn [[instance-id joy]]
                (let [short-name (:short-name joy)
                      fname      (str filename-prefix short-name ".svg")
                      path       (str output-dir "/" fname)
                      disp-info  (display-names short-name instance-id)]
                  {:instance-id instance-id
                   :short-name  short-name
                   :svg-path    path
                   :filename    fname
                   :title       (:title disp-info)
                   :description (:description disp-info)})))
         (filter :short-name)
         (sort-by :instance-id)
         vec)))

;; =============================================================================
;; HTML Generation (Enlive)
;; =============================================================================

(defn create-base-html
  "Creates the basic HTML skeleton as data structure."
  []
  (html/html
   [:html {:lang "en"}
    [:head
     [:meta {:charset "UTF-8"}]
     [:meta {:name "viewport" :content "width=device-width, initial-scale=1.0"}]
     [:title "Star Citizen Joystick SVG Layouts"]
     [:link {:rel "stylesheet" :href "styles.css"}]]
    [:body
     [:h1 "Star Citizen Joystick SVG Layouts"]
     [:div.svg-grid]]]))

(defn joystick-svg-html-block
  "Returns an Enlive HTML node for a single joystick's SVG output."
  [{:keys [instance-id title description filename svg-path]}]
  {:tag :div
   :attrs {:class "svg-item"}
   :content
   [{:tag :h3
     :content [(format "%s (Instance %d)" title instance-id)]}
    {:tag :div
     :attrs {:class "instance-info"}
     :content [description]}
    {:tag :div
     :attrs {:class "svg-container"}
     :content [{:tag :object
                :attrs {:data filename :type "image/svg+xml"}
                :content [{:tag :p :content [(str "SVG not found: " filename)]}]}]}]})

(defn build-index-html-structure
  "Expands the HTML document with all joystick SVG blocks."
  []
  (let [joystick-items (joystick-instance-info)
        base (create-base-html)]
    (html/at base
             [:.svg-grid]
             (html/content (map joystick-svg-html-block joystick-items)))))

(defn make-index-html-string
  "Renders the HTML structure to a HTML string."
  []
  (let [doc (build-index-html-structure)]
    (str "<!doctype html>\n" (apply str (html/emit* doc)))))

;; =============================================================================
;; Output/Filesystem Integration
;; =============================================================================

(defn generate-index-html!
  "Writes the index.html (with joystick SVG blocks) to disk.
   Optionally specify the path (default: out/index.html).
   Returns output path."
  ([] (generate-index-html! "out/index.html"))
  ([output-path]
   (let [output-file (io/file output-path)
         output-dir (.getParent output-file)
         _ (copy-css-to-output! output-dir)
         html-content (make-index-html-string)]
     (io/make-parents output-file)
     (spit output-file html-content)
     (println "Generated HTML index:" output-path)
     output-path)))

(defn generate-index-with-output-dir!
  "Generates index.html in the SVG output directory as determined from config."
  []
  (let [svg-config   (get-svg-config)
        output-dir   (:default-output-dir svg-config)
        output-path  (str (io/file output-dir "index.html"))]
    (generate-index-html! output-path)))

;; =============================================================================
;; SVG Output Status & Verification Helpers
;; =============================================================================

(defn check-svg-files-exist
  "Scans expected output SVGs and marks existence."
  []
  (let [joysticks (joystick-instance-info)]
    (map (fn [info]
           (assoc info :exists? (.exists (io/file (:svg-path info)))))
         joysticks)))

(defn print-svg-status!
  "Prints a table of which SVG files are present or missing."
  []
  (let [items (check-svg-files-exist)]
    (println "\n=== SVG File Status ===")
    (doseq [{:keys [instance-id title svg-path exists?]} items]
      (println (format "Instance %d (%s): %s %s"
                       instance-id title svg-path
                       (if exists? "✓ EXISTS" "✗ MISSING"))))
    (println "=====================\n")))

;; =============================================================================
;; Development Helpers / Comments
;; =============================================================================

(comment
  ;; Generate index.html to default or any output
  (generate-index-html!)
  (generate-index-with-output-dir!)

  ;; Check which files are present
  (print-svg-status!)

  ;; Preview the HTML (as string, not written)
  (subs (make-index-html-string) 0 400)

  ;; See the joystick mapping info
  (joystick-instance-info))
