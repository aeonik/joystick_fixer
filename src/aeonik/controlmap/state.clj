(ns aeonik.controlmap.state
  (:require
   [aeonik.controlmap.discovery :as discovery]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hickory.core :as h]
   [hickory.select :as s]))

;; =============================================================================
;; Loading Functions
;; =============================================================================

(defn load-actionmaps
  "Loads actionmaps XML as Hickory data structure.
   First tries discovery, then falls back to bundled resources.
   Returns the parsed Hickory tree or throws an exception."
  []
  (let [source (or (discovery/find-actionmaps)
                   (io/resource "actionmaps.xml"))]
    (if source
      (let [path (if (instance? java.io.File source)
                   (.getAbsolutePath source)
                   (.toString source))]
        (println "Loading actionmaps from:" path)
        (-> source
            io/reader
            slurp
            h/parse
            h/as-hickory))  ; CONVERT TO HICKORY HERE
      (throw (ex-info "No actionmaps found"
                      {:searched-paths (discovery/get-search-paths)})))))

(defn load-svg-resource
  "Loads a single SVG resource. Returns [keyword hickory-tree] or nil."
  [fname]
  (try
    (when-let [resource (io/resource (str "svg/" fname ".svg"))]
      [(keyword fname)
       (-> resource
           io/reader
           slurp
           h/parse
           h/as-hickory)])  ; CONVERT TO HICKORY HERE
    (catch Exception e
      (println (format "Warning: Failed to load SVG '%s': %s"
                       fname (.getMessage e)))
      nil)))

(defn load-svg-resources
  "Loads all SVG resources into memory as Hickory trees.
   Returns a map of keywordized short-names to Hickory trees.
   Continues on individual load failures."
  []
  (let [svg-map (discovery/get-product-svg-mapping)
        loaded (keep (fn [[_ fname]] (load-svg-resource fname)) svg-map)
        result (into {} loaded)]
    (println (format "Loaded %d/%d SVG resources"
                     (count result)
                     (count svg-map)))
    result))

(defn load-edn-file
  "Loads a single EDN file. Returns [keyword data] or nil on error."
  [file]
  (try
    (let [fname (.getName file)
          key (keyword (str/replace fname #"\.edn$" ""))
          data (edn/read-string (slurp file))]
      [key data])
    (catch Exception e
      (println (format "Warning: Failed to load EDN file '%s': %s"
                       (.getName file) (.getMessage e)))
      nil)))

(defn load-edn-configs
  "Loads all EDN files from the given directory.
   Returns a map of keyword -> parsed data.
   Continues on individual file failures."
  [dir-path]
  (let [dir (io/file dir-path)]
    (if (.exists dir)
      (let [edn-files (->> (file-seq dir)
                           (filter #(and (.isFile %)
                                         (str/ends-with? (.getName %) ".edn"))))
            loaded (keep load-edn-file edn-files)
            result (into {} loaded)]
        (println (format "Loaded %d EDN config files from %s"
                         (count result) dir-path))
        result)
      (do
        (println (format "Warning: EDN config directory not found: %s" dir-path))
        {}))))

;; =============================================================================
;; Extraction Functions
;; =============================================================================

(defn extract-joystick-instance
  "Extracts joystick instance data from a single options node.
   Returns [instance-id info-map] or nil."
  [svg-mapping {:keys [attrs]}]
  (let [instance (some-> (:instance attrs) parse-long)
        product (:product attrs)]
    (cond
      (nil? instance)
      (do
        (println (format "⚠️ Skipping joystick with invalid instance: %s"
                         product))
        nil)

      (nil? product)
      (do
        (println (format "⚠️ Skipping instance %d with no product info"
                         instance))
        nil)

      :else
      (let [match (some (fn [[regex name]]
                          (when (re-find regex product)
                            [regex name]))
                        svg-mapping)]
        (if match
          (let [[regex short-name] match]
            [instance {:product product
                       :name product  ; Could be enhanced with better naming
                       :short-name short-name
                       :match-regex regex}])
          (do
            (println (format "⚠️ No SVG mapping for: %s (instance %d)"
                             product instance))
            [instance {:product product
                       :name product
                       :short-name nil
                       :match-regex nil}]))))))

(defn extract-joystick-instances
  "Extracts all joystick instance configurations from actionmaps.
   Returns a map of instance-id -> info.
   Note: actionmaps should already be in Hickory format."
  [actionmaps]
  (let [selector (s/and (s/tag :options)
                        (s/attr :type #(= % "joystick"))
                        (s/attr :product))
        options (s/select selector actionmaps)  ; Already Hickory, no conversion needed
        svg-mapping (discovery/get-product-svg-mapping)]
    (->> options
         (keep (partial extract-joystick-instance svg-mapping))
         (into {}))))

;; =============================================================================
;; Display Helpers
;; =============================================================================

(defn get-display-name
  "Gets a human-readable display name for a file.
   Useful for GUI applications."
  [file joystick-ids]
  (let [filename (if (instance? java.io.File file)
                   (.getName file)
                   (str file))
        short-name (-> filename
                       (str/replace #"^updated_" "")
                       (str/replace #"\.svg$" ""))]
    (if-let [joystick (some (fn [[_ info]]
                              (when (= (:short-name info) short-name)
                                info))
                            joystick-ids)]
      (or (:name joystick)
          (str (:match-regex joystick))
          short-name)
      short-name)))

;; =============================================================================
;; Context Management
;; =============================================================================

(defn build-context
  "Builds a complete context map with all necessary data.
   This is the main entry point for initializing the system.

   All data structures are in Hickory format for consistency.

   Options:
   - :skip-svgs - Don't load SVG resources (for faster testing)
   - :skip-edn - Don't load EDN configs
   - :actionmaps - Pre-loaded actionmaps (must be in Hickory format)"
  [& {:keys [skip-svgs skip-edn actionmaps]}]
  (println "\n══════════════════════════════════════")
  (println "   Initializing ControlMap Context")
  (println "══════════════════════════════════════\n")

  (let [start-time (System/currentTimeMillis)

        ;; Load actionmaps (already in Hickory format)
        _ (println "▶ Loading actionmaps...")
        actionmaps (or actionmaps (load-actionmaps))

        ;; Extract joystick instances
        _ (println "▶ Extracting joystick instances...")
        joystick-ids (extract-joystick-instances actionmaps)
        _ (println (format "  Found %d joystick instances" (count joystick-ids)))

        ;; Load SVG resources (already in Hickory format)
        _ (println "▶ Loading SVG resources...")
        svg-roots (if skip-svgs
                    (do (println "  Skipped (skip-svgs flag)") {})
                    (load-svg-resources))

        ;; Load EDN configs
        _ (println "▶ Loading EDN configurations...")
        edn-configs (if skip-edn
                      (do (println "  Skipped (skip-edn flag)") {})
                      (load-edn-configs "resources/config/svg/"))

        ;; Get config
        config (discovery/get-config)

        elapsed (- (System/currentTimeMillis) start-time)]

    (println (format "\n✓ Context initialized in %.2f seconds"
                     (/ elapsed 1000.0)))
    (println "══════════════════════════════════════\n")

    {:actionmaps actionmaps          ; Hickory format
     :joystick-ids joystick-ids
     :svg-roots svg-roots            ; Map of Hickory trees
     :svg-config (-> config :mapping :svg-generation)
     :svg-edn-configs edn-configs
     :config config
     :initialized-at (java.util.Date.)}))

;; =============================================================================
;; State Atom (Optional - for REPL/development)
;; =============================================================================

(defonce ^:dynamic *context* (atom nil))

(defn init!
  "Initialize the global context atom. Useful for REPL development.
   Returns the initialized context."
  [& opts]
  (let [ctx (apply build-context opts)]
    (reset! *context* ctx)
    ctx))

(defn get-context
  "Get the current context, initializing if needed."
  []
  (or @*context* (init!)))

;; =============================================================================
;; Lazy Initialization (for production use)
;; =============================================================================

(def context
  "Lazy-initialized context. Will be computed on first access.
   All data is in Hickory format."
  (delay (build-context)))

(defn ensure-initialized
  "Ensures context is initialized. Forces the delay if needed."
  []
  (force context))

;; =============================================================================
;; Development Helpers
;; =============================================================================

(comment
  ;; Initialize with all data
  (init!)

  ;; Initialize quickly for testing (skip SVGs)
  (init! :skip-svgs true)

  ;; Get current context
  (get-context)

  ;; Build a fresh context without storing it
  (build-context)

  ;; Access lazy context
  @context

  ;; Check what's loaded (all should be Hickory format)
  (keys (:svg-roots @*context*))
  (keys (:joystick-ids @*context*))

  ;; Verify Hickory format
  (-> @*context* :svg-roots first val :type)  ; Should be :element
  (-> @*context* :actionmaps :type)           ; Should be :document

  ;; Test display name
  (get-display-name "updated_alpha_L.svg" (:joystick-ids @*context*)))
