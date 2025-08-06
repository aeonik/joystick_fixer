(ns aeonik.controlmap.core
  "Core functionality for Star Citizen control mapping SVG generation"
  (:gen-class)
  (:require
   [aeonik.controlmap.discovery :as discovery]
   [aeonik.controlmap.index :as index]
   [aeonik.controlmap.state :as state]
   [aeonik.controlmap.svg :as svg]
   [clojure.string :as str]
   [clojure.java.io :as io]
   [hickory.core :as h]
   [hickory.render :as render]
   [hickory.select :as s]
   [hickory.zip :as hzip]
   [clojure.zip :as zip]))

;; =============================================================================
;; Action Name Cleaning
;; =============================================================================

(defn clean-action-name
  "Removes common prefixes and arbitrary regex patterns from action names for cleaner display"
  [action-name]
  (let [config (discovery/get-config)
        cleaning-config (-> config :mapping :action-name-cleaning)
        {:keys [remove-v-prefix prefix-filters regex-filters]} cleaning-config
        filtered-name (if remove-v-prefix
                        (str/replace action-name #"^v_" "")
                        action-name)
        prefix-cleaned (reduce (fn [acc prefix]
                                 (if (str/starts-with? acc prefix)
                                   (subs acc (count prefix))
                                   acc))
                               filtered-name
                               prefix-filters)]
    (reduce (fn [acc regex-string]
              (str/replace acc (re-pattern regex-string) ""))
            prefix-cleaned
            (or regex-filters []))))

;; =============================================================================
;; Action Mapping Extraction
;; =============================================================================

(defn extract-input-action-mappings
  "Extracts all input-action mappings from the actionmaps.
   Returns a vector of {:input '...' :action '...'} maps."
  [actionmaps]
  (->> (h/as-hickory actionmaps)
       (s/select (s/tag :action))
       (map (fn [action-node]
              (let [action-name (get-in action-node [:attrs :name])
                    rebind-node (first (s/select (s/tag :rebind) action-node))
                    input (get-in rebind-node [:attrs :input])]
                {:input input
                 :action action-name})))
       (filter :action)  ; Remove any malformed entries
       vec))

(defn joystick-action-mappings
  "Returns action mappings for a specific joystick instance.
   Adds :svg-input field with the joystick prefix stripped."
  [actionmaps joystick-num]
  (let [prefix (str "js" joystick-num "_")
        all-mappings (extract-input-action-mappings actionmaps)]
    (->> all-mappings
         (filter (fn [{:keys [input]}]
                   (and input (str/starts-with? input prefix))))
         (map (fn [mapping]
                (assoc mapping
                       :svg-input (str/replace (:input mapping)
                                               (re-pattern (str "^" prefix))
                                               ""))))
         vec)))

;; =============================================================================
;; Joystick Information
;; =============================================================================

(defn joystick-info
  "Gathers all information about a specific joystick instance.
   Returns a map with all relevant data for SVG generation."
  [context instance-id]
  (let [{:keys [joystick-ids svg-roots svg-edn-configs config actionmaps]} context
        joystick-data (get joystick-ids instance-id)
        {:keys [short-name product match-regex]} joystick-data
        svg-key (some-> short-name keyword)
        svg-root (get svg-roots svg-key)
        svg-edn (get svg-edn-configs svg-key)
        mappings (joystick-action-mappings actionmaps instance-id)
        svg-config (-> config :mapping :svg-generation)]
    {:instance-id instance-id
     :short-name short-name
     :product product
     :match-regex match-regex
     :svg-key svg-key
     :svg-root svg-root
     :svg-edn svg-edn
     :mappings mappings
     :svg-config svg-config
     :has-svg? (boolean svg-root)
     :mapping-count (count mappings)}))

;; =============================================================================
;; SVG Update Functions
;; =============================================================================

(defn update-svg-from-mappings
  "Updates an SVG tree with action mappings.
   Uses the svg/update-nodes function for clean updates."
  [tree mappings & {:keys [format-fn additional-attrs selector-attr]
                    :or {format-fn clean-action-name
                         additional-attrs {}
                         selector-attr :data-for}}]
  (reduce (fn [current-tree mapping]
            (let [svg-input (:svg-input mapping)
                  action (:action mapping)]
              (if (str/blank? svg-input)
                current-tree
                (svg/update-nodes current-tree
                                  (s/attr selector-attr #(= % svg-input))
                                  (svg/compose-edits
                                   (svg/make-content-updater (format-fn action))
                                   (svg/make-attr-updater
                                    (merge {:data-action action}
                                           additional-attrs)))
                                  :first-only? true))))
          tree
          mappings))

(defn render-svg
  "Renders a Hickory SVG tree to an HTML string"
  [svg-tree]
  (render/hickory-to-html svg-tree))

;; =============================================================================
;; Core SVG Generation Functions
;; =============================================================================

(defn get-joystick-svg
  "Gets the base SVG for a joystick instance"
  [{:keys [svg-roots joystick-ids]} instance-id]
  (let [short-name (get-in joystick-ids [instance-id :short-name])
        svg-key (some-> short-name keyword)]
    (get svg-roots svg-key)))

(defn update-svg-for-instance
  "Pure function: Returns updated SVG tree for an instance-id.
   Returns nil if no base SVG exists."
  [{:keys [actionmaps config] :as context} instance-id]
  (when-let [svg-root (get-joystick-svg context instance-id)]
    (let [mappings (joystick-action-mappings actionmaps instance-id)
          selector-attr (get-in config [:mapping :svg-generation :data-attribute] :data-for)]
      (update-svg-from-mappings (h/as-hickory svg-root) mappings
                                :selector-attr (keyword selector-attr)))))

(defn update-all-svgs
  "Pure function: Returns map of {instance-id -> updated-svg-tree} for all instances."
  [{:keys [joystick-ids] :as context}]
  (->> (keys joystick-ids)
       (keep (fn [id]
               (when-let [svg (update-svg-for-instance context id)]
                 [id svg])))
       (into {})))

;; =============================================================================
;; Context Management
;; =============================================================================

(defn build-joystick-lookup
  "Creates a map of svg-key -> instance-id for reverse lookups"
  [joystick-ids]
  (into {}
        (keep (fn [[id {:keys [short-name]}]]
                (when short-name
                  [(keyword short-name) id])))
        joystick-ids))

(defn update-svg-roots
  "Updates all SVG roots with mappings and inlined images.
   Pure function - returns new svg-roots map."
  [{:keys [svg-roots joystick-ids actionmaps config] :as context}]
  (let [base-path (System/getProperty "user.dir")
        short->id (build-joystick-lookup joystick-ids)
        selector-attr (get-in config [:mapping :svg-generation :data-attribute] :data-for)]
    (into {}
          (map (fn [[svg-key svg-root]]
                 (let [instance-id (get short->id svg-key)
                       ;; Apply mappings if we have an instance for this SVG
                       mapped-svg (if instance-id
                                    (let [mappings (joystick-action-mappings actionmaps instance-id)]
                                      (update-svg-from-mappings (h/as-hickory svg-root) mappings
                                                                :selector-attr (keyword selector-attr)))
                                    svg-root)
                       ;; Inline images
                       final-svg (svg/fix-all-relative-images-base64 mapped-svg base-path)]
                   [svg-key final-svg])))
          svg-roots)))

(defn update-context
  "Returns context with updated :svg-roots. Pure function."
  [context]
  (assoc context :svg-roots (update-svg-roots context)))

;; =============================================================================
;; File Generation (Side Effects)
;; =============================================================================

(defn svg-output-path
  "Generates the output path for an SVG file"
  [{:keys [config]} svg-name output-dir]
  (let [prefix (get-in config [:mapping :svg-generation :filename-prefix] "")]
    (str output-dir "/" prefix svg-name ".svg")))

(defn write-svg!
  "Writes an SVG tree to a file. Returns the path written or nil on error."
  [svg-tree output-path]
  (try
    (io/make-parents output-path)
    (spit output-path (render-svg svg-tree))
    output-path
    (catch Exception e
      (println (format "Error writing SVG to %s: %s"
                       output-path (.getMessage e)))
      nil)))

(defn generate-svg-for-instance!
  "Generates and writes an updated SVG for a specific joystick instance.
   Returns the output path or nil if generation failed."
  [{:keys [config] :as context} instance-id output-dir]
  (let [info (joystick-info context instance-id)]
    (cond
      (not (:has-svg? info))
      (do (println (format "⚠️ No SVG template for instance %d (%s)"
                           instance-id (:product info)))
          nil)

      (zero? (:mapping-count info))
      (do (println (format "⚠️ No mappings for instance %d (%s)"
                           instance-id (:product info)))
          nil)

      :else
      (if-let [updated-svg (update-svg-for-instance context instance-id)]
        (let [output-path (svg-output-path context (:short-name info) output-dir)]
          (when (write-svg! updated-svg output-path)
            (println (format "✓ Generated: %s (instance %d, %d mappings)"
                             output-path instance-id (:mapping-count info)))
            output-path))
        nil))))

(defn generate-all-svgs!
  "Generates updated SVGs for all known joystick instances.
   Returns vector of successfully written paths."
  ([context]
   (let [default-dir (get-in context [:config :mapping :svg-generation :default-output-dir])]
     (generate-all-svgs! context default-dir)))
  ([{:keys [joystick-ids] :as context} output-dir]
   (println (format "\n📁 Output directory: %s" output-dir))
   (println (format "🎮 Processing %d joystick instances..." (count joystick-ids)))
   (println)
   (let [results (->> (keys joystick-ids)
                      (map #(generate-svg-for-instance! context % output-dir))
                      (filter some?)
                      vec)]
     (println)
     (println (format "📊 Results: %d/%d SVGs generated successfully"
                      (count results) (count joystick-ids)))
     results)))

;; =============================================================================
;; Analysis & Inspection Functions
;; =============================================================================

(defn find-empty-bindings
  "Find all action mappings with empty input bindings.
   Optionally filter by action keyword."
  ([actionmaps]
   (find-empty-bindings actionmaps nil))
  ([actionmaps action-filter]
   (let [all-mappings (extract-input-action-mappings actionmaps)
         empty-mappings (filter #(str/blank? (:input %)) all-mappings)]
     (if action-filter
       (filter #(str/includes? (:action %) (name action-filter))
               empty-mappings)
       empty-mappings))))

(defn joystick-summary
  "Returns a summary of joystick configuration"
  [{:keys [joystick-ids svg-roots actionmaps]}]
  (let [instances (map (fn [[id info]]
                         (let [svg-key (keyword (:short-name info))
                               mappings (joystick-action-mappings actionmaps id)]
                           [id (assoc info
                                      :has-svg? (boolean (get svg-roots svg-key))
                                      :mapping-count (count mappings))]))
                       joystick-ids)]
    {:total-joysticks (count joystick-ids)
     :joysticks-with-svgs (count (filter (fn [[_ info]] (:has-svg? info)) instances))
     :total-mappings (count (extract-input-action-mappings actionmaps))
     :empty-mappings (count (find-empty-bindings actionmaps))
     :instances (into {} instances)}))

;; =============================================================================
;; System Status & Diagnostics
;; =============================================================================

(defn system-status
  "Returns comprehensive system status including discovery info"
  [context]
  (let [discovery-info (discovery/actionmaps-info)
        actionmaps-loaded? (boolean (:actionmaps context))
        summary (when actionmaps-loaded?
                  (joystick-summary context))]
    (merge discovery-info
           {:actionmaps-loaded? actionmaps-loaded?
            :svg-resources-loaded (count (:svg-roots context))
            :context-updated? (boolean (:context-updated? context))}
           (when summary
             {:joystick-summary summary}))))

(defn print-status!
  "Prints formatted system status to console"
  [context]
  (let [status (system-status context)]
    (println "\n╔══════════════════════════════════════╗")
    (println "║     ControlMap System Status        ║")
    (println "╚══════════════════════════════════════╝")
    (println)
    (println "Platform:" (:platform status))
    (println "Actionmaps found:" (:exists? status))
    (println "Actionmaps valid:" (:valid? status))
    (println "Actionmaps path:" (:path status))

    (when (:env-override status)
      (println "Environment override:" (:env-override status)))

    (println "\n── Resources ──")
    (println "Actionmaps loaded:" (:actionmaps-loaded? status))
    (println "SVG resources:" (:svg-resources-loaded status))

    (when-let [summary (:joystick-summary status)]
      (println "\n── Joysticks ──")
      (println "Total configured:" (:total-joysticks summary))
      (println "With SVG templates:" (:joysticks-with-svgs summary))
      (println "Total mappings:" (:total-mappings summary))
      (println "Unmapped actions:" (:empty-mappings summary))

      (println "\n── Instances ──")
      (doseq [[id info] (sort-by key (:instances summary))]
        (let [status-icon (cond
                            (not (:has-svg? info)) "✗"
                            (zero? (:mapping-count info)) "○"
                            :else "✓")]
          (println (format "  [%d] %s %s (%s) - %d mappings"
                           id
                           status-icon
                           (or (:product info) "Unknown")
                           (or (:short-name info) "no-svg")
                           (:mapping-count info))))))

    (when (and (not (:exists? status)) (:searched-paths status))
      (println "\n── Search Paths ──")
      (doseq [path (:searched-paths status)]
        (println "  •" path)))

    (println "\n════════════════════════════════════════\n")))

;; =============================================================================
;; Main Entry Point
;; =============================================================================

(defn initialize-context
  "Initialize or update the context with current data.
   Options:
   - :skip-svgs - Don't load SVG resources
   - :skip-edn - Don't load EDN configs
   - :force-reload - Force reload even if already initialized"
  [& opts]
  (let [options (apply hash-map opts)
        base-context (if (:force-reload options)
                       (apply state/build-context (flatten (seq options)))
                       @state/context)]
    (-> base-context
        (update-context)
        (assoc :context-updated? true))))

(defn -main
  "Main entry point - generates all SVGs with current actionmaps"
  [& args]
  (try
    (println "\n🚀 Star Citizen ControlMap Generator")
    (println "════════════════════════════════════")

    ;; Initialize context
    (let [context (initialize-context)]

      ;; Print status
      (print-status! context)

      ;; Check if we can proceed
      (if (:actionmaps context)
        (do
          ;; Generate SVGs
          (println "\n📝 Starting SVG generation...")
          (let [generated (generate-all-svgs! context)]

            (when (seq generated)
              ;; Generate HTML index if SVGs were created
              (println "\n📄 Generating HTML index...")
              (index/generate-index-with-output-dir!)

              ;; Show final status
              (println "\n📊 Final status:")
              (index/print-svg-status!))

            (println "\n✅ Complete!")
            (System/exit 0)))

        ;; No actionmaps found
        (do
          (println "\n❌ ERROR: Could not load actionmaps!")
          (println "Please check your Star Citizen installation")
          (println "or set SC_ACTIONMAPS_PATH environment variable")
          (System/exit 1))))

    (catch Exception e
      (println "\n❌ Fatal error:" (.getMessage e))
      (.printStackTrace e)
      (System/exit 2))))

;; =============================================================================
;; Development REPL Helpers
;; =============================================================================

(comment
  ;; Quick initialization for REPL
  (def ctx (initialize-context))

  ;; Force reload everything
  (def ctx (initialize-context :force-reload true))

  ;; Skip SVGs for faster testing
  (def ctx (initialize-context :skip-svgs true :force-reload true))

  ;; Status check
  (print-status! ctx)

  ;; Get joystick info
  (joystick-info ctx 5)

  ;; Get summary
  (joystick-summary ctx)

  ;; Find unmapped actions
  (find-empty-bindings (:actionmaps ctx))
  (find-empty-bindings (:actionmaps ctx) :quantum)

  ;; Test action name cleaning
  (clean-action-name "v_toggle_quantum_mode")

  ;; Extract all mappings for a joystick
  (joystick-action-mappings (:actionmaps ctx) 2)

  ;; Update single SVG in memory (no file I/O)
  (update-svg-for-instance ctx 2)

  ;; Update all SVGs in memory
  (def updated-svgs (update-all-svgs ctx))

  ;; Generate single SVG file
  (generate-svg-for-instance! ctx 5 "/tmp")

  ;; Generate all SVG files
  (generate-all-svgs! ctx "/tmp")

  ;; Full test run
  (do
    (def ctx (initialize-context :force-reload true))
    (print-status! ctx)
    (generate-all-svgs! ctx))

  ;; Debug SVG rendering
  (-> (update-svg-for-instance ctx 5)
      render-svg
      (spit "/tmp/debug.svg")))
