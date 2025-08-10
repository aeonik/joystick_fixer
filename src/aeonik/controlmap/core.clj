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
   [hickory.render :as render]
   [hickory.select :as s]))

;; =============================================================================
;; Action Name Cleaning
;; =============================================================================

(defn clean-action-name
  "Removes common prefixes and arbitrary regex patterns from action names"
  [action-name]
  (when (some? action-name)
    (let [config (discovery/get-config)
          {:keys [remove-v-prefix prefix-filters regex-filters]}
          (-> config :mapping :action-name-cleaning)

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
              (or regex-filters [])))))

;; =============================================================================
;; Action Mapping Extraction
;; =============================================================================

(defn extract-all-mappings
  "Returns a map from input → vector of actions, e.g. {\"js1_button6\" [\"v_toggle_missile_mode\"] ...}"
  [actionmaps]
  (->> actionmaps
       (s/select (s/tag :action))
       (map (fn [action-node]
              (let [action-name (get-in action-node [:attrs :name])
                    rebind-node (first (s/select (s/tag :rebind) action-node))
                    input (get-in rebind-node [:attrs :input])]
                {:input input :action action-name})))
       (remove #(str/blank? (:input %)))
       (reduce (fn [m {:keys [input action]}]
                 (update m input (fnil conj []) action))
               {})))

(defn instance-mappings
  "For `instance-id` N, return {\"buttonXX\" [actions …]} drawn from
   keys that start with \"jsN_\"."
  [actionmaps instance-id]
  (let [prefix (str "js" instance-id "_")
        xf     (comp
                (filter (fn [[k _]] (str/starts-with? k prefix))) ; keep only this stick
                (map    (fn [[k v]] [(subs k (count prefix)) v])))] ; strip prefix
    (into {} xf (extract-all-mappings actionmaps))))

;; =============================================================================
;; Display Names
;; =============================================================================

(defn svg-id->display-name
  "Gets display name for an svg-id"
  [context svg-id]
  (or (get-in context [:registry :displays svg-id])
      ;; Fallback: use first pattern
      (when-let [patterns (get-in context [:registry :by-svg svg-id])]
        (-> (str (first patterns))
            (str/replace #"^\(\?i\)" "")))))

(defn instance->display-name
  "Gets display name for an instance"
  [context instance-id]
  (when-let [svg-id (get-in context [:instances instance-id])]
    (svg-id->display-name context svg-id)))

;; =============================================================================
;; SVG Update Functions
;; =============================================================================

(defn format-button-actions
  "Formats multiple actions for a button"
  [actions & {:keys [format-fn separator max-actions]
              :or {format-fn clean-action-name
                   separator " / "
                   max-actions 8}}]
  (let [cleaned (map format-fn actions)
        n (count actions)]
    (cond
      (= n 0) ""
      (= n 1) (first cleaned)
      (> n max-actions) (str (str/join separator (take max-actions cleaned)) " ...")
      :else (str/join separator cleaned))))

(defn update-svg-with-mappings
  "Updates an SVG tree so each mapped button gets one <tspan> per action."
  [svg-tree mappings & {:keys [selector-attr format-fn]
                        :or {selector-attr :data-for
                             format-fn      clean-action-name}}]

  (reduce-kv
   (fn [tree button-id actions]
     (svg/update-nodes
      tree
      (s/attr selector-attr #(= % button-id))
      (svg/compose-edits
       ;; vector in → multi-tspan out
       (svg/make-content-updater (mapv format-fn actions))
       (svg/make-attr-updater
        {:data-actions (str/join ";" actions)
         :data-count   (str (count actions))}))
      :first-only? true))
   svg-tree
   mappings))

;; =============================================================================
;; Core Generation Functions
;; =============================================================================

(defn get-svg-for-instance
  "Gets the SVG tree for an instance"
  [context instance-id]
  (when-let [svg-id (get-in context [:instances instance-id])]
    (get-in context [:svgs svg-id])))

(defn update-svg-for-instance
  "Updates SVG for a specific instance with its mappings"
  [context instance-id]
  (when-let [svg-tree (get-svg-for-instance context instance-id)]
    (let [mappings (instance-mappings (:actionmaps context) instance-id)
          selector (get-in context [:config :mapping :svg-generation :data-attribute] :data-for)]
      (update-svg-with-mappings svg-tree mappings
                                :selector-attr (keyword selector)))))

(defn update-all-svgs
  "Updates all SVGs with their mappings"
  [context]
  (into {}
        (keep (fn [[instance-id joystick-id]]
                (when-let [updated (update-svg-for-instance context instance-id)]
                  [joystick-id updated]))
              (:instances context))))

;; =============================================================================
;; File I/O
;; =============================================================================

(defn write-svg!
  "Writes an SVG tree to file"
  [svg-tree path]
  (try
    (io/make-parents path)
    (spit path (render/hickory-to-html svg-tree))
    path
    (catch Exception e
      (println (format "Error writing %s: %s" path (.getMessage e)))
      nil)))

(defn generate-svg!
  "Generates and writes SVG for an instance"
  [context instance-id output-dir]
  (if-let [svg-id (get-in context [:instances instance-id])]
    (if-let [updated-svg (update-svg-for-instance context instance-id)]
      (let [prefix (get-in context [:config :mapping :svg-generation :filename-prefix] "")
            path (str output-dir "/" prefix (name svg-id) ".svg")]
        (when (write-svg! updated-svg path)
          (println (format "✓ Generated %s for instance %d" path instance-id))
          path))
      (println (format "⚠️ No SVG template for instance %d" instance-id)))
    (println (format "⚠️ Instance %d not mapped to any SVG" instance-id))))

(defn generate-all-svgs!
  "Generates all SVGs"
  ([context]
   (let [output-dir (get-in context [:config :mapping :svg-generation :default-output-dir])]
     (generate-all-svgs! context output-dir)))
  ([context output-dir]
   (println (format "\n📁 Output: %s" output-dir))
   (println (format "🎮 Processing %d instances..." (count (:instances context))))
   (let [results (keep #(generate-svg! context % output-dir)
                       (keys (:instances context)))]
     (println (format "✅ Generated %d SVGs" (count results)))
     (vec results))))

;; =============================================================================
;; Analysis Functions
;; =============================================================================

(def unmapped-key-re #"^\s*(?:[a-z]{2}\d+_\s?)?$")

(defn find-unmapped-actions
  "Flatten all actions whose input key is blank, nil, or a bare
   two-letter device prefix (js/kb/…) followed only by digits and “_”."
  [actionmaps]
  (into []
        (comp (filter (fn [[k _]]
                        (re-matches unmapped-key-re (str (or k "")))))
              (mapcat val))          ; concat each action-vector into the output
        (extract-all-mappings actionmaps)))

(defn analyze-instance
  "Analyzes a single instance"
  [context instance-id]
  (let [svg-id (get-in context [:instances instance-id])
        product (get-in context [:products instance-id])
        mappings (instance-mappings (:actionmaps context) instance-id)]
    {:instance-id instance-id
     :svg-id svg-id
     :product product
     :has-svg? (boolean (and svg-id (get-in context [:svgs svg-id])))
     :mapping-count (count mappings)
     :multi-binds (count (filter #(> (count (val %)) 1) mappings))}))

(defn summary
  "Returns system summary"
  [context]
  (let [analyses (map #(analyze-instance context %)
                      (keys (:instances context)))]
    {:total-instances (count (:instances context))
     :mapped-instances (count (filter :svg-id analyses))
     :loaded-svgs (count (:svgs context))
     :total-mappings (count (extract-all-mappings (:actionmaps context)))
     :unmapped-actions (count (find-unmapped-actions (:actionmaps context)))
     :instances analyses}))

;; =============================================================================
;; Status Display
;; =============================================================================

(defn print-status!
  "Prints system status"
  [context]
  (let [s (summary context)]
    (println "\n╔══════════════════════════════════════╗")
    (println "║     ControlMap System Status        ║")
    (println "╚══════════════════════════════════════╝")
    (println)
    (println "Instances detected:" (:total-instances s))
    (println "Instances mapped:" (:mapped-instances s))
    (println "SVGs loaded:" (:loaded-svgs s))
    (println "Total mappings:" (:total-mappings s))
    (println "Unmapped actions:" (:unmapped-actions s))
    (println "\n── Instance Details ──")
    (doseq [{:keys [instance-id svg-id product has-svg? mapping-count]}
            (sort-by :instance-id (:instances s))]
      (let [icon (cond
                   (not svg-id) "❌"
                   (not has-svg?) "⚠️"
                   (zero? mapping-count) "○"
                   :else "✓")]
        (println (format "  [%d] %s %s (%s) - %d mappings"
                         instance-id icon
                         (or (name svg-id) "unmapped")
                         (or product "unknown")
                         mapping-count))))))

;; =============================================================================
;; Main Entry Point
;; =============================================================================

(defn -main
  [& args]
  (try
    (println "\n🚀 Star Citizen ControlMap Generator")
    (println "════════════════════════════════════")

    (let [context (state/build-context)]
      (print-status! context)

      (if (:actionmaps context)
        (do
          (println "\n📝 Generating SVGs...")
          (let [generated (generate-all-svgs! context)]
            (when (seq generated)
              (println "\n📄 Generating index...")
              (index/generate-index-with-output-dir!))
            (println "\n✅ Complete!")
            (System/exit 0)))
        (do
          (println "\n❌ No actionmaps found!")
          (System/exit 1))))

    (catch Exception e
      (println "\n❌ Fatal error:" (.getMessage e))
      (.printStackTrace e)
      (System/exit 2))))

;; =============================================================================
;; REPL Helpers
;; =============================================================================

(comment
  ;; Initialize
  (def ctx (state/build-context))

  ;; Check what we have
  (:instances ctx)
  ;; => {0 :alpha_RP, 1 :vpc_mongoose_t50cm3, ...}

  ;; Get display name
  (svg-id->display-name ctx :alpha_RP)
  (instance->display-name ctx 1)

  ;; Analyze
  (analyze-instance ctx 1)

  (summary ctx)
  (print-status! ctx)

  (find-unmapped-actions  (:actionmaps ctx))

  (extract-all-mappings (:actionmaps ctx))

  (def mappings (instance-mappings (:actionmaps ctx) 1))

  (update-svg-for-instance ctx 1)

  ;; Generate
  (generate-svg! ctx 1 "/tmp")
  (generate-all-svgs! ctx "/tmp")

  ;; Test mappings
  (def mappings (instance-mappings (:actionmaps ctx) 1))

  ;; Update SVG in memory
  (def updated (update-svg-for-instance ctx 1)))
