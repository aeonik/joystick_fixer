(ns aeonik.controlmap.core
  (:gen-class)
  (:require
   [aeonik.controlmap.discovery :as discovery]
   [aeonik.controlmap.index :as index]
   [aeonik.controlmap.state :as state :refer [context]]
   [aeonik.controlmap.svg :as svg]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [net.cgrand.enlive-html :as html]
   [hickory.select :as s]
   [hickory.core :as h]
   [tupelo.forest :as f]))

;; =============================================================================
;; Configuration and Data Loading
;; =============================================================================

(def ^:private config
  "Application configuration loaded from config.edn"
  (-> "config.edn"
      io/resource
      slurp
      edn/read-string))

;; =============================================================================
;; Action Name Cleaning
;; =============================================================================

(defn clean-action-name
  "Removes common prefixes and arbitrary regex patterns from action names for cleaner display"
  [action-name]
  (let [cleaning-config (-> config :mapping :action-name-cleaning)
        {:keys [remove-v-prefix prefix-filters regex-filters]} cleaning-config
        filtered-name (if remove-v-prefix
                        (str/replace action-name #"^v_" "")
                        action-name)
        prefix-cleaned (reduce
                        (fn [acc prefix]
                          (if (str/starts-with? acc prefix)
                            (subs acc (count prefix))
                            acc))
                        filtered-name
                        prefix-filters)]
    (reduce
     (fn [acc regex-string]
       (str/replace acc (re-pattern regex-string) ""))
     prefix-cleaned
     (or regex-filters []))))

(defn extract-input-action-mappings
  "Extracts input-action mappings from the actionmaps by traversing the tree structure
   and fetching the corresponding input and action name for each rebind path.

  I use this to get unmapped actions "
  [actionmaps]
  (->> (h/as-hickory actionmaps)
       (s/select (s/tag :action))
       (map (fn [path]
              {:input  (-> (s/select (s/tag :rebind) path) first :attrs :input)
               :action (-> path :attrs :name)}))
       (into [])))

(s/select (s/tag :action)
          (h/as-hickory (state/load-actionmaps)))

(comment (extract-input-action-mappings (state/load-actionmaps)))

(defn find-joystick-bindings
  "Returns enlive structures containing all action maps for a specific joystick"
  [actionmaps js-num]
  (let [prefix (str "js" js-num "_")]
    (f/with-forest (f/new-forest)
      (map f/bush->enlive
           (-> actionmaps
               (f/add-tree-enlive)
               (f/find-paths-with [:** {:input :*}]
                                  #(str/starts-with? (f/hid->attr (last %) :input) prefix))
               (f/format-paths))))))

(defn find-joystick-bindings-hickory*
  [input-actions* js-num]
  (let [prefix (str "js" js-num "_")
        input-actions-filtered (filter #(str/starts-with? (:input %) prefix) input-actions*)]
    input-actions-filtered))

(comment
  (find-joystick-bindings (state/load-actionmaps-legacy) 4)

  (find-joystick-bindings-hickory* (extract-input-action-mappings state/actionmaps) 4))

(comment
  (->> (find-joystick-bindings state/actionmaps 5)
       (map #(html/select % [:actionmap])))

  (->> (find-joystick-bindings state/actionmaps 5)
       (map #(html/select % [[:action (html/attr? :name)]])))

  (->> (find-joystick-bindings state/actionmaps 5)
       (mapcat #(html/select % [[:action (html/attr? :name)]]))
       (map #(html/attr-values % :name)))

  (->> (find-joystick-bindings state/actionmaps 5)
       (mapcat #(html/select % [[:action (html/attr? :name)]]))
       (into {} (map (fn [action]
                       [(get-in action [:attrs :name])
                        (-> action
                            (html/select [:rebind])
                            first
                            (get-in [:attrs :input]))])))))

(defn joystick-action-mappings
  "Returns action mappings for a specific joystick with SVG button references"
  [actionmaps joystick-num]
  (->> (find-joystick-bindings actionmaps joystick-num)
       (mapcat #(html/select % [[:action (html/attr? :name)]]))
       (keep (fn [action]
               (let [input (-> action
                               (html/select [:rebind])
                               first
                               (get-in [:attrs :input]))
                     stripped_input (str/replace input #"^js\d+_" "")]
                 (when input
                   {:action (get-in action [:attrs :name])
                    :input input
                    :svg-input stripped_input}))))))

(defn joystick-action-mappings
  "Returns action mappings for a specific joystick with SVG button references"
  [actionmaps joystick-num]
  (->> (find-joystick-bindings actionmaps joystick-num)
       (mapcat #(html/select % [[:action (html/attr? :name)]]))
       (keep (fn [action]
               (let [input (-> action
                               (html/select [:rebind])
                               first
                               (get-in [:attrs :input]))
                     stripped_input (str/replace input #"^js\d+_" "")]
                 (when input
                   {:action (get-in action [:attrs :name])
                    :input input
                    :svg-input stripped_input}))))))

(comment (joystick-action-mappings (state/load-actionmaps-legacy) 4))

(defn joystick-info
  [context instance-id]
  (let [{:keys [joystick-ids
                svg-roots
                svg-edn-files
                actionmaps]} context
        {:keys [short-name product match-regex]} (joystick-ids instance-id)
        svg-key   (some-> short-name keyword)
        svg-root  (svg-roots svg-key)
        svg-edn   (svg-edn-files svg-key)
        mappings  (joystick-action-mappings actionmaps instance-id)]
    {:instance-id instance-id
     :short-name  short-name
     :product     product
     :match-regex match-regex
     :svg-key     svg-key
     :svg-root    svg-root ;; legacy
     :svg-edn     svg-edn  ;; current, hiccup-based
     :mappings    mappings}))

(comment (joystick-info state/context 5))

;; =============================================================================
;; SVG Generation
;; =============================================================================

(defn update-svg
  [{:keys [svg-root mappings svg-config]}]
  (let [data-attr (:data-attribute svg-config)]
    (reduce
     (fn [svg-doc {:keys [svg-input action]}]
       (if svg-input
         (html/at svg-doc
                  [[:text (html/attr= (keyword data-attr) svg-input)]]
                  (html/content (clean-action-name action)))
         svg-doc))
     svg-root
     mappings)))

(defn render-svg [svg]
  (apply str (html/emit* svg)))

(comment
  (spit "/tmp/debug.svg" (-> context
                             (joystick-info 5)
                             update-svg
                             render-svg)))

(defn update-svg-with-mappings
  "Updates an SVG with action mappings for a specific joystick"
  [svg actionmaps joystick-num]
  (let [mappings (joystick-action-mappings actionmaps joystick-num)
        svg-config (-> config :mapping :svg-generation)
        data-attr (:data-attribute svg-config)]
    (reduce (fn [svg-doc {:keys [svg-input action]}]
              (if svg-input
                (html/at svg-doc
                         [[:text (html/attr= (keyword data-attr) svg-input)]]
                         (html/content (clean-action-name action)))
                svg-doc))
            svg
            mappings)))

(defn update-svg-for-instance
  "Return updated SVG for an instance-id, or nil if no base svg exists."
  [context instance-id]
  (let [{:keys [svg-roots joystick-ids actionmaps]} context
        {:keys [short-name]} (joystick-ids instance-id)
        svg-key  (some-> short-name keyword)
        svg-root (get svg-roots svg-key)]
    (when svg-root
      (update-svg-with-mappings svg-root actionmaps instance-id))))

(defn update-all-svgs-in-memory
  "Returns {instance-id -> updated-svg-tree} for all instances."
  [context]
  (let [ids (keys (:joystick-ids context))]
    (->> ids
         (map (fn [id]
                (when-let [svg (update-svg-for-instance context id)]
                  [id svg])))
         (filter some?)                ;; remove nils
         (into {}))))

(comment (update-all-svgs-in-memory state/context))

(comment

  (let [joystick-id 5
        filename (:short-name (state/joystick-ids joystick-id))
        svg-root (state/svg-roots (keyword filename))]
    (update-svg-with-mappings svg-root state/actionmaps joystick-id)))

;; TODO: ChatGPT dreck here, need to use my old functions for the mapping that I made
(defn update-context
  "Update all SVG roots in-place: apply joystick mappings (when available),
   then inline relative <image> hrefs as base64. Returns context with :svg-roots replaced."
  [context]
  (let [{:keys [svg-roots joystick-ids actionmaps]} context
        base-path (System/getProperty "user.dir")
        short->id (into {}
                        (map (fn [[id {:keys [short-name]}]]
                               [(some-> short-name keyword) id]))
                        joystick-ids)
        updated-roots
        (into {}
              (map (fn [[k svg]]
                     (let [maybe-id (get short->id k)
                           mapped   (if maybe-id
                                      (update-svg-with-mappings svg actionmaps maybe-id)

                                      svg)
                           inlined  (svg/fix-all-relative-images-base64 mapped base-path)]
                       [k inlined])))
              svg-roots)]
    (-> context
        (assoc :svg-roots updated-roots))))

(defn generate-svg-for-instance!
  "Generates an updated SVG for a specific joystick instance"
  [context instance-id output-dir]
  (let [{:keys [svg-roots svg-config joystick-ids]} context
        {:keys [short-name]} (joystick-ids instance-id)
        svg-key (some-> short-name keyword)
        svg-root (svg-roots svg-key)]
    (when svg-root
      (let [info (merge (joystick-info context instance-id)
                        {:svg-root svg-root
                         :svg-config svg-config})
            updated-svg (update-svg info)
            filename    (str (name svg-key) ".svg")
            prefix      (:filename-prefix svg-config)
            output-path (str output-dir "/" prefix filename)]
        (io/make-parents output-path)
        (spit output-path (render-svg updated-svg))
        (println "Generated:" output-path "for instance" instance-id)
        output-path))))

(comment (generate-svg-for-instance! state/context 5 (-> state/context :svg-config :default-output-dir)))

(defn generate-all-svgs!
  "Generates updated SVGs for all known joystick instances"
  ([context]
   (let [default-dir (get-in context [:svg-config :default-output-dir])]
     (generate-all-svgs! context default-dir)))
  ([context output-dir]
   (let [joystick-ids (:joystick-ids context)]
     (->> joystick-ids
          (map (fn [[instance-id _]]
                 (generate-svg-for-instance! context instance-id output-dir)))
          (remove nil?)
          (into [])))))

(comment (generate-all-svgs! state/context))

(comment
  (generate-all-svgs! state/actionmaps)

  (def updated-svg (update-svg-with-mappings svg state/actionmaps 4))

  (->> updated-svg
       html/emit*
       (apply str)
       (spit "updated-panel.svg"))

  (let [instance 4
        svg-location (instance->svg instance)
        svg (get svg-roots svg-location)
        updated-svg (update-svg-with-mappings svg actionmaps instance)]
    (->> updated-svg
         html/emit*
         (apply str)
         (spit "updated-panel.svg"))))

;; =============================================================================
;; Discovery Integration & Status
;; =============================================================================

(defn empty-input-bindings
  ([actionmaps]
   (filter #(clojure.string/blank? (:input %))
           (extract-input-action-mappings actionmaps)))

  ([actionmaps kw]
   (->> (extract-input-action-mappings actionmaps)
        (filter #(clojure.string/blank? (:input %)))
        (filter #(clojure.string/includes? (:action %) (name kw))))))

(defn system-status
  "Returns comprehensive system status including discovery info"
  []
  (let [discovery-info (discovery/actionmaps-info)
        actionmaps-loaded? (try
                             (some? state/actionmaps)
                             (catch Exception _ false))]
    (merge discovery-info
           {:actionmaps-loadable? actionmaps-loaded?
            :svg-resources-loaded (count state/svg-roots)
            :available-instances (keys (state/joystick-ids state/actionmaps))})))

(defn print-status!
  "Prints current system status to console"
  []
  (let [status (system-status)]
    (println "\n=== ControlMap System Status ===")
    (println "Platform:" (:platform status))
    (println "Actionmaps found:" (:exists? status))
    (println "Actionmaps valid:" (:valid? status))
    (println "Actionmaps path:" (:path status))
    (if (:env-override status)
      (println "Environment override:" (:env-override status))
      (println "Using discovery search"))
    (println "Actionmaps loadable:" (:actionmaps-loadable? status))
    (println "SVG resources loaded:" (:svg-resources-loaded status))
    (println "Available instances:" (:available-instances status))
    (when-not (:exists? status)
      (println "\nSearched paths:")
      (doseq [path (:searched-paths status)]
        (println "  -" path)))
    (println "================================\n")))

;; =============================================================================
;; Main Entry Point
;; =============================================================================

(defn -main
  "Main entry point - generates all SVGs with current actionmaps"
  [& args]
  (print-status!)
  (if-let [actionmaps state/actionmaps]
    (do
      (println "Generating SVGs...")
      (let [generated (generate-all-svgs! state/context)]
        (println "Successfully generated" (count generated) "SVG files")

        ;; Generate HTML index
        (println "Generating HTML index...")
        (index/generate-index-with-output-dir!)

        ;; Show final status
        (index/print-svg-status!)))
    (do
      (println "ERROR: Could not load actionmaps!")
      (println "Please check your Star Citizen installation or set SC_ACTIONMAPS_PATH")
      (System/exit 1))))
;; =============================================================================
;; Development Helpers
;; =============================================================================

(comment
  ;; Quick status check
  (print-status!)

  ;; Load and inspect actionmaps
  (def actionmaps state/actionmaps)

  ;; Find joystick mappings
  (find-joystick-ids state/actionmaps)

  ;; Get mappings for specific joystick
  (joystick-action-mappings state/actionmaps 1)

  ;; Generate single SVG
  (generate-svg-for-instance state/actionmaps 4 "svg/panel_3.svg" "out")

  ;; Generate all SVGs
  (generate-all-svgs! state/actionmaps)

  ;; Test discovery
  (discovery/actionmaps-info))
