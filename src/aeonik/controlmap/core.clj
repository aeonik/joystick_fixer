(ns aeonik.controlmap.core
  (:gen-class)
  (:require
   [aeonik.controlmap.discovery :as discovery]
   [aeonik.controlmap.index :as index]
   [aeonik.controlmap.state :as state]
   [clojure.data.xml :as xml]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [net.cgrand.enlive-html :as html]
   [riveted.core :as vtd]
   [tupelo.forest :as f]
   [tupelo.parse.xml :as tx]))

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
  "Removes common prefixes from action names for cleaner display"
  [action-name]
  (let [cleaning-config (-> config :mapping :action-name-cleaning)
        {:keys [remove-v-prefix prefix-filters]} cleaning-config
        filtered_name (if remove-v-prefix
                        (str/replace action-name #"^v_" "")
                        action-name)]
    (reduce
     (fn [acc prefix]
       (if (str/starts-with? acc prefix)
         (subs acc (count prefix))
         acc))
     filtered_name
     prefix-filters)))

(comment
  (def actionmaps state/actionmaps)

  (def xml-resource (html/xml-resource (io/resource "actionmaps.xml")))
  (def nav (vtd/navigator (slurp (io/resource "actionmaps.xml"))))
  (def parsed-xml (xml/parse (io/reader (io/resource "actionmaps.xml")))))

(comment
  (def svg (-> "svg/panel_3.svg"
               io/resource
               io/reader
               tx/parse-streaming))

  (def instance->svg
    {1 "svg/alpha_L.svg"
     3 "svg/alpha_R.svg"
     4 "svg/panel_3.svg"
     5 "svg/vpc_mongoose_t50cm3.svg"
     6 "svg/panel_1.svg"
     7 "svg/panel_2.svg"})

  (def product->svg
    {#"L-VPC Stick MT-50CM2"     "svg/alpha_L.svg"
     #"R-VPC Stick MT-50CM2"     "svg/alpha_R.svg"
     #"VPC SharKa-50 Panel"      "svg/panel_3.svg"
     #"VPC Panel #1"             "svg/panel_1.svg"
     #"VPC Panel #2"             "svg/panel_2.svg"
     #"MongoosT-50CM3"           "svg/vpc_mongoose_t50cm3.svg"})

  (def svg-roots
    (into {}
          (map (fn [[_ fname]]
                 (if-let [resource (io/resource fname)]
                   [fname (-> resource
                              io/reader
                              tx/parse-streaming)]
                   (do
                     (println "Resource not found:" fname)
                     [fname nil])))
               instance->svg))))

(comment
  (->> nav
       (vtd/select :rebind)
       (map #(vtd/attr % :input)))

  (map #(vtd/attr  % :input)
       (-> nav
           (vtd/select :rebind)))

  (map (comp vtd/fragment vtd/parent) (-> nav
                                          (vtd/select :action)))

  (map (comp vtd/text vtd/parent) (-> nav
                                      (vtd/select :action))))

(comment

  (f/with-forest (f/new-forest)
    (-> state/actionmaps
        (f/add-tree-enlive)
        (f/find-paths [:** :rebind])
        f/format-paths))

  (f/with-forest (f/new-forest)
    (-> state/actionmaps
        (f/add-tree-enlive)
        (f/find-paths [:** {:input "js4_ "}])
        f/format-paths))

  (f/with-forest (f/new-forest)
    (-> state/actionmaps
        (f/add-tree-enlive)
        (f/find-paths-with [:** {:input :*}]
                           #(str/starts-with? (f/hid->attr (last %) :input) "js5_"))
        f/format-paths)))

(defn ^:deprecated extract-input-action-mappings
  "Extracts input-action mappings from the actionmaps by traversing the tree structure
   and fetching the corresponding input and action name for each rebind path.

   Returns a vector of maps containing input and action pairs. Probably not going to use this

  Use find-joystick-bindings instead, per joystick"
  [actionmaps]
  (f/with-forest (f/new-forest)
    (-> actionmaps
        f/add-tree-enlive
        (f/find-paths [:** :rebind])
        (->>
         (map (fn [path]
                (let [child  (f/hid->node (peek path))
                      parent (f/hid->node (peek (pop path)))]
                  {:input  (:input child)
                   :action (:name  parent)})))
         (into [])))))

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
                            (get-in [:attrs :input]))]))))

  (-> xml-resource
      (html/select [:action])))

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

(comment
  (joystick-action-mappings state/actionmaps 5)

  (html/select svg [[:text (html/attr= :data-for "btn_27")]])

  (html/at svg
           [[:text (html/attr= :data-for "btn_27")]]
           (html/content "test"))

  (let [mappings (joystick-action-mappings state/actionmaps 1)]
    (reduce (fn [svg-doc {:keys [svg-input action]}]
              (if svg-input
                (html/at svg-doc
                         [[:text (html/attr= :data-for svg-input)]]
                         (html/content action))
                svg-doc))
            svg
            mappings))

  (->> (let [mappings (joystick-action-mappings state/actionmaps 1)]
         (reduce (fn [svg-doc {:keys [svg-input action]}]
                   (if svg-input
                     (html/at svg-doc
                              [[:text (html/attr= :data-for svg-input)]]
                              (html/content action))
                     svg-doc))
                 svg
                 mappings))
       html/emit*
       (apply str)
       (spit "output.svg")))

;; =============================================================================
;; SVG Generation
;; =============================================================================

;; TODO: need to fix the logic here, I stopped using the btn{button} mapping
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

(comment
  (let [joystick-id 5
        filename (state/joystick-ids joystick-id)
        svg-root (state/svg-roots filename)]
    (update-svg-with-mappings svg-root state/actionmaps joystick-id)))

(defn generate-svg-for-instance
  "Generates an updated SVG for a specific joystick instance"
  [actionmaps instance svg-location output-dir]
  (when-let [svg (get state/svg-roots svg-location)]
    (let [updated-svg (update-svg-with-mappings svg actionmaps instance)
          filename (last (str/split svg-location #"/"))
          svg-config (get-in config [:mapping :svg-generation])
          prefix (:filename-prefix svg-config)
          output-path (str output-dir "/" prefix filename)]
      (io/make-parents output-path)
      (->> updated-svg
           html/emit*
           (apply str)
           (spit output-path))
      (println "Generated:" output-path "for instance" instance)
      output-path)))

(defn generate-all-svgs!
  "Generates updated SVGs for all known joystick instances"
  ([actionmaps]
   (let [default-dir (-> config :mapping :svg-generation :default-output-dir)]
     (generate-all-svgs! actionmaps default-dir)))
  ([actionmaps output-dir]
   (let [instance-mapping state/joystick-ids]
     (->> instance-mapping
          (keep (fn [[instance svg-location]]
                  (generate-svg-for-instance actionmaps instance svg-location output-dir)))
          (into [])))))

(comment (def updated-svg (update-svg-with-mappings svg state/actionmaps 4))

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
      (let [generated (generate-all-svgs! actionmaps)]
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
