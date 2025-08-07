(ns aeonik.controlmap.state
  (:require
   [aeonik.controlmap.discovery :as discovery]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.set :as set]
   [clojure.string :as str]
   [hickory.core :as h]
   [hickory.select :as s]))

;; =============================================================================
;; Loading Functions
;; =============================================================================

(defn load-actionmaps []
  (let [source (or (discovery/find-actionmaps)
                   (io/resource "actionmaps.xml"))]
    (if source
      (do
        (println "Loading actionmaps from:"
                 (if (instance? java.io.File source)
                   (.getAbsolutePath source)
                   (.toString source)))
        (-> source io/reader slurp h/parse h/as-hickory))
      (throw (ex-info "No actionmaps found"
                      {:searched-paths (discovery/get-search-paths)})))))

(defn load-svg-resource [svg-id]
  (try
    (when-let [resource (io/resource (str "svg/" (name svg-id) ".svg"))]
      (-> resource io/reader slurp h/parse h/as-hickory))
    (catch Exception e
      (println (format "Warning: Failed to load SVG '%s': %s"
                       svg-id (.getMessage e)))
      nil)))

(defn load-detected-svgs [svg-ids]
  (into {}
        (keep (fn [svg-id]
                (when-let [svg (load-svg-resource svg-id)]
                  [svg-id svg]))
              svg-ids)))

(defn load-edn-configs [dir-path]
  (let [dir (io/file dir-path)]
    (if (.exists dir)
      (let [edn-files (filter #(str/ends-with? (.getName %) ".edn")
                              (file-seq dir))]
        (into {}
              (keep (fn [file]
                      (try
                        (let [key (keyword (str/replace (.getName file) #"\.edn$" ""))]
                          [key (edn/read-string (slurp file))])
                        (catch Exception e
                          (println "Warning: Failed to load" (.getName file))
                          nil)))
                    edn-files)))
      {})))

;; =============================================================================
;; Extraction Functions
;; =============================================================================

(defn extract-products [actionmaps]
  (let [options (s/select (s/and (s/tag :options)
                                 (s/attr :type #(= % "joystick"))
                                 (s/attr :product))
                          actionmaps)]
    (into {}
          (keep (fn [{:keys [attrs]}]
                  (when-let [instance (some-> (:instance attrs) parse-long)]
                    (when-let [product (:product attrs)]
                      [instance product])))
                options))))

(defn map-to-svgs [registry products]
  (into {}
        (keep (fn [[instance product]]
                (when-let [svg-id (discovery/find-svg-for-product registry product)]
                  [instance svg-id]))
              products)))

;; =============================================================================
;; Context Building
;; =============================================================================

(defn build-context [& {:keys [skip-svgs skip-edn]}]
  (println "\n🔧 Building context...")
  (let [registry (discovery/build-joystick-registry)
        actionmaps (load-actionmaps)
        products (extract-products actionmaps)
        instances (map-to-svgs registry products)
        needed-svgs (set (vals instances))
        svgs (if skip-svgs {} (load-detected-svgs needed-svgs))
        edn-configs (if skip-edn {} (load-edn-configs "resources/config/svg/"))]

    (println (format "✓ Loaded: %d instances, %d SVGs"
                     (count instances) (count svgs)))

    {:registry registry
     :instances instances
     :products products
     :svgs svgs
     :edn-configs edn-configs
     :actionmaps actionmaps
     :config (discovery/get-config)}))

(defn refresh! [context]
  (let [actionmaps (load-actionmaps)
        products (extract-products actionmaps)
        instances (map-to-svgs (:registry context) products)
        new-svgs (set/difference (set (vals instances))
                                 (set (keys (:svgs context))))]
    (cond-> context
      true (assoc :actionmaps actionmaps
                  :products products
                  :instances instances)
      (seq new-svgs) (update :svgs merge (load-detected-svgs new-svgs)))))

;; =============================================================================
;; State Management
;; =============================================================================

(defonce ^:dynamic *context* (atom nil))

(defn init! [& opts]
  (reset! *context* (apply build-context opts)))

(defn get-context []
  (or @*context* (init!)))

(comment
  ;; Initialize full context
  (init!)

  ;; Skip loading SVGs for faster startup
  (init! :skip-svgs true)

  ;; Force reload even if context is already initialized
  (reset! *context* (build-context :force-reload true))

  ;; Get current context
  (def c (get-context))

  ;; Explore loaded actionmaps
  (:actionmaps c)

  ;; See extracted joystick products
  (:products c)

  ;; SVGs that were detected and loaded
  (keys (:svgs c))

  ;; Config from resources/config/svg/*.edn
  (:edn-configs c))
