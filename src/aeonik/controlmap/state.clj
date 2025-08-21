(ns aeonik.controlmap.state
  (:require
   [babashka.fs :as fs]
   [aeonik.controlmap.discovery :as discovery]
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [hickory.core :as h]
   [hickory.select :as s]))

;; =============================================================================
;; Core Data Loading
;; =============================================================================

(defn- pathlike? [x]
  (or (string? x) (instance? java.io.File x) (instance? java.nio.file.Path x)))

(defn- pretty-source [source]
  (cond
    (instance? java.net.URL source) (str source)
    (pathlike? source) (-> (fs/path source) fs/absolutize str)
    :else (str source)))

(defn resolve-actionmaps-source
  "Resolves actionmaps source in priority order: custom-path -> discovery -> resource"
  [custom-path]
  (or (when (some? custom-path)
        (fs/path custom-path))
      (discovery/find-actionmaps)                  ;; likely a java.io.File
      (io/resource "actionmaps.xml")))             ;; URL

(defn load-actionmaps-from-source
  "Loads and parses actionmaps from a source (Path/File/string/URL)."
  [source]
  (when source
    (try
      (println "Loading actionmaps from:" (pretty-source source))
      (let [content (cond
                      (instance? java.net.URL source)
                      (slurp (io/reader ^java.net.URL source))

                      (pathlike? source)
                      (slurp (fs/file source))

                      :else
                      (slurp source))]
        (-> content h/parse h/as-hickory))
      (catch Exception e
        (throw (ex-info (str "Failed to load actionmaps: " (.getMessage e))
                        {:source source :error e}))))))

(defn load-actionmaps
  "Public interface for loading actionmaps"
  ([] (load-actionmaps nil))
  ([custom-path]
   (if-let [source (resolve-actionmaps-source custom-path)]
     (load-actionmaps-from-source source)
     (throw (ex-info "No actionmaps found"
                     {:searched-paths (discovery/get-search-paths)})))))

(defn load-svg-resource [svg-id]
  (try
    (when-let [res (io/resource (str "svg/" (name svg-id) ".svg"))]
      (-> (slurp (io/reader res)) h/parse h/as-hickory))
    (catch Exception e
      (println (format "Warning: Failed to load SVG '%s': %s"
                       svg-id (.getMessage e)))
      nil)))

(defn load-detected-svgs [svg-ids]
  (into {}
        (keep (fn [svg-id]
                (when-let [svg (load-svg-resource svg-id)]
                  [svg-id svg])))
        svg-ids))

(defn load-edn-configs [dir-path]
  (let [dir (fs/path dir-path)]
    (when (and (fs/exists? dir) (fs/directory? dir))
      (into {}
            (keep (fn [p]
                    (try
                      (let [fname (str (fs/file-name p))
                            k     (-> fname (str/replace #"\.edn$" "") keyword)]
                        [k (edn/read-string (slurp (fs/file p)))])
                      (catch Exception e
                        (println "Warning: Failed to load" (str p) "-" (.getMessage e))
                        nil))))
            (fs/glob dir "*.edn")))))

;; =============================================================================
;; Data Extraction
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
                      [instance product]))))
          options)))

(defn map-to-svgs [registry products]
  (into {}
        (keep (fn [[instance product]]
                (when-let [svg-id (discovery/find-svg-for-product registry product)]
                  [instance svg-id])))
        products))

;; =============================================================================
;; Context Building
;; =============================================================================

(defn build-base-context
  "Builds context from actionmaps and an opts map. Pure function."
  [actionmaps actionmaps-source {:keys [skip-svgs skip-edn] :as opts}]
  (let [registry    (discovery/build-joystick-registry)
        products    (extract-products actionmaps)
        instances   (map-to-svgs registry products)
        needed-svgs (set (vals instances))
        svgs        (if skip-svgs {} (load-detected-svgs needed-svgs))
        edn-configs (if skip-edn {} (load-edn-configs "resources/config/svg/"))]
    {:registry          registry
     :instances         instances
     :products          products
     :svgs              svgs
     :edn-configs       edn-configs
     :actionmaps        actionmaps
     :actionmaps-source actionmaps-source     ;; for reload
     :build-opts        opts                  ;; preserve opts for reload/change
     :config            (discovery/get-config)}))

(defn build-context
  "Main context builder that loads actionmaps and builds context."
  [& {:keys [skip-svgs skip-edn actionmaps-path] :as opts}]
  (println "\n🔧 Building context...")
  (let [source     (resolve-actionmaps-source actionmaps-path)
        actionmaps (load-actionmaps-from-source source)
        context    (build-base-context actionmaps source opts)]
    (println (format "✅ Loaded: %d instances, %d SVGs"
                     (count (:instances context))
                     (count (:svgs context))))
    context))

;; =============================================================================
;; State Management (Optional global for CLI / legacy)
;; =============================================================================

(defonce ^:dynamic *context* (atom nil))

(defn init!
  "Initializes global context (discouraged for GUI; fine for CLI/tests)."
  [& {:as opts}]
  (reset! *context* (apply build-context (mapcat identity opts))))

(defn get-context
  "Gets current context, initializing if needed."
  []
  (or @*context* (do (init!) @*context*)))

(defn reload!
  "Reloads actionmaps from the same source, preserving original build opts."
  []
  (if-let [{:keys [actionmaps-source build-opts]} @*context*]
    (let [actionmaps (load-actionmaps-from-source actionmaps-source)]
      (reset! *context* (build-base-context actionmaps actionmaps-source (or build-opts {}))))
    (init!)))

(defn change-source!
  "Changes actionmaps source and rebuilds, preserving prior build opts."
  [new-source]
  (let [source     (resolve-actionmaps-source new-source)
        actionmaps (load-actionmaps-from-source source)
        opts       (or (:build-opts @*context*) {})]
    (reset! *context* (build-base-context actionmaps source opts))))

;; =============================================================================
;; Helper / Legacy
;; =============================================================================

(defn get-actionmaps-source [] (:actionmaps-source @*context*))

(defn refresh!
  "Legacy alias for reload! (kept for compatibility)."
  [_context] (reload!))

(comment
  ;; Clean initialization
  (init!)

  ;; Initialize with custom path
  (init! :actionmaps-path "resources/actionmaps.xml.bak2" :skip-svgs true)

  ;; Reload from same source (preserves opts)
  (reload!)

  ;; Change source (preserves prior opts)
  (change-source! "/new/path/actionmaps.xml")

  ;; Get current context
  (def c (get-context)))
