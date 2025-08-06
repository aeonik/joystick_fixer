(ns aeonik.controlmap.discovery
  "Discovers and locates Star Citizen configuration files"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

;; =============================================================================
;; Configuration Management
;; =============================================================================

(defonce ^:private config-atom (atom nil))

(defn load-config
  "Loads configuration from config.edn resource.
   Returns the config map or throws if not found."
  []
  (if-let [resource (io/resource "config.edn")]
    (-> resource slurp edn/read-string)
    (throw (ex-info "Configuration file not found"
                    {:expected "config.edn"
                     :classpath (System/getProperty "java.class.path")}))))

(defn get-config
  "Gets the current configuration, loading if necessary."
  []
  (or @config-atom
      (reset! config-atom (load-config))))

(defn reload-config!
  "Reloads the configuration from config.edn.
   Useful for development when config changes."
  []
  (reset! config-atom (load-config)))

;; =============================================================================
;; Platform Detection
;; =============================================================================

(defn detect-platform
  "Detects the current operating system platform.
   Returns :windows, :linux, or :unknown."
  []
  (let [config (get-config)
        {:keys [windows-prefixes linux-prefixes default-platform]}
        (-> config :discovery :platform-detection)
        os-name (str/lower-case (System/getProperty "os.name"))]
    (cond
      (some #(str/starts-with? os-name (str/lower-case %)) windows-prefixes) :windows
      (some #(str/starts-with? os-name (str/lower-case %)) linux-prefixes) :linux
      :else (or default-platform :unknown))))

;; =============================================================================
;; Path Expansion
;; =============================================================================

(defn get-system-vars
  "Gets commonly used system variables for path expansion."
  []
  {:home (System/getProperty "user.home")
   :username (or (System/getenv "USERNAME")
                 (System/getenv "USER")
                 (System/getProperty "user.name")
                 "user")
   :appdata (System/getenv "APPDATA")
   :localappdata (System/getenv "LOCALAPPDATA")
   :programfiles (System/getenv "ProgramFiles")})

(defn expand-path-template
  "Expands path templates with system-specific values.
   Supports {home}, {username}, {relative-path} placeholders."
  [template]
  (let [config (get-config)
        relative-path (-> config :discovery :actionmaps-relative-path)
        sys-vars (get-system-vars)]
    (-> template
        (str/replace "{home}" (:home sys-vars))
        (str/replace "{username}" (:username sys-vars))
        (str/replace "{relative-path}" relative-path)
        ;; Also support direct env var expansion
        (str/replace #"\{([^}]+)\}"
                     (fn [[_ var-name]]
                       (or (get sys-vars (keyword (str/lower-case var-name)))
                           (System/getenv var-name)
                           (str "{" var-name "}")))))))

(defn expand-environment-variables
  "Expands environment variables in path strings.
   Supports both Windows (%VAR%) and Unix ($VAR or ${VAR}) styles."
  [path]
  (-> path
      ;; Windows style %VAR%
      (str/replace #"%([A-Za-z_][A-Za-z0-9_]*)%"
                   (fn [[_ var-name]]
                     (or (System/getenv var-name)
                         (str "%" var-name "%"))))
      ;; Unix style $VAR or ${VAR}
      (str/replace #"\$\{?([A-Za-z_][A-Za-z0-9_]*)\}?"
                   (fn [[_ var-name]]
                     (or (System/getenv var-name)
                         (str "$" var-name))))))

;; =============================================================================
;; File Discovery
;; =============================================================================

(defn get-search-paths
  "Gets all search paths for the current platform.
   Returns a vector of expanded path strings."
  []
  (let [config (get-config)
        platform (detect-platform)
        paths (get-in config [:discovery :known-paths platform] [])]
    (->> paths
         (map expand-path-template)
         (map expand-environment-variables)
         vec)))

(defn find-file
  "Finds the first existing file from a collection of paths.
   Returns a File object or nil if none exist."
  [paths]
  (->> paths
       (map io/file)
       (filter #(and (.exists %) (.isFile %) (.canRead %)))
       first))

(defn validate-xml-file
  "Validates that a file appears to be a valid XML file.
   Returns true if valid, false otherwise."
  [file]
  (try
    (let [config (get-config)
          {:keys [xml-declaration-patterns min-file-size]}
          (-> config :discovery :validation)]
      (and file
           (.exists file)
           (.canRead file)
           (.isFile file)
           (>= (.length file) min-file-size)
           (with-open [reader (io/reader file)]
             (let [first-line (first (line-seq reader))]
               (some #(str/includes? first-line %) xml-declaration-patterns)))))
    (catch Exception e
      (println (format "Error validating file %s: %s"
                       (.getName file) (.getMessage e)))
      false)))

;; =============================================================================
;; Actionmaps Discovery
;; =============================================================================

(defn find-actionmaps-with-override
  "Finds actionmaps file, checking environment variable override first.
   Returns a File object or nil."
  [env-var-name]
  (if-let [env-path (System/getenv env-var-name)]
    (let [env-file (io/file env-path)]
      (if (.exists env-file)
        (do
          (println (format "Using actionmaps from %s: %s"
                           env-var-name (.getAbsolutePath env-file)))
          env-file)
        (do
          (println (format "Warning: %s points to non-existent file: %s"
                           env-var-name env-path))
          nil)))
    nil))

(defn find-actionmaps
  "Finds the Star Citizen actionmaps.xml file.
   Priority order:
   1. Environment variable override (SC_ACTIONMAPS_PATH)
   2. Platform-specific known paths
   Returns a File object or nil if not found."
  []
  (let [config (get-config)
        env-var (-> config :discovery :environment-var)]
    (or
     ;; Priority 1: Environment variable override
     (find-actionmaps-with-override env-var)

     ;; Priority 2: Platform-specific known paths
     (find-file (get-search-paths)))))

(defn load-actionmaps
  "Loads and returns the actionmaps file content as a string.
   Throws an exception if the file cannot be found or read."
  []
  (if-let [file (find-actionmaps)]
    (slurp file)
    (throw (ex-info "Actionmaps file not found"
                    {:searched-paths (get-search-paths)
                     :platform (detect-platform)
                     :env-var (-> (get-config) :discovery :environment-var)
                     :env-value (System/getenv (-> (get-config) :discovery :environment-var))}))))

;; =============================================================================
;; Product Mapping
;; =============================================================================

(defn get-product-svg-mapping
  "Returns product->svg mapping with compiled regex patterns.
   Returns a vector of [regex-pattern svg-name] pairs."
  []
  (let [config (get-config)
        mapping (-> config :mapping :product-svg-mapping)]
    (mapv (fn [[pattern svg]]
            [(re-pattern pattern) svg])
          mapping)))

(defn find-svg-for-product
  "Finds the SVG name for a given product string.
   Returns the SVG name or nil if no match found."
  [product-string]
  (let [mappings (get-product-svg-mapping)]
    (some (fn [[pattern svg-name]]
            (when (re-find pattern product-string)
              svg-name))
          mappings)))

;; =============================================================================
;; Status & Information
;; =============================================================================

(defn actionmaps-info
  "Returns comprehensive information about actionmaps discovery status."
  []
  (let [config (get-config)
        env-var (-> config :discovery :environment-var)
        env-value (System/getenv env-var)
        search-paths (get-search-paths)
        file (find-actionmaps)]
    {:platform (detect-platform)
     :file file
     :path (some-> file .getAbsolutePath)
     :exists? (boolean file)
     :valid? (when file (validate-xml-file file))
     :size (when file (.length file))
     :readable? (when file (.canRead file))
     :last-modified (when file (java.util.Date. (.lastModified file)))
     :env-var env-var
     :env-override env-value
     :using-override? (boolean env-value)
     :searched-paths search-paths
     :search-count (count search-paths)}))

(defn print-discovery-status!
  "Prints a formatted discovery status report."
  []
  (let [info (actionmaps-info)]
    (println "\n╔══════════════════════════════════════╗")
    (println "║     Discovery Status Report         ║")
    (println "╚══════════════════════════════════════╝")
    (println)
    (println "Platform:" (:platform info))
    (println "Environment:" (:env-var info) "=" (or (:env-override info) "<not set>"))
    (println)
    (if (:exists? info)
      (do
        (println "✓ Actionmaps found!")
        (println "  Path:" (:path info))
        (println "  Size:" (format "%.2f KB" (/ (:size info) 1024.0)))
        (println "  Valid XML:" (if (:valid? info) "Yes" "No"))
        (println "  Last modified:" (:last-modified info)))
      (do
        (println "✗ Actionmaps not found")
        (println "\nSearched paths:")
        (doseq [path (:searched-paths info)]
          (println "  •" path))))
    (println "\n════════════════════════════════════════\n")))

;; =============================================================================
;; Development Helpers
;; =============================================================================

(comment
  ;; Check current configuration
  (get-config)

  ;; Reload config from disk
  (reload-config!)

  ;; Platform detection
  (detect-platform)

  ;; Path expansion testing
  (expand-path-template "{home}/Games/{relative-path}")
  (expand-environment-variables "%APPDATA%/Star Citizen")

  ;; Get all search paths
  (get-search-paths)

  ;; Find actionmaps
  (find-actionmaps)

  ;; Get full status
  (actionmaps-info)

  ;; Print nice status report
  (print-discovery-status!)

  ;; Test product mapping
  (find-svg-for-product "VKB-Sim Gladiator")
  (get-product-svg-mapping))
