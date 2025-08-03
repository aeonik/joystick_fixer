(ns aeonik.controlmap.discovery
  "Discovers and locates Star Citizen configuration files"
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]))

(def ^:private config
  "Application configuration loaded from config.edn"
  (-> "config.edn"
      io/resource
      slurp
      edn/read-string))

(def ^:private home-dir
  "User's home directory"
  (System/getProperty "user.home"))

(defn- expand-path-template
  "Expands path templates with system-specific values"
  [template]
  (let [relative-path (-> config :discovery :actionmaps-relative-path)
        username (or (System/getenv "USERNAME")
                     (System/getenv "USER")
                     "user")]
    (-> template
        (str/replace "{home}" home-dir)
        (str/replace "{relative-path}" relative-path)
        (str/replace "{username}" username))))

(defn- expand-environment-variables
  "Expands Windows-style environment variables (e.g., %USERNAME%) in path strings"
  [path]
  (str/replace path
               #"%([A-Za-z_][A-Za-z0-9_]*)%"
               (fn [[_ var-name]]
                 (or (System/getenv var-name)
                     (str "%" var-name "%")))))

(defn- detect-platform
  "Detects the current operating system platform based on configuration"
  []
  (let [{:keys [windows-prefixes linux-prefixes default-platform]}
        (-> config :discovery :platform-detection)
        os-name (System/getProperty "os.name")]
    (cond
      (some #(str/starts-with? os-name %) windows-prefixes) :windows
      (some #(str/starts-with? os-name %) linux-prefixes) :linux
      :else default-platform)))

(defn- get-known-paths
  "Gets the configured known paths for the current platform"
  []
  (->> (get-in config [:discovery :known-paths (detect-platform)])
       (map expand-path-template)))

(defn- find-existing-file
  "Returns the first existing file from a collection of paths, or nil if none exist"
  [paths]
  (->> paths
       (map expand-environment-variables)
       (map io/file)
       (filter #(.exists %))
       first))

(defn- validate-file-content
  "Validates file content based on configuration rules"
  [file]
  (let [{:keys [xml-declaration-patterns min-file-size]}
        (-> config :discovery :validation)]
    (and (.canRead file)
         (.isFile file)
         (>= (.length file) min-file-size)
         (with-open [reader (io/reader file)]
           (let [first-line (first (line-seq reader))]
             (some #(str/includes? first-line %) xml-declaration-patterns))))))

(defn find-actionmaps
  "Finds the Star Citizen actionmaps.xml file, checking environment override first,
   then platform-specific known paths. Returns a File object or nil if not found."
  []
  (let [env-var (-> config :discovery :environment-var)]
    (or
     ;; Priority 1: Environment variable override
     (some-> (System/getenv env-var)
             io/file
             (#(when (.exists %) %)))

     ;; Priority 2: Platform-specific known paths
     (find-existing-file (get-known-paths)))))

(defn actionmaps-path
  "Returns the absolute path to the actionmaps.xml file as a string, or nil if not found"
  []
  (some-> (find-actionmaps)
          .getAbsolutePath))

(defn validate-actionmaps-file
  "Validates that the found actionmaps file is readable and appears to be valid"
  [file]
  (when file
    (validate-file-content file)))

(defn actionmaps-info
  "Returns a map with information about the actionmaps file location and status"
  []
  (let [file (find-actionmaps)
        env-var (-> config :discovery :environment-var)]
    {:file file
     :path (some-> file .getAbsolutePath)
     :exists? (boolean file)
     :valid? (validate-actionmaps-file file)
     :size (when file (.length file))
     :platform (detect-platform)
     :env-override (System/getenv env-var)
     :searched-paths (get-known-paths)}))

(defn reload-config!
  "Reloads the configuration from config.edn (useful for development)"
  []
  (when (realized? config)
    (.bindRoot #'config
               (delay
                 (-> "config.edn"
                     io/resource
                     slurp
                     edn/read-string)))))

(defn get-product-svg-mapping
  "Returns product->svg mapping with compiled regex patterns"
  []
  (let [mapping (-> config :mapping :product-svg-mapping)]
    (into {} (map (fn [[pattern svg]]
                    [(re-pattern pattern) svg])
                  mapping))))

(comment
  ;; Example usage:
  (find-actionmaps)
  (actionmaps-path)
  (actionmaps-info)

  ;; Development helpers:
  (get-known-paths)
  (detect-platform)
  (reload-config!))

