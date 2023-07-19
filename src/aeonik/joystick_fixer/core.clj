(ns aeonik.joystick-fixer.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]])
  (:import [java.nio.file Files Paths]
           (org.apache.commons.io FileUtils)))

(def evdev-regexp #"-event-joystick$")
(def joydev-regexp #"-joystick$")
(def evdev-path-regexp #"event\d+$")
(def joydev-path-regexp #"js\d+$")
(def device-paths {:by-id   "/dev/input/by-id"
                   :by-path "/dev/input/by-path"})

(def joystick-name-regexp #"VPC")
(def extract-name-rexexp #"usb-VIRPIL_Controls_20220720_(.*?)_FF(-event)?-joystick")
(def extract-pci-regexp #"pci-(.*?)-usb-")
(def extract-usb-regexp #"usb-(.*?)-")

(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (if (map? (first maps))
    (apply merge-with deep-merge maps)
    (apply merge-with deep-merge maps)))

(defn- symlink->real-path
  "Given a symlink, return the real path of the file it points to."
  [symlink]
  (-> symlink .getCanonicalFile .toString))

(defn- extract-name
  "Given a file name, return the joystick name."
  [file-name]
  (second (re-find extract-name-rexexp file-name)))

(defn- get-joystick-names
  "Given a map of device paths, return a list of joystick names."
  [{:keys [by-id]}]
  (->> (io/file by-id)
       (.listFiles)
       (map #(.getName %))
       (filter #(re-find evdev-regexp %))
       (map extract-name)))

(defn reverse-symlink-lookup->symlink-map
  "Given a key (:by-id or :by-path in this app) and a path, return a map of the
   path and the symlink it points to."
  [key path]
  {:path {(keyword key) (.toString path)
          :link         (symlink->real-path path)}})

(defn split-usb-pci [path]
  {:pci (second (re-find #"pci-(.*?)-usb" path))
   :usb (second (re-find #"usb-(.*?)-" path))})

(defn- search-path-for-symlink
  "Given a path and a map of symlinks, return a list of files that match the symlinks."
  [path symlink-map]
  (->> (io/file path)
       (.listFiles)
       (filter #(= (symlink->real-path %) (:link (symlink-map :path))))
       (map #(.getAbsolutePath %))))

(defn get-by-path-from-by-id
  "Given a patch from the by-id directory, return the corresponding path from the by-path directory."
  [by-id-path]
  (->> (io/file "/dev/input/by-path")
       (.listFiles)
       (filter #(= (symlink->real-path %) by-id-path))
       (map #(.getAbsolutePath %))))

(defn lookup-all-symlinks [path]
  (->> (io/file path)
       (.listFiles)
       (map #(reverse-symlink-lookup->symlink-map :by-id %))))

(defn- get-joystick-names
  "Given a map of device paths, return a list of joystick names."
  [{:keys [by-id]}]
  (->> (io/file by-id)
       (.listFiles)
       (map #(.getName %))
       (filter #(re-find evdev-regexp %))
       (map extract-name)))
(defn- search-path
  "Given a path and a regex, return a list of files that match the regex."
  [path regex]
  (->> (io/file path)
       (.listFiles)
       (filter #(re-find regex (.getName %)))
       (map #(.getAbsolutePath %))))

(defn by-id->symlink-map
  "Given a regex, return a map of the path and the symlink it points to."
  [regex]
  (let [path-key :by-id]
    (mapcat reverse-symlink-lookup->symlink-map
            (repeat path-key)
            (map io/file (search-path
                           (device-paths path-key)
                           regex)))))

;; Extract names, and add it to the root of each relevant node on the merged-id-path-map
(defn id-symlink-map
  "Given a regex, return a map of the path and the symlink it points to."
  [regex]
  (let [path-key :by-id]
    (mapcat reverse-symlink-lookup->symlink-map
            (repeat path-key)
            (map io/file (search-path
                           (device-paths path-key)
                           regex)))))

(defn get-joystick-info []
  (let [joystick-names (get-joystick-names device-paths)
        joystick-regexes (map #(re-pattern (str "_" % "_")) joystick-names)
        joystick-map (merge (zipmap joystick-names (map id-symlink-map joystick-regexes)))
        by-id (:by-id device-paths)]
    (map (fn [[k v]]
           [k (into {} (map (fn [path-map]
                              (let [{:keys [link]} (second path-map)
                                    dev-type (if (re-find #"event" link) :evdev-path :joydev-path)
                                    by-path (first (get-by-path-from-by-id link))
                                    usb-pci (split-usb-pci by-path)]
                                [dev-type (assoc (second path-map) :by-path by-path :usb-pci usb-pci)])) v))])
         joystick-map)))

(defn -main
  "If passed the -s argument, saves the output to a timestamped file in the resources directory.
   Otherwise, simply pprint the output."
  [& args]
  (let [output (get-joystick-info)]
    (if (some #(= "-s" %) args)
      (let [timestamp (str (java.time.LocalDateTime/now))
            file-name (str "/home/dave/Projects/joystick_fixer/resources/" timestamp "_joystick_device_map.edn")]
        (spit file-name (with-out-str (clojure.pprint/pprint output))))
      (clojure.pprint/pprint output))))



