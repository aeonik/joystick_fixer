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

(defn deep-merge [a & maps]
  (if (map? a)
    (apply merge-with deep-merge a maps)
    (apply merge-with deep-merge maps)))

(defn- symlink->real-path
  "Input: java.io.File
   Output: String
   Given a symlink, return the real path of the file it points to."
  [symlink]
  (-> symlink .getCanonicalFile .toString))

(symlink->real-path (io/file "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick"))

;; => "/dev/input/event27"

(defn reverse-symlink-lookup->symlink-map
  "Given a key (:by-id or :by-path in this app) and a path, return a map of the
   path and the symlink it points to."
  [key path]
  {:path {(keyword key) (.toString path)
          :link         (symlink->real-path path)}})

(reverse-symlink-lookup->symlink-map :by-id (io/file "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick"))
;; => {:path
;;     {:by-id
;;      "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick",
;;      :link "/dev/input/event27"}}

(defn split-usb-pci [path]
  {:pci (second (re-find #"pci-(.*?)-usb" path))
   :usb (second (re-find #"usb-(.*?)-" path))})

(split-usb-pci "/dev/input/by-path/pci-0000:00:14.0-usb-0:2:1.0-event-joystick")
;; => {:pci ["pci-0000:00:14.0-usb" "0000:00:14.0"], :usb ["usb-0:2:1.0-" "0:2:1.0"]}

;; Given a map with the keys :path :link perform a search in the given path for those same symlinks
;; This is like a reverse lookup for symlinks.
(defn- search-path-for-symlink
  "Given a path and a map of symlinks, return a list of files that match the symlinks."
  [path symlink-map]
  (->> (io/file path)
       (.listFiles)
       (filter #(= (symlink->real-path %) (:link (symlink-map :path))))
       (map #(.getAbsolutePath %))))

(search-path-for-symlink "/dev/input/by-path" (reverse-symlink-lookup->symlink-map :by-id (io/file "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick")))
;; => ("/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.3:1.0-event-joystick")

(defn get-by-path-from-by-id
  "Given a patch from the by-id directory, return the corresponding path from the by-path directory."
  [by-id-path]
  (->> (io/file "/dev/input/by-path")
       (.listFiles)
       (filter #(= (symlink->real-path %) by-id-path))
       (map #(.getAbsolutePath %))))

(get-by-path-from-by-id "/dev/input/event27")

(defn lookup-all-symlinks [path]
  (->> (io/file path)
       (.listFiles)
       (map #(reverse-symlink-lookup->symlink-map :by-id %))))

(lookup-all-symlinks "/dev/input/by-id")

(defn- extract-name
  "Given a file name, return the joystick name."
  [file-name]
  (second (re-find extract-name-rexexp file-name)))

(pprint (extract-name "usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick"))

(defn- get-joystick-names
  "Given a map of device paths, return a list of joystick names."
  [{:keys [by-id]}]
  (->> (io/file by-id)
       (.listFiles)
       (map #(.getName %))
       (filter #(re-find evdev-regexp %))
       (map extract-name)))

(get-joystick-names device-paths)
;; => ("VPC_Stick_MT-50CM2"
;;     "L-VPC_Stick_MT-50CM2"
;;     "VPC_Throttle_MT-50CM3"
;;     "VPC_SharKa-50_Panel")

(defn- search-path
  "Given a path and a regex, return a list of files that match the regex."
  [path regex]
  (->> (io/file path)
       (.listFiles)
       (filter #(re-find regex (.getName %)))
       (map #(.getAbsolutePath %))))

(search-path "/dev/input/by-id" #"VPC_Stick_MT-50CM2")
;; => ("/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick"
;;     "/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-event-joystick"
;;     "/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-joystick"
;;     "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick")

;(search-path (:by-id device-paths) (re-pattern (get-joystick-names device-paths)))
(->> (get-joystick-names device-paths)
     (map re-pattern)
     (map #(search-path (:by-id device-paths) %)))

(pprint (search-path "/dev/input/by-path" #"event27"))

(defn by-id->symlink-map
  "Given a regex, return a map of the path and the symlink it points to."
  [regex]
  (let [path-key :by-id]
    (mapcat reverse-symlink-lookup->symlink-map
            (repeat path-key)
            (map io/file (search-path
                          (device-paths path-key)
                          regex)))))
(by-id->symlink-map #"VPC")
;; => ([:path
;;      {:by-id
;;       "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick",
;;       :link "/dev/input/event27"}]
;;     [:path
;;      {:by-id
;;       "/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-event-joystick",
;;       :link "/dev/input/event28"}]
;;     [:path
;;      {:by-id
;;       "/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-joystick",
;;       :link "/dev/input/js4"}]

(comment (defn merged-id-path-map
           "Look up all the symlinks in :path :by-id, and merge them into a single map."
           [device-paths]
           (let [path-key :by-id]
             (apply merge-with merge
                    (map (fn [path]
                           (let [symlink-map (by-id->symlink-map path)] ; assuming by-id->symlink-map is a function in your context
                             (zipmap (map first symlink-map)
                                     (map second symlink-map))))
                         (search-path
                          (get device-paths path-key)
                          (re-pattern (get-joystick-names device-paths)))))))) ; assuming get-joystick-names is a function in your context

(comment (merged-id-path-map device-paths))

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
(defn add-names-to-merged-map [merged-map]
  (let [joystick-names   (get-joystick-names device-paths)
        joystick-regexes (map re-pattern joystick-names)
        joystick-map     (merge (zipmap joystick-names (map id-symlink-map joystick-regexes)))
        by-id            (:by-id device-paths)]
    (merge (zipmap joystick-names (map #(assoc % :name (extract-name %)) (lookup-all-symlinks by-id)))
           merged-map)))

(let [joystick-names   (get-joystick-names device-paths)
      joystick-regexes (map re-pattern joystick-names)
      joystick-map     (merge (zipmap joystick-names (map id-symlink-map joystick-regexes)))
      by-id            (:by-id device-paths)]
  joystick-map)
;; => {"VPC_Stick_MT-50CM2"
;;     ([:path
;;       {:by-id
;;        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick",
;;        :link "/dev/input/event27"}]
;;      [:path
;;       {:by-id
;;        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-event-joystick",
;;        :link "/dev/input/event28"}]
;;      [:path
;;       {:by-id
;;        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-joystick",
;;        :link "/dev/input/js4"}]
;;      [:path
;;       {:by-id
;;        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick",
;;        :link "/dev/input/js3"}]),
;;     "L-VPC_Stick_MT-50CM2"
;;

(let [joystick-names   (get-joystick-names device-paths)
      joystick-regexes (map #(re-pattern (str "_" % "_")) joystick-names)
      joystick-map     (merge (zipmap joystick-names (map id-symlink-map joystick-regexes)))
      by-id            (:by-id device-paths)]
  (map (fn [[k v]]
         [k (into {} (map (fn [path-map]
                            (let [{:keys [link]} (second path-map)
                                  dev-type (if (re-find #"event" link) :evdev-path :joydev-path)
                                  by-path (first (get-by-path-from-by-id link))
                                  usb-pci (split-usb-pci by-path)]
                              [dev-type (assoc (second path-map) :by-path by-path :usb-pci usb-pci)])) v))]) joystick-map))

(defn get-joystick-map
  "Returns a map containing the joystick name and a map of the path, link, and
   symlink. The path includes both by-id and by-path."
  [path]
  (let [joystick-name        (extract-name path)
        by-id->symlink-map   (reverse-symlink-lookup->symlink-map :by-id (io/file path))
        by-path-path         (search-path-for-symlink "/dev/input/by-path" by-id->symlink-map)
        by-path->symlink-map (reverse-symlink-lookup->symlink-map :by-path (io/file (first by-path-path)))
        ;; Merge the two maps
        merged-map           (deep-merge by-id->symlink-map by-path->symlink-map)]
    {joystick-name merged-map}))

(pprint (get-joystick-map "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick"))
;; => {"VPC_Stick_MT-50CM2"
;;     {:path
;;      {:by-id
;;       "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick",
;;       :link "/dev/input/js3",
;;       :by-path "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.3:1.0-joystick"}}}

(defn get-joystick-map-from-regexes
  "Returns a map of joystick names to their corresponding maps of paths and links.
   Processes all paths for each joystick."
  [joystick-names device-paths]
  (let [by-id (:by-id device-paths)]
    (reduce (fn [result-map joystick-name]
              (let [joystick-regex      (re-pattern joystick-name)
                    joystick-paths      (search-path "/dev/input/by-id" joystick-regex)
                    joystick-maps       (map get-joystick-map (map io/file joystick-paths))
                    merged-joystick-map (reduce deep-merge joystick-maps)]
                (merge result-map {joystick-name merged-joystick-map})))
            {}
            joystick-names)))

(def joystick-names (get-joystick-names device-paths))
(def joystick-regexes (map #(re-pattern (str "_" % "_")) joystick-names))
(let [joystick-names   (get-joystick-names device-paths)
      joystick-regexes (map #(re-pattern (str "_" % "_")) joystick-names)
      joystick-map     (merge (zipmap joystick-names (map id-symlink-map joystick-regexes)))
      by-id            (:by-id device-paths)]
  joystick-map)

(reverse-symlink-lookup->symlink-map :by-path (io/file (first (search-path "/dev/input/by-path" #"joystick"))))

(let [joystick-names   (get-joystick-names device-paths)
      joystick-regexes (map #(re-pattern (str "_" % "_")) joystick-names)
      joystick-map     (merge (zipmap joystick-names (map id-symlink-map joystick-regexes)))
      by-id            (:by-id device-paths)]
  (map (fn [[k v]]
         [k (into {} (map (fn [path-map]
                            (let [{:keys [link]} (second path-map)
                                  dev-type (if (re-find #"event" link) :evdev-path :joydev-path)]
                              [dev-type (second path-map)])) v))]) joystick-map))

(let [joystick-names   (get-joystick-names device-paths)
      joystick-regexes (map #(re-pattern (str "_" % "_")) joystick-names)
      joystick-map     (merge (zipmap joystick-names (map id-symlink-map joystick-regexes)))
      by-id            (:by-id device-paths)]
  (map (fn [[k v]]
         [k (into {} (map (fn [path-map]
                            (let [{:keys [link]} (second path-map)
                                  dev-type (if (re-find #"event" link) :evdev-path :joydev-path)
                                  by-path (first (get-by-path-from-by-id link))]
                              [dev-type (assoc (second path-map) :by-path by-path)])) v))]) joystick-map))

(mapcat reverse-symlink-lookup->symlink-map (repeat :by-id) (map io/file (search-path "/dev/input/by-id" #"VPC")))
;; => ([:path
;;      "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick"]
;;     [:link "/dev/input/event27"]
;;     [:path
;;      "/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-event-joystick"]
;;     [:link "/dev/input/event28"]
;;     [:path
;;      "/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-joystick"]
;;     [:link "/dev/input/js4"]
;;     [:path
;;      "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick"]
;;     [:link "/dev/input/js3"]
;;     [:path
;;      "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick"]
;;     [:link "/dev/input/event26"]
;;     [:path
;;      "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-joystick"]
;;     [:link "/dev/input/js2"]
;;     [:path
;;      "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-event-joystick"]
;;     [:link "/dev/input/event25"]
;;     [:path
;;      "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick"]
;;     [:link "/dev/input/js1"])

;; Inputs:
;;  - device-paths
;;  - joystick-names
;;
;; Example output
;; {"VPC_Stick_MT-50CM2" {
;;                       :joydev /dev/input/js1
;;                       :evdev /dev/input/event28
;;                       :pci "0000:2c:00.1"
;;                       :usb "0:1.4.3:1.0"
;;                       :paths { :by-id ["/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick" ...
;;                       }
;;                       ...

(defn get-joystick-maps
  "Returns a map containing the joystick name and a map of the path, link, and
   symlink. The path includes both by-id and by-path."
  [path]
  (let [joystick-name (extract-name path)
        by-id->symlink-map (reverse-symlink-lookup->symlink-map :by-id (io/file path))
        by-path-path (search-path-for-symlink "/dev/input/by-path" by-id->symlink-map)
        by-path->symlink-map (reverse-symlink-lookup->symlink-map :by-path (io/file (first by-path-path)))
        ;; Split the by-path into PCI and USB
        usb-pci (split-usb-pci (first by-path-path))
        ;; Merge the two maps
        merged-map (deep-merge by-id->symlink-map by-path->symlink-map usb-pci)]
    {joystick-name merged-map}))
(comment (get-joystick-map-from-regexes (get-joystick-names device-paths) device-paths))

(get-joystick-maps "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick")
;; => {"VPC_Stick_MT-50CM2"
;;     {:path
;;      {:by-id
;;       "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick",
;;       :link "/dev/input/js3",
;;       :by-path "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.3:1.0-joystick"},
;;      :pci "0000:2c:00.1",
;;      :usb "0:1.4.3:1.0"}}
;; => {"VPC_Stick_MT-50CM2"
;;     {:path
;;      {:by-id
;;       "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick",
;;       :link "/dev/input/js3",
;;       :by-path "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.3:1.0-joystick"},
;;      :pci "0000:2c:00.1",
;;      :usb "0:1.4.3:1.0"}}

(get-joystick-maps "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick")

(defn get-joystick-maps
  "Returns a map containing the joystick name and a map of the path, link, and
   symlink. The path includes both by-id and by-path."
  [device-paths]
  (let [joystick-names   (get-joystick-names device-paths)
        joystick-regexes (map #(re-pattern (str "_" % "_")) joystick-names)
        joystick-map     (merge (zipmap joystick-names (map id-symlink-map joystick-regexes)))
        by-id            (:by-id device-paths)]
    (into {} (map (fn [[k v]]
                    [k (into {} (map (fn [path-map]
                                       (let [{:keys [link]} (second path-map)
                                             dev-type (if (re-find #"event" link) :evdev-path :joydev-path)
                                             by-path-path (get-by-path-from-by-id (second path-map))
                                             usb-pci (split-usb-pci (first by-path-path))]
                                         [dev-type (deep-merge (second path-map) usb-pci)])) v))]) joystick-map))))

(search-path "/dev/input/by-id" #"VPC")
;; => ("/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick"
;;     "/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-event-joystick"
;;     "/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-joystick"
;;     "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-joystick"
;;     "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick"
;;     "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-joystick"
;;     "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-event-joystick"
;;     "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_SharKa-50_Panel_FF-joystick")

(def joystick-paths (search-path "/dev/input/by-id" #"VPC"))
(pprint joystick-paths)
(def joystick-maps (map get-joystick-maps (remove nil? joystick-paths)))
(pprint joystick-maps)
(def merged-joystick-maps (reduce merge joystick-maps))
(pprint joystick-maps)

(defn -main [& args]
  (println "Hello, World!"))
