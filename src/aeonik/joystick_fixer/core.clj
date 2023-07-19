(ns aeonik.joystick-fixer.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [malli.core :as m])

  (:import (clojure.lang Keyword)
           (java.io File)
           [java.nio.file Files Paths]
           (java.util.regex Pattern)
           (org.apache.commons.io FileUtils)))

(def evdev-regexp #"-event-joystick$")
(def joydev-regexp #"-joystic")
(def evdev-path-regexp #"event\d+$")
(def joydev-path-regexp #"js\d+$")
(def device-paths {:by-id   "/dev/input/by-id"
                   :by-path "/dev/input/by-path"})

(def joystick-name-regexp #"VPC")
(def extract-name-rexexp #"usb-VIRPIL_Controls_20220720_(.*?)_FF(-event)?-joystick")
(def extract-pci-regexp #"pci-(.*?)-usb-")
(def extract-usb-regexp #"usb-(.*?)-")

(def example-joystick-names ["L-VPC_Stick_MT-50CM2" "VPC_SharKa-50_Panel" "VPC_Throttle_MT-50CM3" "VPC_Stick_MT-50CM2"])

(defrecord evdev-info [evdev-id-path evdev-physical-path evdev-symlink-target])
(def example-evdev-info {:evdev-id-path        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick"
                         :evdev-physical-path  "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-event-joystick"
                         :evdev-symlink-target "/dev/input/event5"})
(defrecord joydev-info [joydev-id-path joydev-physical-path joydev-symlink-target])
(def example-joydev-info {:joydev-id-path        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-joystick"
                          :joydev-physical-path  "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-joystick"
                          :joydev-symlink-target "/dev/input/js1"})
(defrecord joystick-map [name evdev-info joydev-info pci-address usb-address])

(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (if (map? (first maps))
    (apply merge-with deep-merge maps)
    (apply merge-with deep-merge maps)))

(defn symlink->target
  "Given a symlink, return the real path of the target file it points to."
  [^File symlink]
  (^String -> symlink .getCanonicalFile .toString))

(comment (symlink->target (io/file "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick")))

(defn filename->joystick-name
  "Given a file name, return the joystick name."
  [^String file-name]
  (^String second (re-find extract-name-rexexp file-name)))

(defn file->symlink
  "Given a key (:by-id or :by-path in this app) and a path, return a map of the
   path and the symlink it points to."
  [^File path]
  {:link (.toString path) :target (symlink->target path)})

(comment (file->symlink (io/file "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick")))

(defn split-usb-pci [^String path]
  {:pci-address (second (re-find #"pci-(.*?)-usb" path))
   :usb-address (second (re-find #"usb-(.*?)-" path))})

(defn get-corresponding-path
  "Given a directory and a target file, return the corresponding path from the directory.
  Scans the directory for symlinks that point to the target file."
  [^String directory {:keys [target]}]
  (->> (io/file directory)
       (.listFiles)
       (filter #(= (symlink->target %) target))
       (map #(.getAbsolutePath %))
       (first)))                                            ; Convert the sequence to a list

(defn by-id->by-path
  "Given a file  from the /dev/inputs/by-id directory, return the corresponding path from the by-path directory."
  [^String by-id-file]
  (get-corresponding-path "/dev/input/by-path" (file->symlink (io/file by-id-file))))

(defn by-path->by-id
  "Given a file from the /dev/inputs/by-path directory, return the corresponding path from the by-id directory."
  [^String by-path-file]
  (get-corresponding-path "/dev/input/by-id" (file->symlink (io/file by-path-file))))

(comment (by-path->by-id "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-joystick"))
(comment (by-id->by-path "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick"))

(comment
  (defn lookup-all-symlinks [^String path]
    (->> (io/file path)
         (.listFiles)
         (map #(file->symlink %))))
  (lookup-all-symlinks "/dev/input/by-id"))

(defn get-joystick-names
  "Given a map of device paths, return a list of joystick names."
  [{:keys [by-id]}]
  (->> (io/file by-id)
       (.listFiles)
       (map #(.getName %))
       (filter #(re-find evdev-regexp %))
       (map filename->joystick-name)))
(comment (get-joystick-names device-paths))

(defn search-path
  "Given a path and a regex, return a list of files that match the regex."
  [^String path ^Pattern regex]
  (->> (io/file path)
       (.listFiles)
       (filter #(re-find regex (.getName %)))
       (mapv #(.getAbsolutePath %))))

(comment (search-path "/dev/input/by-id" #"VPC"))

;; Extract names, and add it to the root of each relevant node on the merged-id-path-map
(defn regex-search->id-symlinks
  "Given a regex, return a map of the path and the symlink it points to."
  [^Pattern regex]
  (->> (search-path "/dev/input/by-id" regex)
       (map io/file)
       (mapv file->symlink)))

(defn symlink-target->evdev-info [symlink-target]
  (let [evdev-id-path       (by-path->by-id symlink-target)
        evdev-physical-path (by-id->by-path evdev-id-path)]
    {:evdev-id-path        evdev-id-path
     :evdev-physical-path  evdev-physical-path
     :evdev-symlink-target symlink-target}))

(defn symlink-target->joydev-info [symlink-target]
  (let [evdev-id-path       (by-path->by-id symlink-target)
        evdev-physical-path (by-id->by-path evdev-id-path)]
    {:joydev-id-path        evdev-id-path
     :joydev-physical-path  evdev-physical-path
     :joydev-symlink-target symlink-target}))

(defn symlink-target->info [symlink-target]
  (if (re-find evdev-path-regexp symlink-target)
    (symlink-target->evdev-info symlink-target)
    (symlink-target->joydev-info symlink-target)))


(comment (symlink-target->joydev-info "/dev/input/js1")
         (symlink-target->evdev-info "/dev/input/event27"))

(comment (symlink-target->info "/dev/input/event27")
         (symlink-target->info "/dev/input/js1"))

(defn join-evdev+joydev-info [evdev-info joydev-info]
  (let [name        (filename->joystick-name (:evdev-id-path evdev-info))
        pci-address (:pci-address (split-usb-pci (:evdev-physical-path evdev-info)))
        usb-address (:usb-address (split-usb-pci (:evdev-physical-path evdev-info)))]
    {:name        name
     :evdev-info  evdev-info
     :joydev-info joydev-info
     :pci-address pci-address
     :usb-address usb-address}))

(defn all-symlinks []
  (->> (search-path "/dev/input/by-id" #"VPC")
       (map io/file)
       (map file->symlink)))

(defn filter-symlinks-by-name [name symlinks]
  (filter #(= name (filename->joystick-name (:link %))) symlinks))

(defn determine-joystick-type [symlink]
  (if (str/includes? (:target symlink) "event") :evdev :joydev))

(defn filter-and-group-symlinks [name symlinks]
  (let [filtered-symlinks (filter #(= name (filename->joystick-name (:link %))) symlinks)]
    (group-by determine-joystick-type filtered-symlinks)))

(defn group-all-symlinks-by-name-and-type []
  (let [symlinks (all-symlinks)
        names    (map filename->joystick-name (map :link symlinks))]
    (reduce (fn [result name]
              (let [grouped-symlinks  (filter-and-group-symlinks name symlinks)
                    symlinks-for-name (into {} (map (fn [[k v]] {k (first v)}) grouped-symlinks))]
                (assoc result name symlinks-for-name)))
            {} names)))

(defn get-single-joystick-info [name symlinks]
  (let [evdev-info  (when-let [evdev-symlink (:evdev symlinks)]
                      (symlink-target->evdev-info (:target evdev-symlink)))
        joydev-info (when-let [joydev-symlink (:joydev symlinks)]
                      (symlink-target->joydev-info (:target joydev-symlink)))
        joined-info (when (and evdev-info joydev-info)
                      (join-evdev+joydev-info evdev-info joydev-info))]
    (when joined-info
      (assoc joined-info :name name))))

(defn get-joysticks-info []
  (let [symlinks-by-name-and-type (group-all-symlinks-by-name-and-type)
        symlinks-info             (map (fn [[name symlinks]]
                                         (get-single-joystick-info name symlinks))
                                       symlinks-by-name-and-type)]
    (remove nil? symlinks-info)))

(defn transform-data
  "This transforms the old data format to the new one.
  KEK I could have used this, and it probably would have been
  more efficient than the refactor, and saved me another 8 hours.
  See core_backup.clj for the old code"
  [input-data]
  (map
    (fn [[name data]]
      (let [{:keys [evdev-path joydev-path]} data
            {:keys [by-id by-path link usb-pci]} evdev-path
            {:keys [pci usb]} usb-pci
            {:keys [by-id by-path link]} joydev-path]
        {:name        name
         :evdev-info  {:evdev-id-path        by-id
                       :evdev-physical-path  by-path
                       :evdev-symlink-target link}
         :joydev-info {:joydev-id-path        by-id
                       :joydev-physical-path  by-path
                       :joydev-symlink-target link}
         :pci-address pci
         :usb-address usb}))
    input-data))
(comment (def data (slurp "/home/dave/Projects/joystick_fixer/resources/2023-07-18T22:55:51.210155120_joystick_device_map.edn"))
         (def data2 (slurp "/home/dave/Projects/joystick_fixer/resources/2023-07-18T23:14:08.453350096_joystick_device_map.edn"))

         (transform-data (read-string data2)))

(defn -main
  "If passed the -s argument, saves the output to a timestamped file in the resources directory.
   Otherwise, simply pprint the output."
  [& args]
  (let [joystick-map (get-joysticks-info)]
    (if (some #(= "-s" %) args)
      (let [timestamp (str (java.time.LocalDateTime/now))
            file-name (str "/home/dave/Projects/joystick_fixer/resources/" timestamp "_joystick_device_map.edn")]
        (spit file-name (with-out-str (clojure.pprint/pprint joystick-map))))
      (clojure.pprint/pprint joystick-map))))