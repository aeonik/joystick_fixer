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
(get-joystick-names device-paths)

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
  (let [evdev-id-path (by-path->by-id symlink-target)
        evdev-physical-path (by-id->by-path evdev-id-path)]
    {:evdev-id-path        evdev-id-path
     :evdev-physical-path  evdev-physical-path
     :evdev-symlink-target symlink-target}))

(defn symlink-target->joydev-info [symlink-target]
  (let [evdev-id-path (by-path->by-id symlink-target)
        evdev-physical-path (by-id->by-path evdev-id-path)]
    {:joydev-id-path        evdev-id-path
     :joydev-physical-path  evdev-physical-path
     :joydev-symlink-target symlink-target}))

(defn symlink-target->info [symlink-target]
  (if (re-find evdev-path-regexp symlink-target)
    (symlink-target->evdev-info symlink-target)
    (symlink-target->joydev-info symlink-target)))

(symlink-target->evdev-info "/dev/input/event27")
(comment (symlink-target->joydev-info "/dev/input/js1"))

(symlink-target->info "/dev/input/event27")
(symlink-target->info "/dev/input/js1")

(defn join-evdev+joydev-info [evdev-info joydev-info]
  (let [name        (filename->joystick-name (:evdev-id-path evdev-info))
        pci-address (:pci-address (split-usb-pci (:evdev-physical-path evdev-info)))
        usb-address (:usb-address (split-usb-pci (:evdev-physical-path evdev-info)))]
    {:name        name
     :evdev-info  evdev-info
     :joydev-info joydev-info
     :pci-address pci-address
     :usb-address usb-address}))

(join-evdev+joydev-info example-evdev-info example-joydev-info)

;; TODO:
;; For each :target in the following code
;;       call either symlink-target->evdev-info or symlink-target->joydev-info
;;       Use evdev-path-regexp or joydev-path-regexp to determine which function to call
;; We can use (get-joystick-names device-paths) to get a list of joystick names
;; We must start with the by-id path to correlate names to joysticks; you can also use filename->joystick-name
;;
;; Then, merge all the results into a single map by Joystick name
;; Note (join-evdev+joydev-info example-evdev-info example-joydev-info) WORKS
(->> (search-path "/dev/input/by-id" #"VPC")
     (map io/file)
     (map file->symlink))

(defn create-joystick-maps
  "Creates the joystick maps, based on the evdev-devices and joydev-devices maps"
  [evdev-devices joydev-devices]
  (let [joystick-names (get-joystick-names device-paths)
        evdev-infos    (map (fn [{:keys [target]}] (symlink-target->evdev-info target)) evdev-devices)
        joydev-infos   (map (fn [{:keys [target]}] (symlink-target->joydev-info target)) joydev-devices)]
    (reduce (fn [result joystick-name]
              (let [evdev-info  (first (filter #(= joystick-name (filename->joystick-name (:evdev-id-path %))) evdev-infos))
                    joydev-info (first (filter #(= joystick-name (filename->joystick-name (:joydev-id-path %))) joydev-infos))]
                (assoc result joystick-name (join-evdev+joydev-info evdev-info joydev-info))))
            {} joystick-names)))

(defn all-symlinks []
  (->> (search-path "/dev/input/by-id" #"VPC")
       (map io/file)
       (map file->symlink)))

(all-symlinks)

(defn group-all-symlinks-by-name []
  (let [symlinks (all-symlinks)
        names    (map filename->joystick-name (map :link symlinks))]
    (reduce (fn [result name]
              (let [symlinks-for-name (filter #(= name (filename->joystick-name (:link %))) symlinks)]
                (assoc result name (:evdev symlinks-for-name))))
            {} names)))

(defn group-all-symlinks-by-name-and-type []
  (let [symlinks (all-symlinks)
        names    (map filename->joystick-name (map :link symlinks))]
    (reduce (fn [result name]
              (let [symlinks-for-name (group-by (fn [symlink]
                                                  (if (str/includes? (:target symlink) "event") :evdev :joydev))
                                                (filter #(= name (filename->joystick-name (:link %))) symlinks))
                    symlinks-for-name (into {} (map (fn [[k v]] {k (first v)}) symlinks-for-name))]
                (assoc result name symlinks-for-name)))
            {} names)))






;; Need to rewrite the previous function as it returns a list of maps, not a map of maps, which is what we want
;; Need to go through each name and call symlink-target->joydev-info and symlink-target->evdev-info on each :target
;; Depending on the path, we'll know which function to call use joydev-path-regexp or evdev-path-regexp against :target
;; Then, we'll have a list of evdev-infos and joydev-infos that we can pass to join-evdev+joydev-info
;; Make sure to do this for each name so we don't lose correlation
;; Then, we can merge all the results into a single map by Joystick name
;; Note (join-evdev+joydev-info example-evdev-info example-joydev-info) WORKS

(group-all-symlinks-by-name)
(group-all-symlinks-by-name-and-type)

(reduce-kv (fn [result name symlinks]
                             (let [symlink-targets (map :target symlinks)]
                               (assoc result name symlink-targets)))
                           {} (group-all-symlinks-by-name))


(reduce-kv (fn [result name symlinks]
             (let [symlink-targets (map :target symlinks)
                   infos           (map symlink-target->info symlink-targets)
                   sorted-infos    (sort-by #(if (str/includes? (:evdev-id-path %) "event") 0 1) infos)
                   joined-info     (apply join-evdev+joydev-info sorted-infos)]
               (assoc result name joined-info)))
           {} (group-all-symlinks-by-name))

(defn create-joystick-maps
  "Creates the joystick maps, based on the evdev-devices and joydev-devices maps"
  []
  (let [joystick-names (get-joystick-names device-paths)
        symlinks (group-all-symlinks-by-name)
        evdev-joydev-infos (reduce-kv (fn [result name symlinks]
                                        (let [symlink-targets (map :target symlinks)
                                              infos           (map symlink-target->info symlink-targets)
                                              sorted-infos    (sort-by #(if (str/includes? (:evdev-id-path %) "event") 0 1) infos)
                                              joined-info     (apply join-evdev+joydev-info sorted-infos)]
                                          (assoc result name joined-info)))
                                      {} symlinks)]
    evdev-joydev-infos))

(create-joystick-maps)


;; execute symlink-target->info

(join-evdev+joydev-info example-evdev-info example-joydev-info)
(reduce-kv (fn [result name symlinks]
             (let [symlink-targets (map :target symlinks)
                   infos           (map symlink-target->info symlink-targets)]
               (assoc result name infos)))
           {} (group-all-symlinks-by-name))
(reduce-kv (fn [result name symlinks]
             (let [symlink-targets (map :target symlinks)
                   infos (map symlink-target->info symlink-targets)
                   sorted-infos (sort-by #(if (str/includes? (:evdev-id-path %) "event") 0 1) infos)
                   joined-info (apply join-evdev+joydev-info sorted-infos)]
               (assoc result name joined-info)))
           {} (group-all-symlinks-by-name))

(defn get-joysticks-info []
  (let [symlinks-by-name-and-type (group-all-symlinks-by-name-and-type)
        symlinks-info (map (fn [[name symlinks]]
                             (let [evdev-info (when-let [evdev-symlink (:evdev symlinks)]
                                                (symlink-target->evdev-info (:target evdev-symlink)))
                                   joydev-info (when-let [joydev-symlink (:joydev symlinks)]
                                                 (symlink-target->joydev-info (:target joydev-symlink)))
                                   joined-info (when (and evdev-info joydev-info)
                                                 (join-evdev+joydev-info evdev-info joydev-info))]
                               (when joined-info
                                 (assoc joined-info :name name))))
                           symlinks-by-name-and-type)]
    (remove nil? symlinks-info))) ; filter out joysticks that didn't have both evdev and joydev info

(get-joysticks-info)

;; Same thing but join the infos bas
(group-all-symlinks-by-name)
(map filename->joystick-name (map :link all-symlinks))
(def evdev-symlinks (filter #(re-find evdev-path-regexp (:target %)) all-symlinks))
(def joydev-symlinks (filter #(re-find joydev-path-regexp (:target %)) all-symlinks))
(def evdev-infos (mapcat symlink-target->evdev-info (map :target evdev-symlinks)))
(def joydev-infos (mapcat symlink-target->joydev-info (map :target joydev-symlinks)))
(def info-pairs (mapcat (fn [evdev-info joydev-info]
                          (join-evdev+joydev-info evdev-info joydev-info))
                        evdev-infos joydev-infos))
(pprint joydev-infos)
(pprint info-pairs)
(let [all-symlinks    (->> (search-path "/dev/input/by-id" #"VPC")
                           (map io/file)
                           (map file->symlink))
      evdev-symlinks  (filter #(re-find evdev-path-regexp (:target %)) all-symlinks)
      joydev-symlinks (filter #(re-find joydev-path-regexp (:target %)) all-symlinks)
      evdev-infos     (map symlink-target->evdev-info (map :target evdev-symlinks))
      joydev-infos    (map symlink-target->joydev-info (map :target joydev-symlinks))
      matched-infos   (map (fn [evdev-info joydev-info] (join-evdev+joydev-info evdev-info joydev-info))
                           evdev-infos joydev-infos)]
  matched-infos)

(comment (join-evdev+joydev-info example-evdev-info example-joydev-info)
         {:name        "VPC_Throttle_MT-50CM3",
          :evdev-info  {:evdev-id-path        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick",
                        :evdev-physical-path  "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-event-joystick",
                        :evdev-symlink-target "/dev/input/event5"},
          :joydev-info {:joydev-id-path        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-joystick",
                        :joydev-physical-path  "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-joystick",
                        :joydev-symlink-target "/dev/input/js1"},
          :pci-address "0000:2c:00.1",
          :usb-address "0:1.4.4:1.0"})
(def group-all-symlinks-by-name (group-all-symlinks-by-name))

(def evdev-infos (map (fn [symlinks] (map symlink-target->evdev-info (map :target symlinks))) (vals group-all-symlinks-by-name)))
(def joydev-infos (map (fn [symlinks] (map symlink-target->joydev-info (map :target symlinks))) (vals group-all-symlinks-by-name)))
(def matched-infos (map (fn [evdev-infos joydev-infos] (map (fn [evdev-info joydev-info] (join-evdev+joydev-info evdev-info joydev-info)) evdev-infos joydev-infos)) evdev-infos joydev-infos))
(defn generate-joystick-map []
  (let [grouped-symlinks-by-joystick-name (group-all-symlinks-by-name)
        joystick-names                    (keys grouped-symlinks-by-joystick-name)
        evdev-infos                       (map (fn [symlinks] (map symlink-target->evdev-info (map :target symlinks))) (vals grouped-symlinks-by-joystick-name))
        joydev-infos                      (map (fn [symlinks] (map symlink-target->joydev-info (map :target symlinks))) (vals grouped-symlinks-by-joystick-name))
        matched-infos                     (map (fn [evdev-infos joydev-infos] (map (fn [evdev-info joydev-info] (join-evdev+joydev-info evdev-info joydev-info)) evdev-infos joydev-infos)) evdev-infos joydev-infos)]
    (reduce (fn [result joystick-name]
              (let [joystick-matches
                    (map (fn [evdev-info joydev-info]
                           (join-evdev+joydev-info evdev-info joydev-info)) (first evdev-infos) (first joydev-infos))]
                (assoc result joystick-name joystick-matches)))
            {} joystick-names)))
(defn -main
  "If passed the -s argument, saves the output to a timestamped file in the resources directory.
   Otherwise, simply pprint the output."
  [& args]
  (let [joystick-map (generate-joystick-map)]
    (if (some #(= "-s" %) args)
      (let [timestamp (str (java.time.LocalDateTime/now))
            file-name (str "/home/dave/Projects/joystick_fixer/resources/" timestamp "_joystick_device_map.edn")]
        (spit file-name (with-out-str (clojure.pprint/pprint joystick-map))))
      (clojure.pprint/pprint joystick-map))))