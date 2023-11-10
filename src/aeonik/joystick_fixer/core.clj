(ns aeonik.joystick-fixer.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.reader.edn :as edn]
            [malli.core :as m])
  (:import (clojure.lang Keyword)
           (java.io File)
           [java.nio.file Files Paths]
           (java.util.regex Pattern)
           (org.apache.commons.io FileUtils)))

(def evdev-regexp #"-event-joystick$")
(def joydev-regexp #"FF-joystick$")
(def evdev-path-regexp #"event\d+$")
(def joydev-path-regexp #"js\d+$")
(def device-paths {:by-id   "/dev/input/by-id"
                   :by-path "/dev/input/by-path"})
(def joystick-name-regexp #"VPC")
(def extract-name-rexexp #"usb-VIRPIL_Controls_20220720_(.*?)_FF(-event)?-joystick")
(def extract-pci-regexp #"pci-(.*?)-usbv?\d*-")
(def extract-usb-regexp #"usbv?\d*-(.*?)-")
(def example-joystick-names ["L-VPC_Stick_MT-50CM2" "VPC_SharKa-50_Panel" "VPC_Throttle_MT-50CM3" "VPC_Stick_MT-50CM2"])

(defrecord evdev-info [evdev-id-path evdev-physical-path evdev-symlink-target])
(def example-evdev-info {:id-path        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick"
                         :physical-path  "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-event-joystick"
                         :symlink-target "/dev/input/event5"})
(defrecord joydev-info [joydev-id-path joydev-physical-path joydev-symlink-target])
(def example-joydev-info {:id-path        "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-joystick"
                          :physical-path  "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.4:1.0-joystick"
                          :symlink-target "/dev/input/js1"})
(def example-joystick-map {:name        "VPC Throttle MT-50CM3"
                           :evdev-info  example-evdev-info
                           :joydev-info example-joydev-info
                           :pci-address "0000:2c:00.1"
                           :usb-address "0:1.4.4:1.0"})
(defrecord joystick-map [name evdev-info joydev-info pci-address usb-address])

(defn read-edn-file [file-path]
  (with-open [rdr (clojure.java.io/reader file-path)]
    (edn/read-string (slurp rdr))))
(defn write-edn-file! [file-path data]
  (spit file-path (binding [clojure.pprint/*print-right-margin* 200]
                    (with-out-str (pprint/pprint data)))))


(defn deep-merge
  "Recursively merges maps."
  [& maps]
  (if (map? (first maps))
    (apply merge-with deep-merge maps)
    (apply merge-with deep-merge maps)))

(defn symlink->target
  "Given a symlink, return the real path of the target file it points to."
  [^File symlink]
  (-> symlink
      .getCanonicalFile
      .toString))

(comment (symlink->target (io/file "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick")))

(defn filename->joystick-name
  "Given a file name, return the joystick name."
  [^String file-name]
  (second (re-find extract-name-rexexp file-name)))

(comment (filename->joystick-name "usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick"))

(defn file->symlink
  "Given a file, return a map of the link and the target it points to.
  {:link /dev/input/by-id/{symlink} :target /dev/input/{target}}"
  [^File path]
  {:link (.toString path) :target (symlink->target path)})

(comment (file->symlink (io/file "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick")))

(defn split-usb-pci [^String path]
  "Given a path in /dev/input/by-path, return the pci and usb addresses."
  {:pci-address (second (re-find extract-pci-regexp path))
   :usb-address (second (re-find extract-usb-regexp path))})

(defn get-corresponding-path
  "Given a directory and a symlink, return the corresponding symlink that points to the same target.
  Aka, you have two symlinks that point to the same target
  This function lets you look up the other symlink, given the directory.
  /dev/input/by-id/{symlink} -> /dev/input/by-path/{symlink} or vice versa."
  [^String directory {:keys [target]}]
  (->> (io/file directory)
       (.listFiles)
       (filter #(= (symlink->target %) target))
       (map #(.getAbsolutePath %))
       (first)))

(defn by-id->by-path
  "Given a file  from the /dev/inputs/by-id directory, return the corresponding path from the by-path directory."
  [^String by-id-file]
  (get-corresponding-path (:by-path device-paths) (file->symlink (io/file by-id-file))))

(defn by-path->by-id
  "Given a file from the /dev/inputs/by-path directory, return the corresponding path from the by-id directory."
  [^String by-path-file]
  (get-corresponding-path (:by-id device-paths) (file->symlink (io/file by-path-file))))

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

(defn regex-search->id-symlinks
  "Given a regex, return a map of the path and the symlink it points to."
  [^Pattern regex]
  (->> (search-path (:by-id device-paths) regex)
       (map io/file)
       (mapv file->symlink)))

(comment (regex-search->id-symlinks #"VPC"))

(defn correlate-joystick-name [joystick-name]
  {:name   joystick-name
   :evdev  (first (regex-search->id-symlinks (re-pattern (str joystick-name ".*" evdev-regexp))))
   :joydev (first (regex-search->id-symlinks (re-pattern (str joystick-name ".*" joydev-regexp))))})

(first (regex-search->id-symlinks (re-pattern (str "VPC" ".*" joydev-regexp))))
(comment (into {} (mapv correlate-joystick-name example-joystick-names)))

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

(comment (join-evdev+joydev-info (symlink-target->evdev-info "/dev/input/event27") (symlink-target->joydev-info "/dev/input/js1")))

(defn get-joystick-info [correlated-joystick-map]
  "Takes input from the correlate-joystick-name function and returns a map of the joystick info."
  (let [joystick-map  correlated-joystick-map
        evdev-target  (get-in joystick-map [:evdev :target])
        joydev-target (get-in joystick-map [:joydev :target])]
    (join-evdev+joydev-info (symlink-target->evdev-info evdev-target) (symlink-target->joydev-info joydev-target))))

(defn process-all-joysticks []
  (->> device-paths
       get-joystick-names
       (map correlate-joystick-name)
       (mapv get-joystick-info)
       (sort-by :name)))

(defn promote-children [joystick-map]
  (let [evdev-info  (get joystick-map :evdev-info {})
        joydev-info (get joystick-map :joydev-info {})]
    (-> joystick-map
        (dissoc :evdev-info :joydev-info)
        (merge evdev-info joydev-info))))

(defn promote-and-order [joystick-map]
  (let [name         {:name (get joystick-map :name)}
        without-name (dissoc joystick-map :name)
        evdev-info   (get without-name :evdev-info {})
        joydev-info  (get without-name :joydev-info {})]
    (-> without-name
        (dissoc :evdev-info :joydev-info)
        (merge evdev-info joydev-info)
        (merge name))))


(comment (defn sort-joysticks [joystick-map]
           (sort-by :name joystick-map)))

(comment
  (def temp-file (process-all-joysticks))
  (def temp-file2 (read-string (slurp "/home/dave/Projects/joystick_fixer/resources/2023-07-18T22:55:51.210155120_joystick_device_map.edn"))))

(defn sort-joysticks [joystick-map]
  (sorted-map joystick-map))

(comment (into (sorted-map) temp-file))

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

(defn update-usb-address
  "This function is needed to fix the usb-address field in the data due to regex changes."
  [entry]
  (let [evdev-physical-path (get-in entry [:evdev-info :evdev-physical-path])
        joydev-physical-path (get-in entry [:joydev-info :joydev-physical-path])
        usb-address (or (-> (split-usb-pci evdev-physical-path) :usb-address)
                        (-> (split-usb-pci joydev-physical-path) :usb-address))]
    (assoc entry :usb-address usb-address)))

(defn process-files-usb-fix! [file-path]
  (let [data         (read-edn-file file-path)
        updated-data (map update-usb-address data)]
    (write-edn-file! file-path updated-data)))

(defn list-edn-files []
  (filter #(re-matches #".*_joystick_device_map\.edn$" (.getPath %))
          (file-seq (clojure.java.io/file "resources"))))

(defn update-all-files! []
  (doseq [file (list-edn-files)]
    (process-files-usb-fix! (.getPath file))))

(comment (def data (slurp "/home/dave/Projects/joystick_fixer/resources/2023-07-18T22:55:51.210155120_joystick_device_map.edn"))
         (def data2 (slurp "/home/dave/Projects/joystick_fixer/resources/2023-07-18T23:14:08.453350096_joystick_device_map.edn"))

         (transform-data (read-string data2)))

(comment (process-all-joysticks))

(defn simple-replace [^String input ^String old ^String new]
  (str/replace input (re-pattern old) new))

"   <rebind input=\"js1_button12\"/>\n"
"   <rebind input=\"js2_button41\"/>\n"
"   <rebind input=\"js3_button50\"/>\n"

" <options type=\"joystick\" instance=\"1\" Product=\"VIRPIL Controls 20220720 VPC Stick MT-50CM2  {012F3344-0000-0000-0000-504944564944}\">\n"
" <options type=\"joystick\" instance=\"2\" Product=\"VIRPIL Controls 20220720 VPC Throttle MT-50CM3  {01973344-0000-0000-0000-504944564944}\">\n"
" <options type=\"joystick\" instance=\"3\" Product=\"VIRPIL Controls 20220720 VPC SharKa-50 Panel  {025D3344-0000-0000-0000-504944564944}\"/>\n"
(def xml-replace {:input   #"input=\"js(\\d)"
                  :options #"<options type=\"joystick\" instance=(\\d) Product=\"(.*?)\""})

(def joystick-tranforms {1 2
                         2 3
                         3 1
                         4 4})
(defn remap-joystick-nums [xml-string]
  )

(defn -main
  "If passed the -s argument, saves the output to a timestamped file in the resources directory.
   Otherwise, simply pprint the output."
  [& args]
  (let [joystick-map (process-all-joysticks)]
    (if (some #(= "-s" %) args)
      (let [timestamp (str (java.time.LocalDateTime/now))
            file-name (str "/home/dave/Projects/joystick_fixer/resources/" timestamp "_joystick_device_map.edn")]
        (spit file-name (binding [clojure.pprint/*print-right-margin* 180]
                          (with-out-str (pprint joystick-map)))))
      (pprint joystick-map))))
