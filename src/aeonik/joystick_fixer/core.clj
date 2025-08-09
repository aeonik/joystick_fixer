(ns aeonik.joystick-fixer.core
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.data :as data]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.reader.edn :as edn]
            [editscript.core :as e]
            [malli.core :as m])
  (:import (clojure.lang Keyword)
           (java.io File)
           [java.nio.file Files Paths]
           (java.time LocalDateTime)
           (java.util.regex Pattern)
           (org.apache.commons.io FileUtils)))

(def evdev-regexp #"-event-joystick$")
(def joydev-regexp #"(?<!-event)-joystick$")
(def evdev-path-regexp #"event\d+$")
(def joydev-path-regexp #"js\d+$")
(def device-paths {:by-id   "/dev/input/by-id"
                   :by-path "/dev/input/by-path"})
(def joystick-name-regexp #"VPC")
(def extract-name-rexexp #"usb-VIRPIL_Controls_\d+_(.*?)_FF(-event)?-joystick")
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

(def order-of-keys [:name :pci-address :usb-address :evdev-id-path :evdev-physical-path :evdev-symlink-target :joydev-id-path :joydev-physical-path :joydev-symlink-target])
(def order-map (zipmap order-of-keys (range)))
(defn key-comparator [k1 k2]
  (compare (order-map k1) (order-map k2)))

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
  [^String file-name] []
  (second (re-find extract-name-rexexp file-name)))

(comment (filename->joystick-name "usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick"))

(defn file->symlink
  "Given a file, return a map of the link and the target it points to.
  {:link /dev/input/by-id/{symlink} :target /dev/input/{target}}"
  [^File path]
  {:link (.toString path) :target (symlink->target path)})

(comment (file->symlink (io/file "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-event-joystick")))

(defn split-usb-pci
  "Given a path in /dev/input/by-path, return the pci and usb addresses."
  [^String path]
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

(defn edit-distance-between-pairs [device1 device2]
  (-> (e/diff device1 device2 {:alg :a-star :str-diff :character :str-change-limit 0.8})
      (e/get-edits)
      (e/edits->script)
      (e/edit-distance)))

(defn calculate-edit-distances [devices]
  (map (fn [[d1 d2]] (edit-distance-between-pairs d1 d2))
       (partition 2 1 devices)))

(def ediff-test (let [devices   (sort (search-path "/dev/input/by-id" #""))
                      distances (calculate-edit-distances devices)]
                  distances))

(comment (search-path "/dev/input/by-id" #"VPC")

         (let [devices (sort (search-path "/dev/input/by-id" #""))
               d (first devices)
               q (nth devices 4)
               d-q (e/diff d q {:str-diff :character :str-change-limit 0.8})
               v (e/get-edits d-q)
               ds (e/edits->script v)]
           (println d)
           (println q)
           (println d-q)
           (println v)
           (println ds)
           (println (e/get-size d-q))
           (println (e/get-size ds))
           (println (e/edit-distance d-q))
           (println (e/edit-distance ds))

           (data/diff (char-array d) (char-array q))))

(defn prefix [s n]
  (subs s 0 (min n (count s))))

(defn group-by-differences [sorted-paths]
  (let [group-helper (fn group-helper [acc [first & rest :as items] last-prefix]
                       (if (empty? items)
                         (conj acc items)
                         (let [current-prefix (prefix first 15)] ;; Adjust the 15 to control the grouping sensitivity
                           (if (= current-prefix last-prefix)
                             (recur acc rest last-prefix)
                             (recur (conj acc [first]) rest current-prefix)))))
        grouped      (group-helper [] sorted-paths "")]
    (remove empty? grouped)))

(def grouped-paths (group-by-differences (sort (search-path "/dev/input/by-id" #"VPC"))))

(defn regex-search->id-symlinks
  "Given a regex, return a map of the path and the symlink it points to."
  [^Pattern regex]
  (->> (search-path (:by-id device-paths) regex)
       (map io/file)
       (mapv file->symlink)))

(comment (regex-search->id-symlinks #"VPC"))

(defn get-all-devices []
  (regex-search->id-symlinks (re-pattern ".*")))

(defn correlate-joystick-links []
  {:evdev  (first (regex-search->id-symlinks (re-pattern evdev-regexp)))
   :joydev (first (regex-search->id-symlinks (re-pattern joydev-regexp)))})

(comment (correlate-joystick-links))

(defn correlate-joystick-name [joystick-name]
  {:name   joystick-name
   :evdev  (first (regex-search->id-symlinks (re-pattern (str "_" joystick-name ".*" evdev-regexp))))
   :joydev (first (regex-search->id-symlinks (re-pattern (str "_" joystick-name ".*" joydev-regexp))))})

(comment (first (regex-search->id-symlinks (re-pattern (str "VPC" ".*" joydev-regexp))))
         (first (regex-search->id-symlinks (re-pattern (str "VPC" ".*" evdev-regexp))))
         (into {} (mapv correlate-joystick-name example-joystick-names)))

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
        joydev-info (get joystick-map :joydev-info {})
        keys-in-order (concat (keys (dissoc joystick-map :evdev-info :joydev-info))
                              (keys evdev-info)
                              (keys joydev-info))
        order-map (zipmap keys-in-order (range))
        comparator (fn [k1 k2] (compare (order-map k1) (order-map k2)))]
    (into (sorted-map-by comparator)
          (merge (dissoc joystick-map :evdev-info :joydev-info) evdev-info joydev-info))))

(defn keep-map-order [map function]
  (let [keys-in-order (keys map)
        order-map (zipmap keys-in-order (range))
        comparator (fn [k1 k2] (compare (order-map k1) (order-map k2)))]
    (into (sorted-map-by comparator)
          (function map))))

(comment (map promote-children (process-all-joysticks)))

(defn promote-and-order [joystick-map]
  (let [name         {:name (get joystick-map :name)}
        without-name (dissoc joystick-map :name)
        evdev-info   (get without-name :evdev-info {})
        joydev-info  (get without-name :joydev-info {})]
    (-> without-name
        (dissoc :evdev-info :joydev-info)
        (merge evdev-info joydev-info)
        (merge name))))

(comment (map promote-and-order (process-all-joysticks)))

(comment (dissoc-in (first (process-all-joysticks)) [:evdev-info]))

(comment (defn sort-joysticks [joystick-map]
           (sort-by :name joystick-map)))

(comment
  (def temp-file (process-all-joysticks))
  (def temp-file2 (read-string (slurp (io/resource "/2023-07-18T22:55:51.210155120_joystick_device_map.edn")))))

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
  (let [evdev-physical-path  (get-in entry [:evdev-info :evdev-physical-path])
        joydev-physical-path (get-in entry [:joydev-info :joydev-physical-path])
        usb-address          (or (-> (split-usb-pci evdev-physical-path) :usb-address)
                                 (-> (split-usb-pci joydev-physical-path) :usb-address))]
    (assoc entry :usb-address usb-address)))

(defn process-files-usb-fix! [file-path]
  (let [data         (read-edn-file file-path)
        updated-data (map update-usb-address data)]
    (write-edn-file! file-path updated-data)))

(defn process-files-flatten-map! [file-path]
  (let [data         (read-edn-file file-path)
        updated-data (map promote-children data)]
    (write-edn-file! file-path updated-data)))

(defn process-files-sort-maps! [file-path]
  (let [data         (read-edn-file file-path)
        sorted-data  (sort-by :name data)
        updated-data (map #(into (sorted-map-by key-comparator) %) sorted-data)]
    (write-edn-file! file-path updated-data)))

(defn list-edn-files []
  (filter #(re-matches #".*_joystick_device_map\.edn$" (.getPath %))
          (file-seq (clojure.java.io/file "resources"))))

(defn update-all-files! []
  (doseq [file (list-edn-files)]
    (process-files-usb-fix! (.getPath file))))

(defn promote-all-files! []
  (doseq [file (list-edn-files)]
    (process-files-flatten-map! (.getPath file))))

(defn sort-all-files! []
  (doseq [file (list-edn-files)]
    (process-files-sort-maps! (.getPath file))))

(defn parse-timestamp [timestamp-str]
  (LocalDateTime/parse timestamp-str))

(defn extract-timestamp-from-filename [filename]
  (re-find #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+" filename))

(defn generate-joystick-table-data [file-path]
  (let [timestamp-str (extract-timestamp-from-filename file-path)
        date          timestamp-str
        joystick-data (read-edn-file file-path)]            ; Replace with your EDN reading function
    (map (fn [joystick]
           {:date   date
            :name   (:name joystick)
            :pci    (:pci-address joystick)
            :usb    (:usb-address joystick)
            :joydev (str/replace (get-in joystick [:joydev-info :joydev-symlink-target])
                                 "/dev/input/" "")
            :evdev  (str/replace (get-in joystick [:evdev-info :evdev-symlink-target])
                                 "/dev/input/" "")})
         joystick-data)))

(defn generate-all-joysticks-table-data [file-paths]
  (mapcat generate-joystick-table-data file-paths))

(defn print-joystick-info [file-path]
  (let [timestamp-str (extract-timestamp-from-filename file-path)
        date          timestamp-str
        joystick-data (read-edn-file file-path)             ; Replace with your EDN reading function
        table-data    (map #(assoc % :date date
                                   :evdev-symlink-target (get-in % [:evdev-info :evdev-symlink-target]))
                           joystick-data)]
    (pprint/pprint table-data)))

(defn print-all-joysticks-info [file-paths]
  (doseq [file-path file-paths]
    (print-joystick-info file-path)))

(generate-all-joysticks-table-data (map #(.getPath %) (list-edn-files)))

(comment (print-all-joysticks-info (map #(.getPath %) (list-edn-files))))

(comment (def data (slurp "/home/dave/Projects/joystick_fixer/resources/2023-07-18T22:55:51.210155120_joystick_device_map.edn"))
         (def data2 (slurp "/home/dave/Projects/joystick_fixer/resources/2023-07-18T23:14:08.453350096_joystick_device_map.edn"))

         (transform-data (read-string data2)))

(comment (map promote-children (process-all-joysticks)))

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
(defn remap-joystick-nums [xml-string])

(defn read-edn-file [file]
  (with-open [r (io/reader file)]
    (let [pb-reader (java.io.PushbackReader. r)
          read-next #(edn/read {:eof ::eof} pb-reader)]
      (loop [data (read-next)
             acc []]
        (if (= data ::eof)
          acc
          (recur (read-next) (conj acc data)))))))

(def catted-edn (slurp (io/resource "cat_joystick.edn")))

(comment  (read-edn-file (io/resource "cat_joystick.edn")))

(require '[clojure.data.csv :as csv])

(defn write-csv [data filename]
  (with-open [writer (io/writer filename)]
    (let [all-keys (->> data (mapcat (comp keys first))
                        distinct)
          headers (cons "reboot-id" all-keys)
          rows (mapcat (fn [group records]
                         (map (fn [record]
                                (cons group (map (fn [k] (get record k ""))
                                                 all-keys)))
                              records))
                       (range) data)]
      (csv/write-csv writer (cons headers rows)))))

(def edn-data (read-edn-file (io/resource "cat_joystick.edn")))
(write-csv edn-data "output.csv")

(defn -main
  "If passed the -s argument, saves the output to a timestamped file in the resources directory.
   Otherwise, simply pprint the output."
  [& args]
  (let [joystick-map (map promote-children
                          (process-all-joysticks))
        sorted-data  (sort-by :name joystick-map)
        updated-data (map #(into (sorted-map-by key-comparator) %)
                          sorted-data)]
    (if (some #(= "-s" %) args)
      (let [timestamp (str (LocalDateTime/now))
            file-name (str "resources/" timestamp "_joystick_device_map.edn")]
        (spit file-name (binding [clojure.pprint/*print-right-margin* 180]
                          (with-out-str (pprint updated-data)))))
      (pprint updated-data))))
