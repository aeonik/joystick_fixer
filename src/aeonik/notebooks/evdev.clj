^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns aeonik.notebooks.evdev
  {:nextjournal.clerk/visibility {:code :show :result :hide}}
  (:require [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [clojure.data :as data]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.pprint :refer [pprint]]
            [clojure.tools.reader.edn :as edn]
            [editscript.core :as e]
            [malli.core :as m]
            [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [tupelo.core :refer :all]
            [ubergraph.core :as uber]
            [zprint.core :as z]
            [ubergraph.core :as ug]
            [medley.core :refer [deep-merge map-kv]])
  (:import (clojure.lang Keyword)
           (java.io File)
           [java.nio.file Files Paths]
           (java.time LocalDateTime)
           (java.util.regex Pattern)
           (org.apache.commons.io FileUtils)))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(def zprint-code-viewer
  {:name         `zprint-code-viewer
   :render-fn    'nextjournal.clerk.render/render-code
   :transform-fn (comp v/mark-presented
                       #(update-in % [:nextjournal/render-opts :language] (fn [lang] (or lang "clojure")))
                       (clerk/update-val (fn [v] (str/trim (with-out-str (z/zprint v {:map {:comma? true :indent 0 :justify? true}}))))))})


(def evdev-regexp #"-event-joystick$")
(def joydev-regexp #"(?<!-event)-joystick$")
(def evdev-path-regexp #"event\d+$")
(def joydev-path-regexp #"js\d+$")
(def device-paths {:by-id   "/dev/input/by-id"
                   :by-path "/dev/input/by-path"})
(def joystick-name-regexp #"VPC")
(def extract-virpil-name-rexexp #"usb-VIRPIL_Controls_20220720_(.*?)_FF(-event)?-joystick")
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

(defn read-edn-file [file-path]
  (with-open [rdr (clojure.java.io/reader file-path)]
    (edn/read-string (slurp rdr))))

(defn write-edn-file! [file-path data]
  (spit file-path (binding [clojure.pprint/*print-right-margin* 200]
                    (with-out-str (pprint/pprint data)))))

(defn search-path
  "Given a path and a regex, return a list of files that match the regex."
  [^String path ^Pattern regex]
  (->> (io/file path)
       (.listFiles)
       (filter #(re-find regex (.getName %)))
       (mapv #(.getAbsolutePath %))))

^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{::clerk/auto-expand-results? true}
(def paths (search-path (:by-path device-paths) #""))

^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{::clerk/auto-expand-results? true}
(def ids (search-path (:by-id device-paths) #""))

;; #### Utility functions to get the symlinks and their targets from a given file:

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn symlink->target
  "Given a symlink, return the real path of the target file it points to."
  [^File symlink]
  (-> symlink
      .getCanonicalFile
      .toString))

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn file->symlink
  "Given a file, return a map of the link and the target it points to.
  {:link /dev/input/by-id/{symlink} :target /dev/input/{target}}"
  [^File path]
  {(.toString path) (symlink->target path)})

;; #### Printing the physical path links, they are usually PCI paths:
^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{::clerk/viewer zprint-code-viewer ::clerk/auto-expand-results? true}
(def phyiscal-path-links (map (comp file->symlink io/file) paths))


;; #### Printing the id path links, they are usually serial IDs from the vendor:
^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{::clerk/viewer zprint-code-viewer ::clerk/auto-expand-results? true}
(def id-links (map (comp file->symlink io/file) ids))

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn split-usb-pci
  "Given a path in /dev/input/by-path, return the pci and usb addresses."
  [^String path]
  {:pci-address (second (re-find extract-pci-regexp path))
   :usb-address (second (re-find extract-usb-regexp path))})

^{:nextjournal.clerk/visibility {:code :show :result :show}}
(split-usb-pci "/dev/input/by-path/pci-0000:2c:00.3-usbv2-0:2:1.0-event-mouse")

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn assoc-pci-usb
  "Associates the PCI and USB addresses to each record."
  #_{"/dev/input/by-path/pci-0000:2c:00.3-usbv2-0:2:1.0-event-mouse" "/dev/input/event6"}
  [path-map]
  (spyx path-map)
  (let [pci-usb (split-usb-pci (first (keys path-map)))]
    (assoc path-map
      :pci-address (:pci-address pci-usb)
      :usb-address (:usb-address pci-usb))))

^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{::clerk/viewer zprint-code-viewer ::clerk/auto-expand-results? true}
(assoc-pci-usb {"/dev/input/by-path/pci-0000:2c:00.3-usbv2-0:2:1.0-event-mouse" "/dev/input/event6"})

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn assoc-joystick-name
  "Associates the joystick name to each record."
  [path-map]
  (assoc path-map
    :name (second (re-find extract-virpil-name-rexexp (first (keys path-map))))))

;; #### Associating the joystick name to each map, this only works for Virpil at the moment:
^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{::clerk/viewer zprint-code-viewer ::clerk/auto-expand-results? true}
(assoc-joystick-name {"/dev/input/by-id/usb-VIRPIL_Controls_20220720_L-VPC_Stick_MT-50CM2_FF-event-joystick" "/dev/input/event27"})

^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{::clerk/viewer zprint-code-viewer ::clerk/auto-expand-results? true}
(assoc-joystick-name {"/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Stick_MT-50CM2_FF-event-joystick" "/dev/input/event26"})



(def updated-phyiscal-path-links
  (map assoc-pci-usb phyiscal-path-links))



^{:nextjournal.clerk/visibility {:code :show :result :hide}}

(def graph (->> (concat phyiscal-path-links id-links)
                (reduce into [])
                (apply ug/multidigraph)))


^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{::clerk/viewer (assoc v/string-viewer :page-size 50000) ::clerk/auto-expand-results? true}
^{:nextjournal.clerk/width :wide}
(with-out-str (ug/pprint graph))

^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{::clerk/viewer zprint-code-viewer ::clerk/auto-expand-results? true}
(ug/find-edges graph {:dest "/dev/input/js1"})

^{:nextjournal.clerk/visibility {:code :show :result :show}}
^{::clerk/viewer zprint-code-viewer ::clerk/auto-expand-results? true}
(ug/find-edges graph {:src "/dev/input/by-path/pci-0000:2c:00.1-usb-0:1.4.1:1.0-joystick"})
(ug/find-edges graph {:src "/dev/input/by-id/usb-VIRPIL_Controls_20220720_VPC_Throttle_MT-50CM3_FF-joystick"})