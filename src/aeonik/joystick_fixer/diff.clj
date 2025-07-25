(ns aeonik.joystick-fixer.diff
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data :as data]
            [clojure.edn :as edn]
            [lambdaisland.deep-diff2 :as ddiff]))

(def resource-dir "resources")

(defn joystick-map-files []
  (->> (file-seq (io/file resource-dir))
       (filter #(re-matches #"\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}\.\d+_joystick_device_map\.edn" (.getName %)))
       (sort-by #(.getName %) #(compare %2 %1))))

(defn read-edn-file [file]
  (with-open [rdr (io/reader file)]
    (edn/read-string (slurp rdr))))

(defn diff-last-two []
  (let [files (joystick-map-files)
        last-two (take-last 2 files)]
    (when (= 2 (count last-two))
      (let [file-a (first last-two)
            file-b (second last-two)
            data-a (read-edn-file file-a)
            data-b (read-edn-file file-b)
            diff   (ddiff/diff data-a data-b)]
        {:files [(.getName file-a) (.getName file-b)]
         :diff diff}))))
;; Example usage:
(ddiff/pretty-print (diff-last-two))
