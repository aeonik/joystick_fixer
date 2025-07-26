(ns aeonik.joystick-fixer.diff
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.data :as data]
            [clojure.edn :as edn]
            [clojure.pprint :refer [pprint]]
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

(defn pretty-print-last-two-diffs []
  (ddiff/pretty-print (diff-last-two)))

(defn print-summary []
  (let [orig-diff (:diff (diff-last-two))
        minimized (ddiff/minimize (diff-last-two))
        output    (assoc minimized
                    :diff
                    (map (fn [min entry]
                           (assoc min :name (:name entry)))
                         (:diff minimized)
                         orig-diff))]
    (ddiff/pretty-print output)))

(defn -main [& args]
  (let [result (diff-last-two)]
    (if (some #(= "--summary" %) args)
      (print-summary)
      (ddiff/pretty-print result))))
