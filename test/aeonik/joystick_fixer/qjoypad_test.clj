(ns aeonik.joystick-fixer.qjoypad-test
  (:require [clojure.test :refer :all])
  (:require [aeonik.joystick-fixer.qjoypad :refer [qjoypad-ebnf parse-qjoypad serialize-parse-tree]]
            [clojure.java.io :as io]
            [clj-commons.digest]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(defn file-stats [file-contents]
  {:size (count file-contents)
   :hash (->> file-contents
              (clj-commons.digest/digest "sha-256"))})

(defn diff-files [file1 file2]
  (let [lines1 (str/split-lines file1)
        lines2 (str/split-lines file2)]
    (for [[l1 l2] (map vector lines1 lines2) :when (not= l1 l2)]
      {:file1 l1 :file2 l2})))

(deftest serialize-parse-tree-test
  (let [qjoypad-file (slurp "resources/virpil_maps.lyt")
        parsed-file (parse-qjoypad qjoypad-file :grammar qjoypad-ebnf :output-format :enlive :unhide :all)
        serialized-file (serialize-parse-tree parsed-file)]
    (let [file-stats1 (file-stats qjoypad-file)
          file-stats2 (file-stats serialized-file)]
      (println "========== QJoypad File Stats ==========")
      (pprint file-stats1)
      (println "\n========== Serialized File Stats ==========")
      (pprint file-stats2))
    (if-not (= qjoypad-file serialized-file)
      (do
        (println "\n========== Differing Lines ==========")
        (pprint (diff-files qjoypad-file serialized-file)))
      (is (= qjoypad-file serialized-file)))))
