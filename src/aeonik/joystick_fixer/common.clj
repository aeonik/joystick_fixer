(ns aeonik.joystick-fixer.common
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.pprint :refer [pprint]]))

(defn diff-lines [file1 file2]
  (let [lines1 (str/split-lines file1)
        lines2 (str/split-lines file2)]
    (for [[l1 l2] (map vector lines1 lines2) :when (not= l1 l2)]
      {:file1 l1 :file2 l2})))

(defn serialize-parse-tree
  "Unparses the tree
  This function currently requires enlive format"
  [tree]
  (cond
    (map? tree) (serialize-parse-tree (get tree :content))
    (coll? tree) (reduce str (map serialize-parse-tree tree))
    :else tree))