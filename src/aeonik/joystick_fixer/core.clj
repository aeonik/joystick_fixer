(ns aeonik.joystick-fixer.core
  (:require [clojure.java.io :as io])
  (:import (org.apache.commons.io FileNameUtils)))

;; List all joystick devices by id, the location is /dev/input/by-id get the absolte path, and the link that the symlink points to
(defn get-joystick-by-id []
  (->> (clojure.java.io/file "/dev/input/by-id")
       (.listFiles)
       (map #(.getName %))
       (filter #(re-find #"event-joystick" %))
       (map #(str "/dev/input/by-id/" %))))

(defn list-joysticks-by-id []
  (->> (clojure.java.io/file "/dev/input/by-id")
       (.listFiles)
       (map #(.getName %))
       (filter #(re-find #"event-joystick" %))))

;; List all joystick devices by path, the location is /dev/input/by-path
(defn list-joysticks-by-path []
  (->> (clojure.java.io/file "/dev/input/by-path")
       (.listFiles)
       (map #(.getName %))
       (filter #(re-find #"event-joystick" %))))

;; Get all joystick names and paths, Joystick names are the keys, paths are the a list of values

(defn get-joystick-names-and-paths []
  (let [joysticks-by-id (list-joysticks-by-id)
        joysticks-by-path (list-joysticks-by-path)]
    (zipmap joysticks-by-id joysticks-by-path)))
