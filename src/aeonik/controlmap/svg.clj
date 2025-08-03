(ns aeonik.controlmap.svg
  (:require [aeonik.controlmap.index :as index]
            [aeonik.controlmap.state :as state]
            [net.cgrand.enlive-html :as html]))

(defn make-rect
  [{:keys [id x y]} {:keys [width height rx ry]}]
  [:rect {:x x
          :y y
          :width width
          :height height
          :rx rx
          :ry ry
          :id (str id "-rect")}])

(defn make-text
  [{:keys [id x y]}]
  [:text {:x x :y y :id id} id])

(defn coordinate-pair->elements
  [coord rect-dims]
  [(make-rect coord rect-dims)
   (make-text coord)])

(defn svg-elements
  [{:keys [text-coordinates button-rect-dimensions]}]
  (->> text-coordinates
       vals
       (mapcat #(coordinate-pair->elements % button-rect-dimensions))))

(defn coordinates->svg-hiccup
  [data]
  [:svg {:xmlns "http://www.w3.org/2000/svg"}
   (svg-elements data)])

(apply str (html/emit* (html/html (coordinates->svg-hiccup (:alpha_R state/svg-edn-files->map)))))

(def hiccup-svg
  (coordinates->svg-hiccup (:alpha_R state/svg-edn-files->map)))

state/svg-edn-files->map
(apply str ((comp html/emit* html/html) (:alpha_R state/svg-edn-files->map)))
(apply str (html/emit* (html/html [:svg {:xmlns "http://www.w3.org/2000/svg"} (:alpha_R state/svg-edn-files->map)])))
