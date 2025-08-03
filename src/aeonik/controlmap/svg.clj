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
  (let [[class-key joystick-data] (first data)]
    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :class (name class-key)}
     (svg-elements joystick-data)]))

(comment
  (apply str (html/emit* (html/html (coordinates->svg-hiccup state/svg-edn-files->map))))

  (coordinates->svg-hiccup state/svg-edn-files->map))

(def hiccup-svg
  (coordinates->svg-hiccup state/svg-edn-files->map))
