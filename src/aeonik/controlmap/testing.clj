(ns aeonik.controlmap.testing
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [hickory.core :as h]
   [hickory.render :as render]
   [hickory.select :as s]
   [hickory.zip :as hzip]
   [hickory.convert :as hconvert]
   [clojure.zip :as zip]
   [clojure.data.xml :as xml]))

#_(comment
    ;; SVG Schema
    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :xmlns:xlink "http://www.w3.org/1999/xlink"
           :id "panel"
           :version "1.1"
           :viewBox "0 0 1341 948"
           :width "1341"
           :height "948"
           :style "background-color:#1e1e1e;"}
     ;; Background rectangle
     [:rect {:x "0" :y "0" :width "100%" :height "100%" :fill "#1e1e1e"}]

     ;; Layer groups
     [:g {:id "button-box-layer"
          :style "pointer-events:none"}]

     [:g {:id "button-text-layer"
          :style "font-family:'Segoe UI', sans-serif; font-size:18px; fill:white; pointer-events:none"}]

     [:g {:id "joystick-outline-layer"
          :style "fill:none;stroke:white;stroke-width:2;opacity:0.2"}]

     ;; Example symbol reference
     [:use {:xlink:href "#joystick-outline" :x "0" :y "0"}]]

    (render/hiccup-to-html '([:html {} [:head {}] [:body {} [:a {} "foo"]]]))

    (h/as-hickory (h/parse (slurp "resources/actionmaps.xml")))

    (xml/parse-str (slurp "resources/actionmaps.xml"))

    (into [:ul]
          (map (fn [i] [:li i]) "test"))

    (into [:ul]
          (map #(vector :li %) "test")))
