(ns aeonik.controlmap.gui.svg-viewer
  "SVG viewing and manipulation utilities for JavaFX"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [cljfx.api :as fx])
  (:import
   [javafx.scene.paint Color]
   [javafx.scene Node Parent]
   [javafx.scene.shape Shape]
   [javafx.scene.image ImageView]
   [javafx.scene.layout Pane]
   [javafx.scene.transform Scale]
   [javafx.scene.shape Rectangle]
   [javafx.scene CacheHint]
   [javafx.event EventHandler Event]
   [org.girod.javafx.svgimage SVGLoader SVGImage SVGImageRegion]))

;; =============================================================================
;; SVG Node Manipulation
;; =============================================================================

(defn make-buttons-transparent!
  "Makes button background elements transparent while keeping them pickable"
  [^SVGImage img]
  (let [selectors [".button-bg" ".button-box" ".button-rect" "#button-bg" "#button-box"]
        targets   (->> selectors
                       (mapcat #(seq (.lookupAll img %)))
                       (remove nil?))]
    (doseq [^Node n targets]
      (cond
        (instance? Shape n)
        (let [s ^Shape n]
          ;; Use transparent color, not nil, so the shape remains pickable.
          (.setFill s Color/TRANSPARENT)
          (.setStroke s Color/TRANSPARENT)
          (.setPickOnBounds s false))

        (instance? ImageView n)
        ;; In case the "button" is an <image>, just hide it.
        (.setOpacity ^ImageView n 0.0)))))

(defn debug-list-svg-nodes!
  "Debug function to list all SVG nodes with their IDs and classes"
  [^Parent p]
  (doseq [n (.lookupAll p "*")]
    (println "node" (.getId n) "classes" (.getStyleClass n) "type" (class n))))

(defn paint!
  "Applies custom styling to SVG elements using CSS selectors"
  [^SVGImage img]
  ;; Try CSS lookup first (classes/ids from the SVG map to JavaFX style classes/ids)
  (doseq [n (.lookupAll img ".button-bg")]
    (when (instance? javafx.scene.shape.Shape n)
      (doto ^javafx.scene.shape.Shape n
        (.setFill   (Color/web "#1e1e1e"))
        (.setStroke (Color/web "#ff4d4d"))
        (.setStrokeWidth 1.5))))
  ;; repeat for other classes if needed
  img)

;; =============================================================================
;; SVG Region Creation
;; =============================================================================

(defn svg-region
  "Creates a resizable SVG region from a file path"
  ^SVGImageRegion [^String path]
  (let [^SVGImage img (SVGLoader/load (io/file path))]
    (.createRegion img))) ; resizes with parent, preserves aspect

(defn svg-region-from-string
  ^org.girod.javafx.svgimage.SVGImageRegion [^String svg]
  (let [^org.girod.javafx.svgimage.SVGImage img
        (org.girod.javafx.svgimage.SVGLoader/load svg)]
    (.createRegion img)))

;; =============================================================================
;; SVG Utilities
;; =============================================================================

(defn find-button-id
  "Finds the button ID by traversing up the node hierarchy"
  ^String [^Node n]
  (loop [m n]
    (when m
      (if (and (.getId m)
               (.contains (.getStyleClass m) "button-box"))
        (.getId m)
        (recur (.getParent m))))))

(defn viewbox-wh
  "Returns {:w .. :h ..} from the SVG string (defaults to 1000x1000 if missing)."
  ^java.util.Map [^String s]
  (if-let [[_ a b c d]
           (re-find #"viewBox\s*=\s*\"(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)\"" s)]
    {:w (Double/parseDouble c) :h (Double/parseDouble d)}
    {:w 1000.0 :h 1000.0}))

(defn normalize-svg-for-fx
  "Normalizes SVG string for JavaFX compatibility"
  [s]
  (let [;; unwrap any <html> wrapper so root is <svg …>
        s (-> s
              (str/replace #"(?is)^.*?<svg" "<svg")
              (str/replace #"(?is)</svg>.*$" "</svg>"))
        ;; fix case of viewBox
        s (str/replace s #"(?i)\bviewbox\b" "viewBox")
        ;; ensure xlink namespace if we decide to use xlink:href
        s (if (re-find #"xmlns:xlink" s)
            s
            (str/replace s #"(?i)<svg\b" "<svg xmlns:xlink=\"http://www.w3.org/1999/xlink\""))
        ;; (optional) make <image href=…> also available as xlink:href=…
        s (if (re-find #"xlink:href" s)
            s
            (str/replace s #"(?i)(<image\b[^>]*?)\bhref="
                         "$1xlink:href="))]
    s))

(defn strip-script
  "Removes script tags from SVG string"
  [s]
  (str/replace s #"<script\b[^>]*>[\s\S]*?</script>" ""))

(defn inline-css-vars
  "Replaces CSS custom properties with fallback values"
  [s]
  ;; fill: var(--x, #ff0) -> fill: #ff0  (keeps other props)
  (str/replace s #"fill:\s*var\(\s*--[^,]+,\s*([^)]+)\)"
               (fn [[_ fallback]] (str "fill:" (str/trim fallback)))))

;; =============================================================================
;; SVG Pane Creation
;; =============================================================================

(defn svg-pane-str
  "Creates a simple SVG pane from string with click handling"
  [{:keys [^String svg on-click]}]
  {:fx/type fx/ext-instance-factory
   :create (fn []
             (let [^SVGImage img (SVGLoader/load svg)        ;; ← loads directly from STRING
                   ^SVGImageRegion region (.createRegion img)]
               (.setOnMouseClicked region
                                   (reify EventHandler
                                     (handle [_ e]
                                       (when-let [bid (some-> (.getTarget e) find-button-id)]
                                         (when on-click (on-click bid))))))
               region))})

(defn svg-pane-str-scaled
  "Creates a scaled SVG pane with various scaling modes"
  [{:keys [^String svg on-click mode] :or {mode :contain}}]
  {:fx/type fx/ext-instance-factory
   :create
   (fn []
     (let [^SVGImage img (SVGLoader/load svg)
           {:keys [w h]} (viewbox-wh svg)
           scale-xf (Scale. 1.0 1.0 0.0 0.0) ; scaleX, scaleY, pivotX=0, pivotY=0
           pane (proxy [Pane] []
                  (computePrefWidth  [_] (double w))
                  (computePrefHeight [_] (double h))
                  (computeMinWidth  [_] 0.0)
                  (computeMinHeight [_] 0.0)
                  (layoutChildren []
                    (let [pw (.getWidth this)
                          ph (.getHeight this)
                          sx (/ pw w)
                          sy (/ ph h)
                          s  (case mode
                               :contain    (min sx sy)
                               :cover      (max sx sy)
                               :fit-width  sx
                               :fit-height sy)
                          aw (* w s)
                          ah (* h s)
                          center? (#{:contain :cover} mode)
                          ox (if center? (/ (- pw aw) 2.0) 0.0)
                          oy (if center? (/ (- ph ah) 2.0) 0.0)]

                      (.setX scale-xf s)
                      (.setY scale-xf s)
                      (.setLayoutX img ox)
                      (.setLayoutY img oy)
                      ;; clip only for cover
                      (.setClip this (when (= mode :cover)
                                       (Rectangle. 0 0 pw ph))))))]
       ;; attach transform and some perf hints
       (doto (.getTransforms img)
         (.setAll (into-array javafx.scene.transform.Transform [scale-xf])))
       (make-buttons-transparent! img)
       (.setManaged img false)
       (.setCache img true)
       (.setCacheHint img CacheHint/SCALE)

       ;; events
       (.setOnMouseClicked img
                           (reify EventHandler
                             (^void handle [_ ^Event e]
                               (when-let [bid (some-> (.getTarget e) find-button-id)]
                                 (when on-click (on-click bid))))))

       (doto (.getChildren pane) (.setAll (into-array Node [img])))
       pane))})

;; =============================================================================
;; Development Helpers
;; =============================================================================

(comment
  ;; Test SVG loading
  (def test-svg "<svg viewBox=\"0 0 100 100\"><rect x=\"10\" y=\"10\" width=\"80\" height=\"80\"/></svg>")
  (def region (svg-region-from-string test-svg))

  ;; Test viewbox parsing
  (viewbox-wh test-svg)

  ;; Test SVG normalization
  (normalize-svg-for-fx "<html><svg viewbox=\"0 0 100 100\"></svg></html>"))
