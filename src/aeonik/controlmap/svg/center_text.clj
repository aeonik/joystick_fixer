(ns aeonik.controlmap.svg.center-text
  "Utility for centering text elements within their corresponding button rectangles in SVG files."
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.xml :as xml]))

;; -----------------------------------------------------------------------------
;; Parsing and Formatting
;; -----------------------------------------------------------------------------

(defn parse-number
  "Parse a numeric value from a string, handling SVG unit suffixes."
  [s]
  (when (string? s)
    (try
      (Double/parseDouble (str/replace s #"[^\d\.\-\+eE]" ""))
      (catch NumberFormatException _ nil))))

(defn format-coordinate
  "Format a coordinate value with appropriate precision."
  [^double x]
  (format "%.3f" x))

;; -----------------------------------------------------------------------------
;; XML Node Predicates and Accessors
;; -----------------------------------------------------------------------------

(defn element?
  "Check if a node is an XML element."
  [node]
  (and (map? node)
       (keyword? (:tag node))))

(defn button-rect?
  "Check if an element is a button rectangle."
  [element]
  (and (element? element)
       (= :rect (:tag element))
       (= "button-box" (get-in element [:attrs :class]))))

(defn text-element-to-center?
  "Check if an element is a text element that should be centered."
  [element]
  (and (element? element)
       (= :text (:tag element))
       (get-in element [:attrs :data-for])))

;; -----------------------------------------------------------------------------
;; Tree Walking
;; -----------------------------------------------------------------------------

(defn walk-elements
  "Walk all elements in the document tree."
  [root]
  (tree-seq
   element?
   (fn [el]
     (filter element? (:content el)))
   root))

;; -----------------------------------------------------------------------------
;; Geometry Calculations
;; -----------------------------------------------------------------------------

(defn calculate-center
  "Calculate the center point of a rectangle."
  [{:keys [x y width height]}]
  (let [x-val (parse-number x)
        y-val (parse-number y)
        w-val (parse-number width)
        h-val (parse-number height)]
    (when (every? some? [x-val y-val w-val h-val])
      {:cx (+ x-val (/ w-val 2.0))
       :cy (+ y-val (/ h-val 2.0))})))

(defn extract-rect-centers
  "Extract center points for all button rectangles, indexed by ID."
  [document]
  (->> (walk-elements document)
       (filter button-rect?)
       (keep (fn [rect]
               (when-let [id (get-in rect [:attrs :id])]
                 (when-let [center (calculate-center (:attrs rect))]
                   [id center]))))
       (into {})))

;; -----------------------------------------------------------------------------
;; Text Centering Transform
;; -----------------------------------------------------------------------------

(defn center-text-attrs
  "Update text element attributes to center it at the given coordinates."
  [attrs {:keys [cx cy]}]
  (-> attrs
      (assoc :x (format-coordinate cx)
             :y (format-coordinate cy)
             :text-anchor "middle"
             :dominant-baseline "middle")))

(defn transform-element
  "Transform an element, centering text elements that reference button rectangles."
  [element centers]
  (cond
    ;; Not an element - return as-is (handles text content, strings, etc.)
    (not (element? element))
    element

    ;; Text element with data-for reference - center it
    (text-element-to-center? element)
    (let [rect-id (get-in element [:attrs :data-for])
          center (get centers rect-id)]
      (if center
        ;; Update attributes while preserving content
        (assoc element :attrs (center-text-attrs (:attrs element) center))
        element))

    ;; Element with children - recursively transform
    :else
    (if-let [content (:content element)]
      (assoc element :content
             (mapv #(transform-element % centers) content))
      element)))

;; -----------------------------------------------------------------------------
;; File Processing
;; -----------------------------------------------------------------------------

(defn process-svg
  "Process an SVG document to center text elements within their button rectangles."
  [input-stream output-stream]
  ;; Parse with namespace handling
  (let [document (xml/parse input-stream :namespace-aware false)
        _ (println "Parsed document with root tag:" (:tag document))
        centers (extract-rect-centers document)
        _ (println "Found" (count centers) "rectangles to use as centers")
        transformed (transform-element document centers)]
    (xml/emit transformed output-stream)))

(defn process-file!
  "Process an SVG file, reading from input-path and writing to output-path."
  [input-path output-path]
  (println (str "Processing: " input-path " -> " output-path))
  (with-open [reader (io/reader input-path)
              writer (io/writer output-path)]
    (process-svg reader writer))
  (println "Done!"))

;; -----------------------------------------------------------------------------
;; Debugging Utilities
;; -----------------------------------------------------------------------------

(defn debug-structure
  "Debug function to show document structure."
  [input-path]
  (with-open [reader (io/reader input-path)]
    (let [document (xml/parse reader :namespace-aware false)]
      (println "Document structure:")
      (println "Root tag:" (:tag document))
      (println "Root attrs:" (:attrs document))
      (println "Number of top-level children:" (count (:content document)))
      (println "\nTop-level children tags:")
      (doseq [child (:content document)]
        (when (element? child)
          (println "  -" (:tag child)
                   (when-let [id (get-in child [:attrs :id])]
                     (str "(id=" id ")"))
                   (when-let [cls (get-in child [:attrs :class])]
                     (str "(class=" cls ")")))))

      ;; Look deeper for g elements
      (println "\nLooking for <g> elements and their children:")
      (doseq [g-elem (filter #(and (element? %) (= :g (:tag %)))
                             (walk-elements document))]
        (println "  Found <g> with id:" (get-in g-elem [:attrs :id]))
        (doseq [child (:content g-elem)]
          (when (element? child)
            (println "    -" (:tag child)
                     "id=" (get-in child [:attrs :id])
                     "class=" (get-in child [:attrs :class])))))
      document)))

(defn debug-all-rects
  "Debug function to find ALL rect elements."
  [input-path]
  (with-open [reader (io/reader input-path)]
    (let [document (xml/parse reader :namespace-aware false)
          all-rects (->> (walk-elements document)
                         (filter #(and (element? %) (= :rect (:tag %)))))]
      (println "Found" (count all-rects) "total <rect> elements:")
      (doseq [rect all-rects]
        (let [attrs (:attrs rect)]
          (println (format "  id=%s, class=%s, x=%s, y=%s"
                           (:id attrs)
                           (:class attrs)
                           (:x attrs)
                           (:y attrs))))
        all-rects))))

(defn debug-rect-centers
  "Debug function to print found rectangles and their centers."
  [input-path]
  (with-open [reader (io/reader input-path)]
    (let [document (xml/parse reader :namespace-aware false)
          centers (extract-rect-centers document)]
      (println "Found" (count centers) "button rectangles:")
      (doseq [[id {:keys [cx cy]}] centers]
        (println (format "  %s -> center: (%.2f, %.2f)" id cx cy)))
      centers)))

(defn debug-text-elements
  "Debug function to print text elements that will be centered."
  [input-path]
  (with-open [reader (io/reader input-path)]
    (let [document (xml/parse reader :namespace-aware false)
          text-elements (->> (walk-elements document)
                             (filter text-element-to-center?))]
      (println "Found" (count text-elements) "text elements to center:")
      (doseq [el text-elements]
        (let [attrs (:attrs el)
              content (:content el)]
          (println (format "  id=%s, data-for=%s, x=%s, y=%s, content=%s"
                           (:id attrs)
                           (:data-for attrs)
                           (:x attrs)
                           (:y attrs)
                           (pr-str content)))))
      text-elements)))

;; -----------------------------------------------------------------------------
;; Example Usage
;; -----------------------------------------------------------------------------

(comment
  ;; First, debug the structure to understand what's being parsed
  (debug-structure "resources/svg/sharka-50_panel.svg")

  ;; Find all rect elements
  (debug-all-rects "resources/svg/sharka-50_panel.svg")

  ;; Debug - see what rectangles are found
  (debug-rect-centers "resources/svg/sharka-50_panel.svg")

  ;; Debug - see what text elements will be centered
  (debug-text-elements "resources/svg/sharka-50_panel.svg")

  ;; Process a single file
  (process-file! "resources/svg/sharka-50_panel.svg"
                 "resources/svg/sharka-50_panel_centered.svg"))
