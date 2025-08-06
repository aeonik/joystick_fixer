(ns aeonik.controlmap.svg
  "SVG manipulation utilities for control mapping"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [hickory.core :as h]
   [hickory.render :as render]
   [hickory.select :as s]
   [hickory.zip :as hzip]
   [hickory.convert :as hconvert]
   [clojure.zip :as zip])
  (:import
   (java.util Base64)
   (java.nio.file Files)
   (java.io File)))

;; =============================================================================
;; EDN to SVG Conversion (Hiccup/Hickory)
;; =============================================================================

(defn make-rect
  "Creates a rect element in Hiccup format"
  [{:keys [id x y]} {:keys [width height rx ry]}]
  [:rect {:x x
          :y y
          :width width
          :height height
          :rx (or rx 0)
          :ry (or ry 0)
          :id (str id "-rect")
          :data-for id}])

(defn make-text
  "Creates a text element in Hiccup format"
  [{:keys [id x y content]}]
  [:text {:x x
          :y y
          :id (str "lbl_" id)
          :data-for id}
   (or content id)])

(defn make-button-group
  "Creates a group containing rect and text for a button"
  [coord rect-dims]
  [:g {:id (str (:id coord) "-group")
       :class "button-group"}
   (make-rect coord rect-dims)
   (make-text coord)])

(defn edn-config->svg-elements
  "Converts EDN configuration to SVG elements"
  [{:keys [text-coordinates button-rect-dimensions]}]
  (->> text-coordinates
       vals
       (map #(make-button-group % button-rect-dimensions))))

(defn edn-config->svg-hiccup
  "Converts EDN configuration to complete SVG in Hiccup format"
  [[class-key joystick-data]]
  [:svg {:xmlns "http://www.w3.org/2000/svg"
         :class (name class-key)
         :viewBox "0 0 1024 768"}
   [:g {:id "buttons"}
    (edn-config->svg-elements joystick-data)]])

(defn edn-config->hickory
  "Converts EDN configuration to Hickory format"
  [edn-config]
  (-> edn-config
      edn-config->svg-hiccup
      h/as-hickory))

;; =============================================================================
;; Node Update Utilities
;; =============================================================================

(defn update-nodes
  "Updates all nodes in a tree that match the given selector.

   Args:
   - tree: Hickory tree to modify
   - selector: Hickory selector function
   - edit-fn: Function that takes a node and returns modified node

   Options:
   - :first-only? - If true, only update first match (default false)
   - :return-count? - If true, return [tree update-count] (default false)

   Returns modified tree (or [tree count] if :return-count? is true)"
  [tree selector edit-fn & {:keys [first-only? return-count?]
                            :or {first-only? false
                                 return-count? false}}]
  (let [update-count (atom 0)]
    (loop [current-tree tree]
      (let [root-loc (hzip/hickory-zip current-tree)
            found-loc (s/select-next-loc selector root-loc)]
        (if found-loc
          (do
            (swap! update-count inc)
            (let [updated-tree (-> found-loc
                                   (zip/edit edit-fn)
                                   zip/root)]
              (if first-only?
                (if return-count?
                  [updated-tree @update-count]
                  updated-tree)
                (recur updated-tree))))
          (if return-count?
            [current-tree @update-count]
            current-tree))))))

(defn update-nodes-batch
  "Apply multiple selector->edit-fn pairs to a tree.

   Args:
   - tree: Hickory tree to modify
   - updates: Vector of [selector edit-fn] or [selector edit-fn options]

   Returns modified tree"
  [tree updates]
  (reduce (fn [current-tree update-spec]
            (let [[selector edit-fn opts]
                  (if (>= (count update-spec) 3)
                    update-spec
                    [(first update-spec) (second update-spec) {}])]
              (apply update-nodes current-tree selector edit-fn
                     (flatten (seq opts)))))
          tree
          updates))

;; =============================================================================
;; Edit Function Builders
;; =============================================================================

(defn make-content-updater
  "Creates an edit function that updates content"
  [content-or-fn]
  (fn [node]
    (assoc node :content
           (cond
             (fn? content-or-fn) [(content-or-fn node)]
             (vector? content-or-fn) content-or-fn
             :else [content-or-fn]))))

;; TODO: Figure out how to get the tspan out of here, and use the functions below
(defn make-content-updater
  "Creates an edit function that updates content. Supports multiline <tspan> if string contains newlines, <br>, or semicolons."
  [content-or-fn]
  (fn [node]
    (let [x-val (get-in node [:attrs :x])
          content (cond
                    (fn? content-or-fn) (content-or-fn node)
                    :else content-or-fn)]
      (assoc node :content
             (cond
               (vector? content) content
               (string? content)
               (let [lines (str/split content #"(?:\s*<br\s*/?>\s*|\s*;\s*|\s*\n\s*)")]
                 (map-indexed
                  (fn [idx line]
                    {:type :element
                     :tag :tspan
                     :attrs {:x x-val :dy (str (* idx 1.2) "em")}
                     :content [line]})
                  lines))
               :else [content])))))

(defn make-attr-updater
  "Creates an edit function that updates attributes"
  [attr-map]
  (fn [node]
    (update node :attrs merge attr-map)))

(defn make-class-adder
  "Creates an edit function that adds CSS classes"
  [classes]
  (fn [node]
    (update-in node [:attrs :class]
               (fn [existing]
                 (if existing
                   (str existing " " classes)
                   classes)))))

(defn make-class-remover
  "Creates an edit function that removes CSS classes"
  [classes-to-remove]
  (let [remove-set (set (str/split classes-to-remove #"\s+"))]
    (fn [node]
      (update-in node [:attrs :class]
                 (fn [existing]
                   (when existing
                     (->> (str/split existing #"\s+")
                          (remove remove-set)
                          (str/join " "))))))))

(defn compose-edits
  "Chains multiple edit functions together"
  [& edit-fns]
  (fn [node]
    (reduce (fn [n edit-fn] (edit-fn n))
            node
            edit-fns)))

(defn make-tspan
  "Creates a single tspan element in Hickory format"
  [{:keys [text x y dx dy attrs]}]
  {:type :element
   :tag :tspan
   :attrs (merge (cond-> {}
                   x (assoc :x x)
                   y (assoc :y y)
                   dx (assoc :dx dx)
                   dy (assoc :dy dy))
                 attrs)
   :content [(or text "")]})

(defn actions-to-tspans
  "Converts a vector of action strings into tspan elements.
   First action can have a count prefix like [2] if multiple actions."
  [actions & {:keys [x y line-height show-count?]
              :or {line-height "1.2em"
                   show-count? true}}]
  (let [n (count actions)]
    (cond
      ;; No actions
      (empty? actions) []

      ;; Single action - return as simple text
      (= 1 n) [(first actions)]

      ;; Multiple actions - create tspans
      :else
      (map-indexed
       (fn [idx action]
         (let [text (if (and (zero? idx) show-count?)
                      (str "[" n "] " action)
                      action)]
           (make-tspan {:text text
                        :x x
                        :dy (if (zero? idx) "0" line-height)})))
       actions))))

(defn make-multiline-content-updater
  "Creates an edit function that updates content with multi-line support.
   Can accept either a string with separators or a vector of strings."
  [content-or-fn & {:keys [separator line-height]
                    :or {separator #"(?:\n|<br/>?)"
                         line-height "1.2em"}}]
  (fn [node]
    (let [content (cond
                    (fn? content-or-fn) (content-or-fn node)
                    :else content-or-fn)
          x-attr (get-in node [:attrs :x])
          y-attr (get-in node [:attrs :y])]
      (assoc node :content
             (cond
               ;; Vector of strings - convert to tspans
               (vector? content)
               (actions-to-tspans content  ; <-- THIS IS THE KEY CALL
                                  :x x-attr
                                  :y y-attr
                                  :line-height line-height)
               ;; ... other cases
               )))))

;; =============================================================================
;; Image Processing
;; =============================================================================

(def ^:private ext->mime
  "File extension to MIME type mapping"
  {"png"  "image/png"
   "jpg"  "image/jpeg"
   "jpeg" "image/jpeg"
   "gif"  "image/gif"
   "svg"  "image/svg+xml"
   "webp" "image/webp"
   "bmp"  "image/bmp"
   "ico"  "image/x-icon"})

(defn detect-mime-type
  "Detects MIME type from file path or File object"
  [file-or-path]
  (let [file (if (instance? File file-or-path)
               file-or-path
               (io/file file-or-path))]
    (or
     ;; Try Java's built-in detection
     (try
       (Files/probeContentType (.toPath file))
       (catch Exception _ nil))
     ;; Fall back to extension-based detection
     (some-> (str/lower-case (.getName file))
             (re-find #"\.([^.]+)$")
             second
             ext->mime)
     ;; Default
     "application/octet-stream")))

(defn file->base64
  "Converts a file to base64 encoded string"
  [file-or-path]
  (let [file (if (instance? File file-or-path)
               file-or-path
               (io/file file-or-path))]
    (when (.exists file)
      (let [bytes (Files/readAllBytes (.toPath file))]
        (.encodeToString (Base64/getEncoder) bytes)))))

(defn file->data-uri
  "Converts a file to a data URI"
  [file-or-path]
  (let [file (if (instance? File file-or-path)
               file-or-path
               (io/file file-or-path))]
    (when (.exists file)
      (str "data:"
           (detect-mime-type file)
           ";base64,"
           (file->base64 file)))))

(defn resolve-relative-path
  "Resolves a relative path against a base path"
  [relative-path base-path]
  (let [clean-relative (-> relative-path
                           (str/replace #"^\.\./" "")
                           (str/replace #"^\\./" ""))
        base-file (io/file base-path)]
    (.getAbsolutePath
     (io/file (if (.isDirectory base-file)
                base-file
                (.getParentFile base-file))
              clean-relative))))

;; =============================================================================
;; SVG Image Inlining
;; =============================================================================

(defn find-external-images
  "Finds all image elements with external references (href or xlink:href)"
  [hickory-tree]
  (->> (s/select (s/tag :image) hickory-tree)
       (filter (fn [node]
                 (let [href (or (get-in node [:attrs :href])
                                (get-in node [:attrs :xlink:href]))]
                   (and href
                        (not (str/starts-with? href "data:"))
                        (not (str/starts-with? href "http"))))))
       vec))

(defn inline-image
  "Inlines a single image element by converting its href to a data URI"
  [image-node base-path]
  (let [href (or (get-in image-node [:attrs :href])
                 (get-in image-node [:attrs :xlink:href]))
        resolved-path (resolve-relative-path href base-path)
        data-uri (file->data-uri resolved-path)]
    (if data-uri
      (-> image-node
          (assoc-in [:attrs :href] data-uri)
          #_(assoc-in [:attrs :xlink:href] data-uri))
      (do
        (println (format "Warning: Could not inline image: %s" href))
        image-node))))

(defn fix-all-relative-images-base64
  "Inlines all relative image references as base64 data URIs.
   Handles both href and xlink:href attributes for SVG compatibility."
  [hickory-tree base-path]
  (update-nodes hickory-tree
                (s/and (s/tag :image)
                       (s/or (s/attr :href #(and %
                                                 (not (str/starts-with? % "data:"))
                                                 (not (str/starts-with? % "http"))))
                             (s/attr :xlink:href #(and %
                                                       (not (str/starts-with? % "data:"))
                                                       (not (str/starts-with? % "http"))))))
                #(inline-image % base-path)))

;; =============================================================================
;; Rendering Utilities
;; =============================================================================

(defn hickory->svg-string
  "Renders a Hickory tree to an SVG string"
  [hickory-tree]
  (render/hickory-to-html hickory-tree))

(defn hiccup->svg-string
  "Renders Hiccup format to an SVG string"
  [hiccup]
  (-> hiccup
      hconvert/hiccup-to-hickory
      hickory->svg-string))

(defn create-data-url
  "Creates a data URL from content"
  [content mime-type]
  (str "data:" mime-type ";charset=utf-8,"
       (java.net.URLEncoder/encode content "UTF-8")))

(defn create-svg-data-url
  "Creates a data URL specifically for SVG content"
  [svg-content]
  (create-data-url svg-content "image/svg+xml"))

;; =============================================================================
;; SVG Analysis Utilities
;; =============================================================================

(defn find-buttons
  "Finds all button elements in an SVG (elements with data-for attribute)"
  [hickory-tree]
  (s/select (s/attr :data-for) hickory-tree))

(defn count-buttons
  "Counts button elements in an SVG"
  [hickory-tree]
  (count (find-buttons hickory-tree)))

(defn get-button-ids
  "Gets all button IDs from an SVG"
  [hickory-tree]
  (->> (find-buttons hickory-tree)
       (map #(get-in % [:attrs :data-for]))
       (filter some?)
       sort
       vec))

(defn find-unmapped-buttons
  "Finds buttons that don't have associated action text"
  [hickory-tree]
  (->> (find-buttons hickory-tree)
       (filter (fn [node]
                 (let [content (:content node)]
                   (or (empty? content)
                       (every? str/blank? content)))))
       vec))

;; =============================================================================
;; Development Helpers
;; =============================================================================

(comment
  ;; Load some test data
  (require '[aeonik.controlmap.state :as state])
  (def ctx @state/context)
  (def tree (get-in ctx [:svg-roots :alpha_L]))

  ;; Test EDN to SVG conversion
  (def edn-config (get-in ctx [:svg-edn-configs :alpha_L]))
  (when edn-config
    (def svg-from-edn (edn-config->hickory [:alpha_L edn-config])))

  ;; Find all buttons
  (get-button-ids tree)
  (count-buttons tree)

  ;; Find unmapped buttons
  (count (find-unmapped-buttons tree))

  ;; Update button text
  (def updated (update-nodes tree
                             (s/attr :data-for #(= % "button1"))
                             (make-content-updater "Fire!")
                             :first-only? true))

  ;; Add classes to multiple buttons
  (def with-classes
    (update-nodes-batch tree
                        [[(s/attr :data-for #(= % "button1"))
                          (make-class-adder "primary")]
                         [(s/attr :data-for #(= % "button2"))
                          (make-class-adder "secondary")]]))

  ;; Inline images
  (def with-inlined-images
    (fix-all-relative-images-base64 tree (System/getProperty "user.dir")))

  ;; Check how many images were inlined
  (let [[result count] (update-nodes tree
                                     (s/tag :image)
                                     identity
                                     :return-count? true)]
    (println "Found" count "images"))

  ;; Render to string
  (spit "/tmp/test.svg" (hickory->svg-string updated))

  ;; Compose multiple edits
  (def complex-edit
    (compose-edits
     (make-content-updater "Modified")
     (make-class-adder "updated")
     (make-attr-updater {:data-modified "true"})))

  (def complex-updated
    (update-nodes tree
                  (s/attr :data-for)
                  complex-edit)))
