(ns aeonik.controlmap.svg
  (:require [aeonik.controlmap.index :as index]
            [aeonik.controlmap.state :as state]
            [clojure.string :as str]
            [clojure.java.io :as io]
            [net.cgrand.enlive-html :as html]
            [hickory.core :as h]
            [hickory.select :as s]
            [hickory.zip :as hzip]
            [clojure.zip :as zip]
            [riveted.core :as vtd])
  (:import (java.util Base64)
           (java.nio.file Files)))

;; EDN SVG Section
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
  (let [[class-key joystick-data] data]
    [:svg {:xmlns "http://www.w3.org/2000/svg"
           :class (name class-key)}
     (svg-elements joystick-data)]))

(comment
  (coordinates->svg-hiccup (nth (seq state/svg-edn-files->map) 4))

  (map coordinates->svg-hiccup state/svg-edn-files->map)

  (apply str (html/emit* (html/html (coordinates->svg-hiccup (nth (seq state/svg-edn-files->map) 4)))))

  (coordinates->svg-hiccup state/svg-edn-files->map))

(def hiccup-svg
  (coordinates->svg-hiccup (first state/svg-edn-files->map)))

;; Helper functions for Hickory
(defn update-nodes
  "Updates all nodes in a tree that match the given selector.

   Args:
   - tree: Hickory tree to modify
   - selector: Hickory selector function (e.g., (s/attr :data-for #(= % \"button1\")))
   - edit-fn: Function that takes a node and returns modified node

   Options:
   - :first-only? - If true, only update first match (default false)
   - :return-locs? - If true, return [modified-tree matched-locs] (default false)

   Returns modified tree (or [tree locs] if :return-locs? is true)"
  [tree selector edit-fn & {:keys [first-only? return-locs?]
                            :or {first-only? false
                                 return-locs? false}}]
  (let [matched-locs (atom [])]
    (loop [current-tree tree]
      (let [root-loc (hzip/hickory-zip current-tree)
            found-loc (s/select-next-loc selector root-loc)]
        (if found-loc
          (do
            (when return-locs?
              (swap! matched-locs conj (zip/node found-loc)))
            (let [updated-tree (-> found-loc
                                   (zip/edit edit-fn)
                                   zip/root)]
              (if first-only?
                (if return-locs?
                  [updated-tree @matched-locs]
                  updated-tree)
                (recur updated-tree))))
          (if return-locs?
            [current-tree @matched-locs]
            current-tree))))))

;; Even more flexible version with multiple selectors/edit-fns
(defn update-nodes-batch
  "Apply multiple selector->edit-fn pairs to a tree.

   Args:
   - tree: Hickory tree to modify
   - updates: Vector of [selector edit-fn] pairs or [selector edit-fn options-map] triples

   Returns modified tree"
  [tree updates]
  (reduce (fn [current-tree update-spec]
            (let [[selector edit-fn opts] (if (= 3 (count update-spec))
                                            update-spec
                                            [(first update-spec) (second update-spec) {}])]
              (update-nodes current-tree selector edit-fn opts)))
          tree
          updates))

;; Helper function to create common edit functions
(defn make-content-updater
  "Creates an edit function that updates content"
  [content-or-fn]
  (fn [node]
    (assoc node :content
           (if (fn? content-or-fn)
             [(content-or-fn node)]
             [content-or-fn]))))

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
               #(str % " " classes))))

;; Compose multiple edit functions
(defn compose-edits
  "Chains multiple edit functions together"
  [& edit-fns]
  (fn [node]
    (reduce (fn [n edit-fn] (edit-fn n))
            node
            edit-fns)))

(comment
  ;; Usage examples:
  ;; Update all buttons to show "Click me"
  (update-nodes tree
                (s/tag :button)
                (make-content-updater "Click me"))

  ;; Add a class to all elements with data-for attribute
  (update-nodes tree
                (s/attr :data-for)
                (make-class-adder "has-data-for"))

  ;; Complex update with custom function
  (update-nodes tree
                (s/and (s/tag :text)
                       (s/attr :data-row))
                (fn [node]
                  (let [row (get-in node [:attrs :data-row])]
                    (-> node
                        (assoc :content [(str "Row " row)])
                        (assoc-in [:attrs :data-modified] "true")))))

  ;; Batch updates with different selectors
  (update-nodes-batch tree
                      [[(s/attr :data-for "button1")
                        (make-content-updater "Emergency Exit")]
                       [(s/attr :data-for "button2")
                        (make-content-updater "Turret Mode")]
                       [(s/class "inactive")
                        (make-class-adder "dimmed")
                        {:first-only? false}]]) ; Update all inactive elements

  ;; Get back the nodes that were modified
  (let [[modified-tree matched-nodes]
        (update-nodes tree
                      (s/attr :data-action)
                      (make-class-adder "has-action")
                      :return-locs? true)]
    (println "Modified" (count matched-nodes) "nodes")
    modified-tree))

(defn svg-roots->navigator-map
  "No api in riveted for modification, so this isn't useful"
  [svg-roots]
  (into {}
        (map (fn [root]
               [(first root)
                ((comp vtd/navigator #(apply str %) html/emit*) (rest root))])
             svg-roots)))
;; Usage:
(comment (svg-roots->navigator-map (-> state/context :svg-roots)))

;; Helper functions for action maps and svg wrangling
(defn svg-tree->html-string [svg-tree]
  "Convert an SVG tree structure to HTML string"
  (apply str (html/emit* svg-tree)))

(defn create-data-url [html-content]
  "Create a data URL from HTML content"
  (str "data:text/html;charset=utf-8,"
       (java.net.URLEncoder/encode html-content "UTF-8")))

(comment
  (-> state/svg-roots first rest first)

  (-> state/svg-roots first rest first (html/select [[:text#lbl_button14]]))

  (-> state/svg-roots first rest first (html/select [[:svg :text (html/attr= :data-for "button14")]]))

  (-> state/svg-roots first rest first (html/select [:svg :> :g#labels [:text (html/attr= :data-for "button14")]]))

  (-> state/svg-roots first rest first
      (html/transform
       [[:text (html/attr= :data-for "button14")]] (html/content "New text content")))

  (-> state/svg-roots
      :alpha_L
      (html/at [:svg :> :g#labels [:text (html/attr= :data-for "button14")]]
               (html/content "New button text"))))

(defn update-button-text [svg-root button new-text]
  (into {} (-> svg-root
               (html/at [:svg :> :g#labels [:text (html/attr= :data-for button)]]
                        (html/content new-text)))))

(defn relative-to-absolute-url
  "Convert a relative path to an absolute file URL"
  [relative-path base-path]
  (let [clean-path (str/replace relative-path #"^\.\./" "")
        absolute-path (str base-path "/" clean-path)]
    (str absolute-path)))

(defn fix-all-relative-images
  "Convert all relative image paths to absolute file URLs"
  [svg-tree base-path]
  (let [selector [[:image (html/attr-starts :href "../")]]
        text (-> (html/select svg-tree selector)
                 first
                 (html/attr-values :href)
                 first)
        replacement-text (relative-to-absolute-url text base-path)]
    (into {} (html/at svg-tree
                      [[:image (html/attr-starts :href "../")]]
                      (html/set-attr :href replacement-text)))))

(def ^:private ext->mime
  {"png" "image/png" "jpg" "image/jpeg" "jpeg" "image/jpeg"
   "gif" "image/gif" "svg" "image/svg+xml" "webp" "image/webp"})

(defn- mime-type [f]
  (or (Files/probeContentType (.toPath (io/file f)))
      (some-> (str/lower-case (str f))
              (re-find #"\.([^.]+)$") second ext->mime)
      "application/octet-stream"))

(defn- file->data-uri [f]
  (let [bytes (Files/readAllBytes (.toPath (io/file f)))
        b64   (.encodeToString (Base64/getEncoder) bytes)]
    (str "data:" (mime-type f) ";base64," b64)))

(defn fix-all-relative-images-base64
  "Inline <image> hrefs that start with \"../\" as base64 data URIs."
  [svg-tree base-path]
  (html/at svg-tree
           [[:image (html/attr-starts :href "../")]]
           (fn [el]
             (let [href (-> el (html/attr-values :href) first)
                   abs (relative-to-absolute-url href base-path)
                   data (file->data-uri abs)]
               ;; Set both for SVG 1.1 compatibility.
               (-> el
                   (assoc-in [:attrs :href] data)
                   (assoc-in [:attrs :xlink:href] data))))))

(comment (fix-all-relative-images (-> state/context :svg-roots first rest first) (System/getProperty "user.dir"))

         (fix-all-relative-images-base64 (-> state/context :svg-roots first rest first) (System/getProperty "user.dir")))
