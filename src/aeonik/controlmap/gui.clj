(ns aeonik.controlmap.gui
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cljfx.api :as fx]
            [cljfx.ext.web-view :as fx.ext.web-view]
            [aeonik.controlmap.core :as core]
            [aeonik.controlmap.state :as state]
            [aeonik.controlmap.svg :as svg]
            [net.cgrand.enlive-html :as html])
  (:import [javafx.scene.web WebEvent])
  (:gen-class))

(svg/fix-all-relative-images (-> state/context :svg-roots first rest first) (System/getProperty "user.dir"))

(defn fix-context-images
  "Fix all image paths in all SVG roots within the context"
  [context]
  (let [base-path (System/getProperty "user.dir")
        fixed-svg-roots (into {}
                              (map (fn [[k svg-tree]]
                                     [k (svg/fix-all-relative-images-base64 svg-tree base-path)])
                                   (:svg-roots context)))]
    (assoc context :svg-roots fixed-svg-roots)))

(defn svg-tree->html-string
  "Convert an SVG tree structure to HTML string"
  [svg-tree]
  (let [svg-content (apply str (html/emit* svg-tree))]
    (str "<!DOCTYPE html><html><head><meta charset=\"utf-8\"></head><body>"
         svg-content
         "</body></html>")))

(defn create-data-url
  "Create a data URL from HTML content"
  [html-content]
  (str "data:text/html;base64,"
       (.encodeToString (java.util.Base64/getEncoder)
                        (.getBytes html-content "UTF-8"))))

;; --- Application State ---
(def fixed-context (fix-context-images state/context))

(def *state
  (atom
   {:svg-names (keys (-> fixed-context :svg-roots))
    :active-svg (first (keys (-> fixed-context :svg-roots)))
    :status nil
    :filter-text ""
    :unmapped-actions (core/empty-input-bindings state/actionmaps)
    :context fixed-context}))

(defn handle-status-change [old new]
  (let [old-svg (:active-svg old)
        new-svg (:active-svg new)
        old-status (:status old)
        new-status (:status new)]
    (cond
      ;; Tab switched
      (not= old-svg new-svg)
      (do
        (swap! *state assoc :status nil)
        (println "Switched tabs to:" (name new-svg)))
      ;; Status changed in same tab
      (and new-status (not= old-status new-status))
      (cond
        (str/starts-with? new-status "clicked:")
        (let [id (subs new-status (count "clicked:"))]
          (println "Clicked:" id "in SVG:" (name new-svg)))
        (str/starts-with? new-status "hovered:")
        (let [id (subs new-status (count "hovered:"))]
          (println "Hovered:" id "in SVG:" (name new-svg)))
        :else
        (println "Unknown status message from" (name new-svg) ":" new-status)))))

(add-watch *state ::status-watcher
           (fn [_ _ old new]
             (handle-status-change old new)))

(defn unmapped-actions-panel [filter-text unmapped-actions]
  (let [all-unmapped unmapped-actions
        filtered (if (str/blank? filter-text)
                   all-unmapped
                   (filter #(str/includes? (:action %) filter-text) all-unmapped))]
    {:fx/type :v-box
     :spacing 10
     :padding 10
     :children
     [{:fx/type :label
       :text "unmapped actions"
       :style "-fx-font-size: 16px; -fx-font-weight: bold;"}
      {:fx/type :text-field
       :prompt-text "filter..."
       :text filter-text
       :on-text-changed #(swap! *state assoc :filter-text %)}
      {:fx/type :list-view
       :v-box/vgrow :always
       :items (mapv :action filtered)
       :pref-height 300
       :pref-width 350}]}))

;; --- View Function ---
(defn view [{:keys [svg-names active-svg status filter-text unmapped-actions context]}]
  (let [svg-roots (-> context :svg-roots)]
    {:fx/type :stage
     :showing true
     :title (str (name active-svg))
     :scene
     {:fx/type :scene
      :root
      {:fx/type :h-box
       :spacing 20
       :padding 10
       :children
       [{:fx/type :tab-pane
         :side :top
         :h-box/hgrow :always
         :tabs (mapv
                (fn [svg-name]
                  (let [svg-tree (get svg-roots svg-name)
                        html-content (svg-tree->html-string svg-tree)
                        data-url (create-data-url html-content)]
                    {:fx/type :tab
                     :id (name svg-name)
                     :text (name svg-name)
                     :closable false
                     :on-selection-changed
                     (fn [e]
                       (let [^javafx.scene.control.Tab tab (.getSource e)]
                         (when (.isSelected tab)
                           (swap! *state assoc :active-svg svg-name))))
                     :content
                     {:fx/type fx.ext.web-view/with-engine-props
                      :desc {:fx/type :web-view
                             :max-width Double/MAX_VALUE
                             :max-height Double/MAX_VALUE}
                      :props {:url data-url
                              :on-status-changed
                              (fn [^WebEvent evt]
                                (swap! *state assoc :status (.getData evt)))}}}))
                svg-names)}
        ;; Right-side global unmapped actions list
        (unmapped-actions-panel filter-text unmapped-actions)]}}}))

;; --- Renderer ---
(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc #'view)))

(defn -main
  [& args]
  ;; --- App Start ---
  (fx/mount-renderer *state renderer))
