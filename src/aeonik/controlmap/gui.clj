(ns aeonik.controlmap.gui
  "Interactive GUI for exploring mapped SVGs and unmapped actions"
  (:require
   [clojure.string :as str]
   [clojure.java.io :as io]
   [cljfx.api :as fx]
   [cljfx.ext.web-view :as fx.ext.web-view]
   [aeonik.controlmap.core :as core]
   [aeonik.controlmap.state :as state]
   [aeonik.controlmap.svg :as svg]
   [net.cgrand.enlive-html :as html]
   [hickory.core :as h]
   [hickory.render :as render])
  (:import [javafx.scene.web WebEvent])
  (:gen-class))

;; =============================================================================
;; Context Initialization and Synchronization
;; =============================================================================

(defn compute-initial-context
  "Returns the initial, fully mapped, and inlined GUI context."
  []
  (-> @state/context
      core/update-context))

(defonce ^:private gui-context (atom (compute-initial-context)))

;; =============================================================================
;; SVG Rendering Utilities
;; =============================================================================

(defn svg-tree->html-string
  "Renders SVG (hickory) tree into a minimal HTML document string."
  [svg-tree]
  (let [svg-content (svg/hickory->svg-string svg-tree)]
    (str "<!DOCTYPE html><html><head>"
         "<meta charset=\"utf-8\"></head><body style=\"margin:0;padding:0;\">"
         svg-content
         "</body></html>")))

(defn html->data-url
  "Encodes HTML string into a base64 data URL."
  [html-content]
  (str "data:text/html;base64,"
       (.encodeToString (java.util.Base64/getEncoder)
                        (.getBytes html-content "UTF-8"))))

;; =============================================================================
;; Global UI State Atom
;; =============================================================================

(defonce gui-state
  (atom
   {:context        @gui-context
    :svg-names      (keys (get-in @gui-context [:svg-roots]))
    :active-svg     (first (keys (get-in @gui-context [:svg-roots])))
    :status         nil
    :filter-text    ""
    :unmapped-actions (core/find-empty-bindings (get-in @gui-context [:actionmaps]))}))

(defn reload-context!
  "Reloads and re-maps context, updating all SVGs and unmapped actions."
  []
  (let [new-context (core/update-context (state/init! :force-reload true))
        unmapped    (core/find-empty-bindings (:actionmaps new-context))]
    (swap! gui-state assoc
           :context new-context
           :svg-names (keys (:svg-roots new-context))
           :active-svg (first (keys (:svg-roots new-context)))
           :unmapped-actions unmapped)))

;; =============================================================================
;; State Watch/Change Handling
;; =============================================================================

(defn handle-status-event
  "Handles updates to :status in gui-state when tab or status changes."
  [old new]
  (let [old-svg (:active-svg old)
        new-svg (:active-svg new)
        old-status (:status old)
        new-status (:status new)]
    (cond
      ;; Tab switched
      (not= old-svg new-svg)
      (do
        (swap! gui-state assoc :status nil)
        (println "Switched to:" new-svg))
      ;; Status message changed
      (and (not= old-status new-status) new-status)
      (cond
        (str/starts-with? new-status "clicked:")
        (println "Clicked:" (subs new-status 8) "in" new-svg)
        (str/starts-with? new-status "hovered:")
        (println "Hovered:" (subs new-status 8) "in" new-svg)
        :else (println "Status event:" new-status)))))

(add-watch gui-state ::status-listener
           (fn [_ _ old new]
             (handle-status-event old new)))

;; =============================================================================
;; Unmapped Actions Panel
;; =============================================================================

(defn unmapped-actions-list
  "Renders filterable list of actions without bindings."
  [{:keys [unmapped-actions filter-text]}]
  (let [actions (if (str/blank? filter-text)
                  unmapped-actions
                  (filter #(str/includes? (:action %) filter-text) unmapped-actions))]
    {:fx/type :v-box
     :spacing 10
     :padding 10
     :children
     [{:fx/type :label
       :text "Unmapped Actions"
       :style "-fx-font-size: 16px; -fx-font-weight: bold"}
      {:fx/type :text-field
       :prompt-text "Filter..."
       :text filter-text
       :on-text-changed #(swap! gui-state assoc :filter-text %)}
      {:fx/type :list-view
       :v-box/vgrow :always
       :items (mapv :action actions)
       :pref-height 300
       :pref-width 350}]}))

;; =============================================================================
;; SVG Viewing Tab Pane
;; =============================================================================

(defn svg-tab
  "Returns a single tab containing a rendered SVG."
  [{:keys [svg-name svg-tree active?]}]
  (let [html-content (svg-tree->html-string svg-tree)
        data-url (html->data-url html-content)]
    {:fx/type :tab
     :id (name svg-name)
     :text (name svg-name)
     :closable false
     :on-selection-changed
     (fn [e]
       (let [^javafx.scene.control.Tab tab (.getSource e)]
         (when (.isSelected tab)
           (swap! gui-state assoc :active-svg svg-name))))
     :content
     {:fx/type fx.ext.web-view/with-engine-props
      :desc {:fx/type :web-view
             :max-width Double/MAX_VALUE
             :max-height Double/MAX_VALUE}
      :props {:url data-url
              :on-status-changed
              (fn [^WebEvent evt]
                (swap! gui-state assoc :status (.getData evt)))}}}))

(defn svg-tab-pane
  "Tab pane for all SVGs in the current context."
  [context svg-names active-svg]
  {:fx/type :tab-pane
   :side :top
   :h-box/hgrow :always
   :tabs
   (mapv (fn [svg-name]
           (svg-tab {:svg-name svg-name
                     :svg-tree (get-in context [:svg-roots svg-name])
                     :active? (= svg-name active-svg)}))
         svg-names)})

;; =============================================================================
;; Top-Level View
;; =============================================================================

(defn root-view
  "Main GUI view function, orchestrates SVG pane and sidepanel."
  [{:keys [context svg-names active-svg status filter-text unmapped-actions]}]
  {:fx/type :stage
   :showing true
   :title (str "ControlMap: " (name active-svg))
   :width 1280
   :height 900
   :scene
   {:fx/type :scene
    :root
    {:fx/type :h-box
     :spacing 20
     :padding 10
     :children
     [(svg-tab-pane context svg-names active-svg)
      (unmapped-actions-list {:unmapped-actions unmapped-actions
                              :filter-text filter-text})]}}})

;; =============================================================================
;; Renderer and Main Entry Point
;; =============================================================================

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc #'root-view)))

(defn run-gui!
  "Mounts the GUI. Optional: call reload-context! before for reload."
  []
  (fx/mount-renderer gui-state renderer))

(defn -main
  "Entry point for standalone execution (via clj -M/-main)."
  [& args]
  (run-gui!))

;; =============================================================================
;; Development & REPL Helpers
;; =============================================================================

(comment
  ;; To reload all mappings and re-mount:
  (reload-context!)
  (run-gui!)

  ;; Or just show GUI immediately (using current context):
  (run-gui!)

  ;; Update unmapped actions list manually if needed:
  (swap! gui-state assoc :unmapped-actions (core/find-empty-bindings (get-in @gui-state [:context :actionmaps])))

  ;; To debug the current GUI state:
  @gui-state

  ;; To force reload and mount fresh context:
  (reload-context!)
  (run!))
