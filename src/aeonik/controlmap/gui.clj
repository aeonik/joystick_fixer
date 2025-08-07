(ns aeonik.controlmap.gui
  "Interactive GUI for exploring mapped SVGs and unmapped actions"
  (:require
   [clojure.string :as str]
   [cljfx.api :as fx]
   [cljfx.ext.web-view :as fx.ext.web-view]
   [aeonik.controlmap.core :as core]
   [aeonik.controlmap.state :as state]
   [aeonik.controlmap.svg :as svg])
  (:import [javafx.scene.web WebEvent])
  (:gen-class))

;; =============================================================================
;; Context Initialization
;; =============================================================================

(defn load-fresh-context
  "Loads a fresh context with all mappings applied."
  []
  (-> (state/build-context)  ; Build fresh context from scratch
      core/update-context))   ; Apply mappings and inline images

(defn get-initial-context
  "Gets or creates the initial context."
  []
  (try
    (if-let [ctx @state/*context*]
      (core/update-context ctx)
      (load-fresh-context))
    (catch Exception e
      (println "Error loading context:" (.getMessage e))
      (load-fresh-context))))

;; =============================================================================
;; SVG Rendering Utilities
;; =============================================================================

(defn svg-tree->html-string
  "Renders SVG (hickory) tree into a minimal HTML document string."
  [svg-tree]
  (let [svg-content (svg/hickory->svg-string svg-tree)]
    (str "<!DOCTYPE html>"
         "<html><head>"
         "<meta charset=\"utf-8\">"
         "<style>"
         "body { margin: 0; padding: 0; overflow: hidden; }"
         "svg { max-width: 100%; height: auto; display: block; }"
         "</style>"
         "</head><body>"
         svg-content
         "</body></html>")))

(defn html->data-url
  "Encodes HTML string into a base64 data URL."
  [html-content]
  (str "data:text/html;base64,"
       (.encodeToString (java.util.Base64/getEncoder)
                        (.getBytes html-content "UTF-8"))))

(defn short-name->display-name [context short-name]
  (-> context
      :joystick-ids
      (as-> ids
            (get ids (short-name (core/build-joystick-lookup ids))))
      :match-regex
      str))

;; =============================================================================
;; Global UI State
;; =============================================================================

(defonce gui-state
  (atom nil))

(defn initialize-gui-state!
  "Initializes or resets the GUI state."
  []
  (let [context (get-initial-context)
        svg-names (sort (keys (:svg-roots context)))
        unmapped (core/find-empty-bindings (:actionmaps context))]
    (reset! gui-state
            {:context context
             :svg-names svg-names
             :active-svg (first svg-names)
             :status nil
             :filter-text ""
             :unmapped-actions unmapped
             :show-unmapped? true})))

(defn reload-context!
  "Reloads the context with fresh data."
  []
  (println "Reloading context...")
  (try
    ;; Force reload the underlying state
    (state/init! :force-reload true)
    ;; Reinitialize GUI state with fresh context
    (initialize-gui-state!)
    (println "Context reloaded successfully!")
    true
    (catch Exception e
      (println "Error reloading context:" (.getMessage e))
      false)))

;; =============================================================================
;; State Watch/Change Handling
;; =============================================================================

(defn handle-status-event
  "Handles updates to :status in gui-state."
  [old new]
  (let [old-svg (:active-svg old)
        new-svg (:active-svg new)
        old-status (:status old)
        new-status (:status new)]
    (cond
      ;; Tab switched
      (not= old-svg new-svg)
      (println (format "📑 Switched to: %s" (name new-svg)))

      ;; Status message changed
      (and (not= old-status new-status) new-status)
      (cond
        (str/starts-with? new-status "clicked:")
        (println (format "🖱️ Clicked: %s in %s"
                         (subs new-status 8) (name new-svg)))

        (str/starts-with? new-status "hovered:")
        nil  ; Don't log hovers, too noisy

        :else
        (println (format "📌 Status: %s" new-status))))))

;; =============================================================================
;; UI Components
;; =============================================================================

(defn action-item-renderer
  "Renders a single action in the list."
  [action]
  {:fx/type :h-box
   :spacing 5
   :children [{:fx/type :label
               :text (core/clean-action-name action)
               :style "-fx-font-family: monospace;"}]})

(defn unmapped-actions-panel
  "Renders filterable list of actions without bindings."
  [{:keys [unmapped-actions filter-text show-unmapped?]}]
  (when show-unmapped?
    (let [filtered-actions (if (str/blank? filter-text)
                             unmapped-actions
                             (filter #(str/includes?
                                       (str/lower-case (:action %))
                                       (str/lower-case filter-text))
                                     unmapped-actions))
          action-names (map :action filtered-actions)]
      {:fx/type :v-box
       :spacing 10
       :padding 10
       :min-width 300
       :pref-width 350
       :children
       [{:fx/type :label
         :text "Unmapped Actions"
         :style "-fx-font-size: 16px; -fx-font-weight: bold;"}

        {:fx/type :label
         :text (format "%d actions (filtered: %d)"
                       (count unmapped-actions)
                       (count filtered-actions))
         :style "-fx-text-fill: gray;"}

        {:fx/type :text-field
         :prompt-text "Filter actions..."
         :text filter-text
         :on-text-changed #(swap! gui-state assoc :filter-text %)}

        {:fx/type :scroll-pane
         :v-box/vgrow :always
         :fit-to-height true
         :fit-to-width true
         :content
         {:fx/type :list-view
          :items action-names
          :cell-factory
          (fn [action]
            {:text (core/clean-action-name action)})}}]})))

(defn svg-tab
  "Returns a single tab containing a rendered SVG."
  [{:keys [svg-name svg-tree display-name]}]
  (let [html-content (svg-tree->html-string svg-tree)
        data-url (html->data-url html-content)]
    {:fx/type :tab
     :id (name svg-name)
     :text display-name
     :closable false
     :on-selection-changed
     (fn [e]
       (let [^javafx.scene.control.Tab tab (.getSource e)]
         (when (.isSelected tab)
           (swap! gui-state assoc :active-svg svg-name))))
     :content
     {:fx/type fx.ext.web-view/with-engine-props
      :desc {:fx/type :web-view
             :pref-width 800
             :pref-height 600}
      :props {:url data-url
              :on-status-changed
              (fn [^WebEvent evt]
                (swap! gui-state assoc :status (.getData evt)))}}}))

(defn svg-tab-pane
  "Tab pane for all SVGs in the current context."
  [{:keys [context svg-names active-svg]}]
  {:fx/type :tab-pane
   :h-box/hgrow :always
   :tab-closing-policy :unavailable
   :tabs
   (mapv (fn [svg-name]
           (svg-tab {:svg-name svg-name
                     :svg-tree (get-in context [:svg-roots svg-name])
                     :display-name (short-name->display-name context svg-name)}))
         svg-names)})

(defn control-toolbar
  "Top toolbar with controls."
  [{:keys [show-unmapped?]}]
  {:fx/type :tool-bar
   :items [{:fx/type :button
            :text "🔄 Reload"
            :tooltip {:fx/type :tooltip :text "Reload all mappings"}
            :on-action (fn [_] (reload-context!))}

           {:fx/type :separator}

           {:fx/type :toggle-button
            :text "📋 Unmapped"
            :selected show-unmapped?
            :tooltip {:fx/type :tooltip :text "Show/hide unmapped actions"}
            :on-action (fn [_]
                         (swap! gui-state update :show-unmapped? not))}

           {:fx/type :separator}

           {:fx/type :button
            :text "💾 Export SVGs"
            :tooltip {:fx/type :tooltip :text "Generate SVG files"}
            :on-action (fn [_]
                         (println "Generating SVGs...")
                         (core/generate-all-svgs! (:context @gui-state))
                         (println "SVGs generated!"))}]})

;; =============================================================================
;; Main View
;; =============================================================================

(defn root-view
  "Main GUI view function."
  [{:keys [context svg-names active-svg status filter-text
           unmapped-actions show-unmapped?] :as state}]
  (if (nil? state)
    ;; Loading view
    {:fx/type :stage
     :showing true
     :title "ControlMap - Loading..."
     :width 400
     :height 200
     :scene {:fx/type :scene
             :root {:fx/type :v-box
                    :alignment :center
                    :children [{:fx/type :label
                                :text "Loading ControlMap..."
                                :style "-fx-font-size: 18px;"}]}}}
    ;; Main view
    {:fx/type :stage
     :showing true
     :title (str "ControlMap - " (when active-svg (name active-svg)))
     :width 1400
     :height 900
     :scene
     {:fx/type :scene
      :root
      {:fx/type :v-box
       :children
       [(control-toolbar {:show-unmapped? show-unmapped?})

        {:fx/type :h-box
         :v-box/vgrow :always
         :spacing 10
         :padding 10
         :children
         [(svg-tab-pane {:context context
                         :svg-names svg-names
                         :active-svg active-svg})

          (when show-unmapped?
            (unmapped-actions-panel {:unmapped-actions unmapped-actions
                                     :filter-text filter-text
                                     :show-unmapped? show-unmapped?}))]}]}}}))

;; =============================================================================
;; Application Lifecycle
;; =============================================================================

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc #'root-view)))

(defn start!
  "Starts the GUI application."
  []
  ;; Initialize state if needed
  (when (nil? @gui-state)
    (initialize-gui-state!))

  ;; Add state watcher
  (remove-watch gui-state ::status-listener)
  (add-watch gui-state ::status-listener
             (fn [_ _ old new]
               (handle-status-event old new)))

  ;; Mount renderer
  (fx/mount-renderer gui-state renderer)
  (println "GUI started!"))

(defn stop!
  "Stops the GUI application."
  []
  (fx/unmount-renderer gui-state renderer)
  (remove-watch gui-state ::status-listener)
  (println "GUI stopped!"))

(defn restart!
  "Restarts the GUI application."
  []
  (stop!)
  (Thread/sleep 100)
  (start!))

(defn -main
  "Main entry point for standalone execution."
  [& args]
  (start!))

;; =============================================================================
;; Development & REPL Helpers
;; =============================================================================

(comment
  ;; Start the GUI
  (start!)

  ;; Stop the GUI
  (stop!)

  ;; Restart the GUI
  (restart!)

  ;; Reload context (while GUI is running)
  (reload-context!)

  ;; Check current state
  @gui-state

  ;; Check context
  (keys (:svg-roots (:context @gui-state)))
  (:panel_3 (:svg-roots (:context @gui-state)))

  ;; Check unmapped actions
  (count (:unmapped-actions @gui-state))

  ;; Initialize from scratch
  (do
    (initialize-gui-state!)
    (start!))

  ;; Force reload everything
  (do
    (state/init! :force-reload true)
    (reload-context!)
    (restart!))

  ;; Debug a specific SVG
  (-> @gui-state
      :context
      :svg-roots
      :alpha_L
      svg/count-buttons))

