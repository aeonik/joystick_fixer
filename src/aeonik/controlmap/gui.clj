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

(defn prepare-context
  "Prepares context with updated SVGs for display"
  [context]
  (let [updated-svgs (core/update-all-svgs context)]
    (assoc context :display-svgs updated-svgs)))

;; =============================================================================
;; Global UI State
;; =============================================================================

(defonce gui-state (atom nil))

(defn initialize-gui-state!
  "Initializes the GUI state with the new structure"
  []
  (let [context (prepare-context (state/get-context))
        ;; Get SVG ids that have actual SVG files loaded
        available-svgs (set (keys (:svgs context)))
        ;; Get instances that have SVGs
        instances-with-svgs (filter #(available-svgs (val %))
                                    (:instances context))
        unmapped (core/find-unmapped-actions (:actionmaps context))]
    (reset! gui-state
            {:context context
             :instances instances-with-svgs
             :active-instance (ffirst instances-with-svgs)
             :status nil
             :filter-text ""
             :unmapped-actions unmapped
             :show-unmapped? true})))

(defn reload-context!
  "Reloads the context with fresh data"
  []
  (println "Reloading context...")
  (try
    (state/init!)
    (initialize-gui-state!)
    (println "Context reloaded successfully!")
    true
    (catch Exception e
      (println "Error reloading context:" (.getMessage e))
      false)))

;; =============================================================================
;; UI Components
;; =============================================================================

(defn unmapped-actions-panel
  "Renders filterable list of unmapped actions"
  [{:keys [unmapped-actions filter-text]}]
  (let [filtered (if (str/blank? filter-text)
                   unmapped-actions
                   (filter #(str/includes? (str/lower-case %)
                                           (str/lower-case filter-text))
                           unmapped-actions))]
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
       :text (format "%d total, %d shown"
                     (count unmapped-actions)
                     (count filtered))
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
        :items filtered
        :cell-factory
        (fn [action]
          {:text (core/clean-action-name action)})}}]}))

(defn instance-tab
  "Creates a tab for a joystick instance"
  [{:keys [instance-id svg-id svg-tree display-name]}]
  (let [html-content (svg/svg-tree->html-string svg-tree)
        data-url (svg/html->data-url html-content)]
    {:fx/type :tab
     :text (format "[%d] %s" instance-id display-name)
     :closable false
     :on-selection-changed
     (fn [e]
       (let [^javafx.scene.control.Tab tab (.getSource e)]
         (when (.isSelected tab)
           (swap! gui-state assoc :active-instance instance-id))))
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
  "Tab pane showing all instances with SVGs"
  [{:keys [context instances active-instance]}]
  {:fx/type :tab-pane
   :h-box/hgrow :always
   :tab-closing-policy :unavailable
   :tabs
   (mapv (fn [[instance-id svg-id]]
           (let [svg-tree (get-in context [:display-svgs instance-id])]
             (instance-tab {:instance-id instance-id
                            :svg-id svg-id
                            :svg-tree svg-tree
                            :display-name (core/svg-id->display-name context svg-id)})))
         instances)})

(defn control-toolbar
  "Top toolbar"
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
            :on-action #(swap! gui-state update :show-unmapped? not)}

           {:fx/type :separator}

           {:fx/type :button
            :text "💾 Export SVGs"
            :tooltip {:fx/type :tooltip :text "Generate SVG files"}
            :on-action (fn [_]
                         (println "Generating SVGs...")
                         (core/generate-all-svgs! (:context @gui-state))
                         (println "SVGs generated!"))}

           {:fx/type :separator}

           {:fx/type :label
            :text (format "Instances: %d" (count (:instances @gui-state)))}]})

;; =============================================================================
;; Main View
;; =============================================================================

(defn root-view
  "Main GUI view"
  [{:keys [context instances active-instance unmapped-actions
           filter-text show-unmapped?] :as state}]
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
     :title (format "ControlMap - Instance %s"
                    (or active-instance "None"))
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
                         :instances instances
                         :active-instance active-instance})

          (when show-unmapped?
            (unmapped-actions-panel {:unmapped-actions unmapped-actions
                                     :filter-text filter-text}))]}]}}}))

;; =============================================================================
;; Application Lifecycle
;; =============================================================================

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc #'root-view)))

(defn start!
  "Starts the GUI"
  []
  (when (nil? @gui-state)
    (initialize-gui-state!))
  (fx/mount-renderer gui-state renderer)
  (println "✓ GUI started"))

(defn stop!
  "Stops the GUI"
  []
  (fx/unmount-renderer gui-state renderer)
  (println "✓ GUI stopped"))

(defn restart!
  "Restarts the GUI"
  []
  (stop!)
  (Thread/sleep 100)
  (start!))

(defn -main
  "Main entry point"
  [& args]
  (start!))

;; =============================================================================
;; REPL Helpers
;; =============================================================================

(comment
  ;; Start/stop
  (start!)
  (stop!)
  (restart!)

  ;; Reload data
  (reload-context!)

  ;; Check state
  @gui-state
  (:instances @gui-state)

  ;; Check what's loaded
  (-> @gui-state :context :instances)
  ;; => {0 :alpha_RP, 1 :vpc_mongoose_t50cm3, ...}

  ;; Check display SVGs
  (-> @gui-state :context :display-svgs keys)

  ;; Debug specific instance
  (let [ctx (:context @gui-state)]
    (core/analyze-instance ctx 0)))
