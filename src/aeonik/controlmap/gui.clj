(ns aeonik.controlmap.gui
  "Interactive GUI for exploring mapped SVGs and unmapped actions"
  (:require
   [clojure.string :as str]
   [cljfx.api :as fx]
   [cljfx.ext.web-view :as fx.ext.web-view]
   [aeonik.controlmap.core :as core]
   [aeonik.controlmap.state :as state]
   [aeonik.controlmap.svg :as svg]
   [clojure.java.io :as io])
  (:import
   [javafx.scene.image Image]
   [javafx.scene.web WebEvent])
  (:gen-class))

(def joystick-icon
  (javafx.scene.image.Image. "images/gui_icon3_transparent.png"))

;; =============================================================================
;; Non-reactive Image Cache
;; =============================================================================

(defonce *image-cache
  (atom {:display-svgs {}
         :html-by-id {}
         :data-url-by-id {}}))

;; =============================================================================
;; Initial State
;; =============================================================================

(defn create-initial-state []
  (let [context (state/get-context)
        svgs (core/update-all-svgs context)
        htmls (into {} (map (fn [[iid svg]] [iid (svg/svg-tree->html-string svg)])) svgs)
        urls (into {} (map (fn [[iid html]] [iid (svg/html->data-url html)])) htmls)
        _ (reset! *image-cache {:display-svgs svgs
                                :html-by-id htmls
                                :data-url-by-id urls})
        available-svgs (set (keys (:svgs context)))
        instances (->> (:instances context)
                       (filter (fn [[_ svg-id]] (contains? available-svgs svg-id)))
                       (into []))
        unmapped (core/find-unmapped-actions (:actionmaps context))
        active (some-> instances first first)]
    {:context context
     :instances instances
     :active-instance active
     :status nil
     :filter-text ""
     :unmapped-actions unmapped
     :show-unmapped? true}))

(def *state (atom (create-initial-state)))

;; =============================================================================
;; Computed Values
;; =============================================================================

(defn filtered-unmapped-actions [state]
  (let [ft (str/lower-case (or (:filter-text state) ""))]
    (if (str/blank? ft)
      (:unmapped-actions state)
      (filterv #(str/includes? (str/lower-case %) ft)
               (:unmapped-actions state)))))

;; =============================================================================
;; Event Handling
;; =============================================================================

(defn map-event-handler [event]
  (case (:event/type event)
    ::set-status (fn [state]
                   (let [^WebEvent we (:fx/event event)
                         msg (.getData we)]
                     (println "⚡ SVG clicked:" msg)
                     (assoc state :status msg)))
    ::set-filter-text #(assoc % :filter-text (:fx/event event))
    ::toggle-unmapped #(update % :show-unmapped? not)
    ::set-active-instance (fn [state]
                            (if (:fx/event event)
                              (assoc state :active-instance (:instance-id event))
                              state))
    ::reload-context (fn [_]
                       (println "Reloading context...")
                       (try
                         (state/init!)
                         (create-initial-state)
                         (catch Exception e
                           (println "Error reloading context:" (.getMessage e))
                           @*state)))
    ::export-svgs (fn [state]
                    (println "Generating SVGs...")
                    (core/generate-all-svgs! (:context state))
                    (println "SVGs generated!")
                    state)
    identity))

;; =============================================================================
;; UI Components
;; =============================================================================

(defn unmapped-actions-panel [state]
  (let [filtered (filtered-unmapped-actions state)]
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
       :text (format "%d total, %d shown" (count (:unmapped-actions state)) (count filtered))
       :style "-fx-text-fill: gray;"}
      {:fx/type :text-field
       :prompt-text "Filter actions..."
       :text (:filter-text state)
       :on-text-changed {:event/type ::set-filter-text}}
      {:fx/type :list-view
       :v-box/vgrow :always
       :items (mapv core/clean-action-name filtered)}]}))

(defn instance-tab [{:keys [instance-id svg-id display-name data-url]}]
  {:fx/type :tab
   :text (format "[%d] %s" instance-id display-name)
   :closable false
   :on-selection-changed {:event/type ::set-active-instance :instance-id instance-id}
   :content {:fx/type fx.ext.web-view/with-engine-props
             :desc {:fx/type :web-view
                    :pref-width 800
                    :pref-height 600}
             :props {:url data-url
                     :on-status-changed {:event/type ::set-status}}}})

(defn svg-tab-pane [state]
  (let [context (:context state)
        cache @*image-cache]
    {:fx/type :tab-pane
     :h-box/hgrow :always
     :tab-closing-policy :unavailable
     :tabs (mapv (fn [[instance-id svg-id]]
                   {:fx/type instance-tab
                    :instance-id instance-id
                    :svg-id svg-id
                    :display-name (core/svg-id->display-name context svg-id)
                    :data-url (get-in cache [:data-url-by-id instance-id])})
                 (:instances state))}))

(defn control-toolbar [state]
  {:fx/type :tool-bar
   :items
   [{:fx/type :button
     :text "🔄 Reload"
     :tooltip {:fx/type :tooltip :text "Reload all mappings"}
     :on-action {:event/type ::reload-context}}
    {:fx/type :separator}
    {:fx/type :toggle-button
     :text "📋 Unmapped"
     :selected (boolean (:show-unmapped? state))
     :tooltip {:fx/type :tooltip :text "Show/hide unmapped actions"}
     :on-action {:event/type ::toggle-unmapped}}
    {:fx/type :separator}
    {:fx/type :button
     :text "📂 Export SVGs"
     :tooltip {:fx/type :tooltip :text "Generate SVG files"}
     :on-action {:event/type ::export-svgs}}
    {:fx/type :separator}
    {:fx/type :label
     :text (format "Instances: %d" (count (:instances state)))}]})

;; =============================================================================
;; Root View
;; =============================================================================

(defn root-view [state]
  {:fx/type :stage
   :showing true
   :title (format "Control Mapper - Instance %s" (or (:active-instance state) "None"))
   :width 1400
   :height 900
   :icons [joystick-icon]
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :children
                  [(control-toolbar state)
                   {:fx/type :h-box
                    :v-box/vgrow :always
                    :spacing 10
                    :padding 10
                    :children
                    (vec
                     (cond-> [(svg-tab-pane state)]
                       (:show-unmapped? state)
                       (conj (assoc (unmapped-actions-panel state) :h-box/hgrow :never))))}]}}})

;; =============================================================================
;; Application Lifecycle
;; =============================================================================

(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc root-view)
   :opts {:fx.opt/map-event-handler
          (fn [event]
            (swap! *state (map-event-handler event)))}))

(defn start! []
  (fx/mount-renderer *state renderer))

(defn stop! []
  (fx/unmount-renderer *state renderer)
  (println "\u2713 GUI stopped"))

(defn restart! []
  (stop!)
  (Thread/sleep 100)
  (start!))

(defn -main [& _] (start!))

(comment
  (start!)
  (stop!)
  (restart!))
