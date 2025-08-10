(ns aeonik.controlmap.gui
  "Interactive GUI for exploring mapped SVGs and unmapped actions"
  (:require
   [clojure.string :as str]
   [cljfx.api :as fx]
   [clojure.java.io :as io]  ; Add this import
   [cljfx.ext.web-view :as fx.ext.web-view]
   [aeonik.controlmap.core :as core]
   [aeonik.controlmap.state :as state]
   [aeonik.controlmap.svg :as svg])
  (:import
   [javafx.scene.image Image]
   [javafx.scene.web WebEvent]
   [java.awt Taskbar Taskbar$Feature]    ; Add these imports
   [javax.imageio ImageIO])
  (:gen-class))

(when (.startsWith (System/getProperty "os.name" "") "Mac")
  (System/setProperty "apple.awt.application.name" "Control Mapper"))

(def joystick-icon-path "images/gui_icon3.png")
(def joystick-icon (javafx.scene.image.Image. joystick-icon-path))

(defn set-macos-dock-icon! []
  (when (and (.startsWith (System/getProperty "os.name" "") "Mac")
             (Taskbar/isTaskbarSupported)
             (.isSupported (Taskbar/getTaskbar) Taskbar$Feature/ICON_IMAGE))
    (try
      (with-open [in (or (some-> (io/resource joystick-icon-path) io/input-stream)
                         (io/input-stream (io/file joystick-icon-path)))]
        (let [awt (ImageIO/read in)]
          (.setIconImage (Taskbar/getTaskbar) awt)))
      (catch Throwable t
        (println "Dock icon set failed:" (.getMessage t))))))

;; Inline version
(comment (defn prepare-context [context]
           (let [base (state/get-context)
                 svgs (core/update-all-svgs base)
                 base-path (System/getProperty "user.dir") ; or wherever your images are relative to
                 svg-strings (into {}
                                   (map (fn [[k svg]]
                                          [k (-> svg
                                                 (svg/inline-images base-path) ; Inline the images first
                                                 svg/hickory->svg-string)]) ; Then convert to string
                                        svgs))]
             (assoc base :svgs svg-strings))))

(defn prepare-context [context]
  (let [base (state/get-context)
        svgs (core/update-all-svgs base)
        base-path (System/getProperty "user.dir")
        svg-strings (into {}
                          (map (fn [[k svg]]
                                 [k (-> svg
                                        (svg/make-urls-absolute base-path)
                                        svg/hickory->svg-string)])
                               svgs))]
    (assoc base :svgs svg-strings)))

;; =============================================================================
;; Initial State
;; =============================================================================

(defn create-initial-state []
  (let [context (prepare-context (state/get-context))
        available-svgs (set (keys (:svgs context)))
        instances-with-svgs (->> (:instances context)
                                 (filter (fn [[_ svg-id]] (contains? available-svgs svg-id)))
                                 (into []))
        unmapped (core/find-unmapped-actions (:actionmaps context))
        active (some-> instances-with-svgs first first)]
    {:context context
     :instances instances-with-svgs
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

(defn instance-tab [{:keys [instance-id svg-id display-name svg]}]
  {:fx/type :tab
   :text (format "[%d] %s" instance-id display-name)
   :closable false
   :on-selection-changed {:event/type ::set-active-instance :instance-id instance-id}
   :content {:fx/type fx.ext.web-view/with-engine-props
             :desc {:fx/type :web-view
                    :pref-width 800
                    :pref-height 600}
             :props {:content svg
                     :on-status-changed {:event/type ::set-status}}}})

(defn svg-tab-pane [state]
  (let [{:keys [context instances active-instance]} state]
    {:fx/type :tab-pane
     :h-box/hgrow :always
     :tab-closing-policy :unavailable
     :tabs (mapv (fn [[instance-id svg-id]]
                   {:fx/type instance-tab
                    :fx/key instance-id
                    :instance-id instance-id
                    :svg-id svg-id
                    :display-name (core/svg-id->display-name context svg-id)
                    :svg (get-in context [:svgs svg-id])})
                 instances)}))

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
  (fx/mount-renderer *state renderer)
  (set-macos-dock-icon!)   (println "✓ GUI started"))

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
