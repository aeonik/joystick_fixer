(ns aeonik.controlmap.gui.main
  "Interactive GUI for exploring mapped SVGs and unmapped actions"
  (:require
   [clojure.string :as str]
   [cljfx.api :as fx]
   [clojure.java.io :as io]
   [cljfx.ext.web-view :as fx.ext.web-view]

   [aeonik.controlmap.core :as core]
   [aeonik.controlmap.state :as state]
   [aeonik.controlmap.svg :as svg]
   [aeonik.controlmap.gui.svg-viewer :as svg-viewer]
   [aeonik.controlmap.gui.image-view :as image-view]
   [aeonik.controlmap.gui.svg-component :as svgc])
  (:import
   [javafx.scene.image Image ImageView]
   [javafx.geometry Orientation]
   [javafx.scene.web WebEvent]
   [java.awt Taskbar Taskbar$Feature]
   [javax.imageio ImageIO]
   [javafx.stage FileChooser FileChooser$ExtensionFilter]
   [javafx.event ActionEvent EventHandler]
   [javafx.scene Node Cursor]
   [javafx.application Platform]
   [javafx.beans.value ChangeListener])
  (:gen-class))

(set! *warn-on-reflection* true)

(when (.startsWith (System/getProperty "os.name" "") "Mac")
  (System/setProperty "apple.awt.application.name" "Control Mapper"))

(def joystick-icon-path "images/gui_icon3.png")
;; Fix #1: Add type hint for Image constructor
(def joystick-icon (Image. ^String joystick-icon-path))

(defn fix-svg-text-positioning [svg-string]
  (if-let [[_ shift-str]
           (re-find #"\.button-text\s*\{[^}]*transform:\s*translate\(\s*([-\d.]+)px\s*,\s*0(?:px)?\s*\)\s*;?[^}]*\}"
                    svg-string)]
    (let [shift (Double/parseDouble shift-str)]
      (-> svg-string
          ;; Remove the CSS transform from button-text class. JavaFX's SVG loader
          ;; does not reliably honor it, so bake the horizontal offset into x attrs.
          (str/replace #"(transform:\s*translate\(\s*[-\d.]+px\s*,\s*0(?:px)?\s*\)\s*;?)" "")
          ;; Update text element x positions.
          (str/replace #"(<text[^>]*class=\"button-text\"[^>]*x=\")([-\d.]+)"
                       (fn [[_ prefix x-val]]
                         (let [x (Double/parseDouble x-val)]
                           (str prefix (+ x shift)))))
          ;; Update first-line tspans as well, since generated labels may carry x
          ;; on the tspan instead of inheriting from the text node.
          (str/replace #"(<tspan[^>]*x=\")([-\d.]+)"
                       (fn [[_ prefix x-val]]
                         (let [x (Double/parseDouble x-val)]
                           (str prefix (+ x shift)))))))
    svg-string))

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

(defn prepare-context
  "This is essentially the state create from the core program,
  probably want to move this to core or state.
  Though having the SVGs rendered and ready to go is a bit complicated, GUI specific.
  Not sure Yet..."
  [context]
  (let [base (state/get-context)
        svgs (core/update-all-svgs base)
        base-path (System/getProperty "user.dir")
        svg-strings (into {}
                          (map (fn [[k svg]]
                                 [k (-> svg
                                        (svg/make-urls-absolute base-path)
                                        svg/hickory->svg-string
                                        svg-viewer/strip-script
                                        fix-svg-text-positioning
                                        svg-viewer/normalize-svg-for-fx
                                        svg-viewer/inline-css-vars)])
                               svgs))]
    (assoc base :svgs svg-strings)))

;; =============================================================================
;; Initial State
;; =============================================================================

(defn create-initial-state
  "Function for priming the state for the GUI
  TODO: Move find-unmapped-actions computation to initial state creation, this is useful for more than Just GUI"
  []
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
     :show-unmapped? true
     :show-file-chooser? false}))

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

(defn file-chooser-button []
  {:fx/type :button
   :text "📁 Open..."
   :tooltip {:fx/type :tooltip :text "Choose a different actionmaps.xml file"}
   :on-action {:event/type ::choose-actionmaps}})

(defn map-event-handler [event]
  (case (:event/type event)
    ::shutdown (fn [state]
                 ;; In the packaged app, relying on implicit exit alone has proven
                 ;; unreliable. Explicitly terminate the JavaFX toolkit when the
                 ;; main window receives a close request.
                 (Platform/exit)
                 state)
    ::set-status (fn [state]
                   (let [^WebEvent we (:fx/event event)
                         msg (.getData we)]
                     (println "⚡ SVG clicked:" msg)
                     (assoc state :status msg)))
    ::set-filter-text #(assoc % :filter-text (:fx/event event))
    ::toggle-unmapped #(update % :show-unmapped? not)
    ::set-active-instance #(if (:fx/event event)
                             (assoc % :active-instance (:instance-id event))
                             %)
    ::choose-actionmaps (fn [state]
                          (println "Choose actionmaps clicked")
                          (let [^ActionEvent action-event (:fx/event event)
                                ^Node target (.getTarget action-event)
                                window (.getWindow (.getScene target))
                                ^FileChooser chooser (FileChooser.)
                                ^javafx.collections.ObservableList filters (.getExtensionFilters chooser)
                                ^java.util.List exts ["*.*" "*"]
                                all-filter (FileChooser$ExtensionFilter. "All Files" exts)]
                            (.setTitle chooser "Select actionmaps file")
                            (.clear filters)
                            (.add filters all-filter)
                            (if-let [^java.io.File file (.showOpenDialog chooser window)]
                              (try
                                (println "Loading actionmaps from:" (.getAbsolutePath file))
                                (state/init! :actionmaps-path file)
                                (create-initial-state)
                                (catch Exception e
                                  (println "Error loading actionmaps:" (.getMessage e))
                                  (assoc state :status (str "Error: " (.getMessage e)))))
                              (do
                                (println "No file selected")
                                state))))
    ::reload-context (fn [state]
                       (println "Reloading context...")
                       (try
                         (let [path (get-in state [:context :actionmaps-path])]
                           (state/init! :actionmaps-path path)
                           (create-initial-state))
                         (catch Exception e
                           (println "Error reloading context:" (.getMessage e))
                           state)))
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
                    :pref-width 1341
                    :pref-height 948}
             :props {:content svg
                     :on-status-changed {:event/type ::set-status}}}})

(defn instance-tab [{:keys [instance-id display-name]}]
  {:fx/type :tab
   :text (format "[%d] %s" instance-id display-name)
   :closable false
   :on-selection-changed {:event/type ::set-active-instance
                          :instance-id instance-id}
   :content (image-view/resizable-image-view
             {:image {:is (io/input-stream "resources/images/vpc_alpha_R.png")}})})

(defn instance-tab [{:keys [instance-id display-name svg]}]
  {:fx/type :tab
   :text (format "[%d] %s" instance-id display-name)
   :closable false
   :on-selection-changed {:event/type ::set-active-instance
                          :instance-id instance-id}
   :content (svg-viewer/svg-pane-str-scaled
             {:svg svg
              :on-click (fn [id]
                          (swap! *state assoc :status (str "clicked:" id)))})})

(defn instance-tab [{:keys [instance-id display-name svg]}]
  {:fx/type :tab
   :text (format "[%d] %s" instance-id display-name)
   :closable false
   :on-selection-changed {:event/type ::set-active-instance
                          :instance-id instance-id}
   :content {:fx/type svgc/svg-view
             :svg-content svg
             :scale-mode :contain
             :on-svg-click (fn [{:keys [button-id]}]
                             (swap! *state assoc :status (str "clicked:" button-id)))
             :pref-width  1341
             :pref-height 948}})

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
   [(file-chooser-button)
    {:fx/type :separator}
    {:fx/type :button
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
   :on-close-request {:event/type ::shutdown}
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
  ;; cljfx initializes JavaFX with implicit exit disabled for REPL friendliness.
  ;; Re-enable it here so closing the last window shuts the toolkit down even
  ;; when callers invoke `start!` directly instead of going through `-main`.
  (Platform/setImplicitExit true)
  (fx/mount-renderer *state renderer)
  (set-macos-dock-icon!)
  (println "✓ GUI started"))

(defn stop! []
  (fx/unmount-renderer *state renderer)
  (println "\u2713 GUI stopped"))

(defn restart! []
  (stop!)
  (Thread/sleep 100)
  (start!))

(defn -main [& _]
  (start!))

(comment
  (start!)
  (stop!)
  (restart!)

  (root-view @*state))
