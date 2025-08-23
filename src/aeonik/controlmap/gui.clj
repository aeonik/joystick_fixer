(ns aeonik.controlmap.gui
  "Interactive GUI for exploring mapped SVGs and unmapped actions"
  (:require
   [clojure.string :as str]
   [cljfx.api :as fx]
   [clojure.java.io :as io]
   [cljfx.ext.web-view :as fx.ext.web-view]
   [cljfx.fx.image-view :as fx.image-view]
   [aeonik.controlmap.core :as core]
   [aeonik.controlmap.state :as state]
   [aeonik.controlmap.svg :as svg])
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
   [javafx.beans.value ChangeListener]
   [org.girod.javafx.svgimage SVGLoader SVGImage SVGImageRegion])
  (:gen-class))

(set! *warn-on-reflection* true)

(when (.startsWith (System/getProperty "os.name" "") "Mac")
  (System/setProperty "apple.awt.application.name" "Control Mapper"))

(def joystick-icon-path "images/gui_icon3.png")
;; Fix #1: Add type hint for Image constructor
(def joystick-icon (Image. ^String joystick-icon-path))

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

(comment
  "I used to inline the entire SVG, but I was able to figure out how to get the SVG to be loaded from disk, probably don't want to use this technique,
but knowing about it, and having the capibility around seems useful"
  (defn prepare-context [context]
    (let [base (state/get-context)
          svgs (core/update-all-svgs base)
          base-path (System/getProperty "user.dir")
          svg-strings (into {}
                            (map (fn [[k svg]]
                                   [k (-> svg
                                          (svg/inline-images base-path)
                                          svg/hickory->svg-string)])
                                 svgs))]
      (assoc base :svgs svg-strings))))

(def ^:private ext-with-image-view-props
  (fx/make-ext-with-props fx.image-view/props))

(defn resizable-image-view
  [{:keys [image]}]
  (letfn [(aspect-ratio [^Image img]
            (let [h (some-> img .getHeight)]
              (if (and h (pos? h))
                (/ (.getWidth ^Image img) h)
                1.0)))
          (img-width  [^Image img]
            (or (some-> img .getWidth) 0.0))
          (img-height [^Image img]
            (or (some-> img .getHeight) 0.0))
          (pref [constraint img scale-fn fallback]
            (if (pos? constraint)
              (scale-fn constraint (aspect-ratio img))
              (fallback img)))]
    {:fx/type ext-with-image-view-props
     :desc {:fx/type fx/ext-instance-factory
            :create (fn []
                      (proxy [ImageView] []
                        (minWidth  [_] 0.0)
                        (minHeight [_] 0.0)
                        (prefWidth [h]
                          (let [^Image img (.getImage ^ImageView this)]
                            (pref h img * img-width)))
                        (prefHeight [w]
                          (let [^Image img (.getImage ^ImageView this)]
                            (pref w img (fn [x ar] (/ x ar)) img-height)))
                        (isResizable [] true)
                        (resize [w h]
                          (.setFitWidth  ^ImageView this w)
                          (.setFitHeight ^ImageView this h))
                        (getContentBias [] Orientation/HORIZONTAL)))}
     :props {:preserve-ratio true
             :image image}}))

;; Going to remove these after testing. Point directly to SVG or parse directly in future

(defn normalize-svg-for-fx [s]
  (let [;; unwrap any <html> wrapper so root is <svg …>
        s (-> s
              (str/replace #"(?is)^.*?<svg" "<svg")
              (str/replace #"(?is)</svg>.*$" "</svg>"))
        ;; fix case of viewBox
        s (str/replace s #"(?i)\bviewbox\b" "viewBox")
        ;; ensure xlink namespace if we decide to use xlink:href
        s (if (re-find #"xmlns:xlink" s)
            s
            (str/replace s #"(?i)<svg\b" "<svg xmlns:xlink=\"http://www.w3.org/1999/xlink\""))
        ;; (optional) make <image href=…> also available as xlink:href=…
        s (if (re-find #"xlink:href" s)
            s
            (str/replace s #"(?i)(<image\b[^>]*?)\bhref="
                         "$1xlink:href="))]
    s))

(defn strip-script [s]
  (str/replace s #"<script\b[^>]*>[\s\S]*?</script>" ""))

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
                                        strip-script
                                        normalize-svg-for-fx)])
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

(defn svg-region ^SVGImageRegion [^String path]
  (let [^SVGImage img (SVGLoader/load (clojure.java.io/file path))]
    (.createRegion img))) ; resizes with parent, preserves aspect

(defn ^SVGImageRegion svg-region-from-string
  "Persist SVG string to a temp file and load as a resizable Region."
  [^String svg]
  (let [^java.io.File tmp (java.io.File/createTempFile "panel" ".svg")]
    (.deleteOnExit tmp)
    (spit tmp svg)
    (let [^SVGImage img (SVGLoader/load tmp)]
      (.createRegion img))))

(defn ^String find-button-id ^String [^Node n]
  (loop [m n]
    (when m
      (if (and (.getId m)
               (.contains (.getStyleClass m) "button-box"))
        (.getId m)
        (recur (.getParent m))))))

(defn svg-pane-str [{:keys [^String svg on-click]}]
  {:fx/type fx/ext-instance-factory
   :create (fn []
             (let [^SVGImage img (SVGLoader/load svg)        ;; ← loads directly from STRING
                   ^SVGImageRegion region (.createRegion img)]
               (.setOnMouseClicked region
                                   (reify EventHandler
                                     (handle [_ e]
                                       (when-let [bid (some-> (.getTarget e) find-button-id)]
                                         (when on-click (on-click bid))))))
               region))})

(defn viewbox-wh
  "Returns {:w .. :h ..} from the SVG string (defaults to 1000x1000 if missing)."
  ^java.util.Map [^String s]
  (if-let [[_ a b c d]
           (re-find #"viewBox\s*=\s*\"(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)\"" s)]
    {:w (Double/parseDouble c) :h (Double/parseDouble d)}
    {:w 1000.0 :h 1000.0}))

;; anchor can be :center (default), :topleft, :top, :left
(defn svg-pane-str-scaled
  [{:keys [^String svg on-click mode] :or {mode :contain}}]
  {:fx/type fx/ext-instance-factory
   :create
   (fn []
     (let [^org.girod.javafx.svgimage.SVGImage img (org.girod.javafx.svgimage.SVGLoader/load svg)
           {:keys [w h]} (viewbox-wh svg)
           scale-xf (javafx.scene.transform.Scale. 1.0 1.0 0.0 0.0) ; scaleX, scaleY, pivotX=0, pivotY=0
           pane (proxy [javafx.scene.layout.Pane] []
                  (computePrefWidth  [_] (double w))
                  (computePrefHeight [_] (double h))
                  (computeMinWidth  [_] 0.0)
                  (computeMinHeight [_] 0.0)
                  (layoutChildren []
                    (let [pw (.getWidth this)
                          ph (.getHeight this)
                          sx (/ pw w)
                          sy (/ ph h)
                          s  (case mode
                               :contain    (min sx sy)
                               :cover      (max sx sy)
                               :fit-width  sx
                               :fit-height sy)
                          aw (* w s)
                          ah (* h s)
                          center? (#{:contain :cover} mode)
                          ox (if center? (/ (- pw aw) 2.0) 0.0)
                          oy (if center? (/ (- ph ah) 2.0) 0.0)]
                      ;; scale the whole SVG node tree (text + paths) uniformly
                      (.setX scale-xf s)
                      (.setY scale-xf s)
                      (.setLayoutX img ox)
                      (.setLayoutY img oy)
                      ;; clip only for cover
                      (.setClip this (when (= mode :cover)
                                       (javafx.scene.shape.Rectangle. 0 0 pw ph))))))]
       ;; attach transform and some perf hints
       (doto (.getTransforms img)
         (.setAll (into-array javafx.scene.transform.Transform [scale-xf])))
       (.setManaged img false)
       (.setCache img true)
       (.setCacheHint img javafx.scene.CacheHint/SCALE)

       ;; events
       (.setOnMouseClicked img
                           (reify javafx.event.EventHandler
                             (^void handle [_ ^javafx.event.Event e]
                               (when-let [bid (some-> (.getTarget e) find-button-id)]
                                 (when on-click (on-click bid))))))

       (doto (.getChildren pane) (.setAll (into-array javafx.scene.Node [img])))
       pane))})

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
   :content (resizable-image-view
             {:image {:is (io/input-stream "resources/images/vpc_alpha_R.png")}})})

(defn instance-tab [{:keys [instance-id display-name svg]}]
  {:fx/type :tab
   :text (format "[%d] %s" instance-id display-name)
   :closable false
   :on-selection-changed {:event/type ::set-active-instance
                          :instance-id instance-id}
   :content (svg-pane-str-scaled
             {:svg svg
              :on-click (fn [id]
                          (swap! *state assoc :status (str "clicked:" id)))})})

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
  (Platform/setImplicitExit true)
  (start!))

(comment
  (start!)
  (stop!)
  (restart!))
