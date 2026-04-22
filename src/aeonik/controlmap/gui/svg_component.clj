(ns aeonik.controlmap.gui.svg-component
  "Production-ready idiomatic cljfx SVG component"
  (:require
   [aeonik.controlmap.gui.fx-util :as fxu]
   [aeonik.controlmap.gui.svg-viewer :as svg-viewer]
   [cljfx.api :as fx]
   [cljfx.lifecycle :as lifecycle]
   [clojure.java.io :as io]
   [clojure.string :as str])
  (:import
   [java.net URL]
   [javafx.event EventHandler]
   [javafx.geometry Orientation]
   [javafx.scene Node]
   [javafx.scene.layout Pane]
   [org.girod.javafx.svgimage SVGImage SVGLoader]))

;; =============================================================================
;; Helpers
;; =============================================================================

(defmacro with-fx [& body]
  `(eval-on-fx! (fn [] ~@body)))

(defn eval-on-fx!
  "Run `f` on the JavaFX thread; if it throws, rethrow on the calling thread
   (so CIDER opens *cider-error* instead of spewing to the REPL)."
  [f]
  (let [p (promise)]
    (fx/on-fx-thread
     (try (deliver p (f))
          (catch Throwable t (deliver p t))))
    (let [v @p]
      (when (instance? Throwable v)
        (throw v))
      v)))

(defn load-svg-content ^SVGImage [^String content]
  ;; content is the *actual SVG text*, not a path
  (fxu/on-fx-sync #(SVGLoader/load content)))

(defn load-svg-url ^SVGImage [u]
  ;; u may be a java.net.URL, a string URL, or a file path -> URL
  (let [^URL url (cond
                   (instance? URL u) u
                   (re-matches #"^[a-zA-Z]+:.*" (str u)) (URL. (str u))
                   :else (-> (io/file (str u)) .toURI .toURL))]
    (fxu/on-fx-sync #(SVGLoader/load url))))

(defn- parse-viewbox
  "Extract viewBox dimensions from SVG string, with sensible defaults"
  [^String svg]
  (if-let [[_ _ _ w h]
           (re-find #"viewBox\s*=\s*\"(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)\s+(\d+(?:\.\d+)?)\"" svg)]
    {:w (Double/parseDouble w) :h (Double/parseDouble h)}
    {:w 1000.0 :h 1000.0}))

(defn- load-svg-image ^SVGImage [^String svg]
  ;; if you pass in SVG text
  (doto (load-svg-content svg)
    ;; JavaFX SVG rendering does not consistently honor the transparent CSS used
    ;; by our overlay button rectangles, which makes them show up as black boxes.
    ;; Apply the same post-load fix used by the older svg-viewer path.
    (svg-viewer/make-buttons-transparent!)))

;; or, if your prop is a path/URL:
#_(defn- load-svg-image ^SVGImage [source]
    (svgl/load-svg-url source))

(defn load-svg-url ^SVGImage [u]
  ;; u may be a java.net.URL, a string URL, or a file path -> URL
  (let [^URL url (cond
                   (instance? URL u) u
                   (re-matches #"^[a-zA-Z]+:.*" (str u)) (URL. (str u))
                   :else (-> (io/file (str u)) .toURI .toURL))]
    (fxu/on-fx-sync #(SVGLoader/load url))))

(defn- make-click-handler
  "Create event handler that walks up node tree to find .button-box"
  [on-click]
  (when on-click
    (reify EventHandler
      (handle [_ e]
        (let [^Node target (.getTarget e)]
          (loop [n target]
            (when n
              (if (and (.getId n)
                       (some #{"button-box"} (.getStyleClass n)))
                (on-click {:button-id (.getId n)})
                (recur (.getParent n))))))))))

(defn normalize-svg
  "Clean up SVG string for JavaFX compatibility"
  [svg]
  (-> svg
      ;; Remove HTML wrapper
      (str/replace #"(?is)^.*?<svg" "<svg")
      (str/replace #"(?is)</svg>.*$" "</svg>")
      ;; Fix case sensitivity
      (str/replace #"(?i)\bviewbox\b" "viewBox")
      ;; Remove scripts for security
      (str/replace #"<script\b[^>]*>[\s\S]*?</script>" "")))

;; =============================================================================
;; Pane + state
;; =============================================================================

(defn- set-on-click! [^Pane pane handler]
  (.setOnMouseClicked pane (make-click-handler handler)))

(defn- set-scale-mode! [state ^Pane pane mode]
  (swap! state assoc :mode (keyword (name (or mode :contain))))
  (.requestLayout pane))

(defn- set-svg-content! [state ^Pane pane ^String raw]
  (let [svg  (some-> raw normalize-svg)
        root ^javafx.scene.Group (:root @state)
        kids (.getChildren root)]
    (.clear kids)
    (when svg
      (let [img  (load-svg-image svg)
            dims (parse-viewbox svg)]
        (.add kids img)
        (swap! state assoc
               :svg raw
               :svg-image img
               :dims dims)))
    ;; ensure layout reacts
    (.requestLayout pane)))

(defn- make-pane []
  (let [state  (atom {:svg nil
                      :svg-image nil
                      :dims {:w 1000.0 :h 1000.0}
                      :mode :contain
                      :root (javafx.scene.Group.)
                      :scale-xf (javafx.scene.transform.Scale. 1.0 1.0 0.0 0.0)}) ; sx,sy,pivot(0,0)
        pane  (proxy [Pane] []
                (isResizable [] true)
                (getContentBias [] Orientation/HORIZONTAL)
                (computePrefWidth [height]
                  (let [{:keys [w h]} (:dims @state)
                        aspect (if (pos? h) (/ w h) 1.0)]
                    (if (pos? height) (* height aspect) w)))
                (computePrefHeight [width]
                  (let [{:keys [w h]} (:dims @state)
                        aspect (if (pos? w) (/ h w) 1.0)]
                    (if (pos? width) (* width aspect) h)))
                (layoutChildren []
                  (let [{:keys [mode dims root scale-xf]} @state
                        {:keys [w h]} dims
                        pw (.getWidth ^Pane this)
                        ph (.getHeight ^Pane this)
                        sx (if (pos? w) (/ pw w) 1.0)
                        sy (if (pos? h) (/ ph h) 1.0)
                        s  (case mode
                             :contain    (min sx sy)
                             :cover      (max sx sy)
                             :fit-width  sx
                             :fit-height sy
                             :stretch    1.0)
                        fx (if (= mode :stretch) sx s)
                        fy (if (= mode :stretch) sy s)
                        sw (* w fx)
                        sh (* h fy)
                        ox (/ (- pw sw) 2.0)
                        oy (/ (- ph sh) 2.0)]
                    ;; scale + position the wrapper group
                    (.setX ^javafx.scene.transform.Scale scale-xf fx)
                    (.setY ^javafx.scene.transform.Scale scale-xf fy)
                    (.setLayoutX ^javafx.scene.Group root ox)
                    (.setLayoutY ^javafx.scene.Group root oy)
                    ;; clip only for :cover (and clear otherwise)
                    (.setClip ^Pane this
                              (when (= mode :cover)
                                (javafx.scene.shape.Rectangle. 0 0 pw ph))))))]
    ;; mount wrapper group + its scale transform once
    (doto (.getChildren pane)
      (.setAll (into-array javafx.scene.Node [(:root @state)])))
    (doto (.getTransforms ^javafx.scene.Group (:root @state))
      (.setAll (into-array javafx.scene.transform.Transform [(:scale-xf @state)])))
    (.. pane getProperties (put ::state state))
    pane))

(defn- apply-region-props!
  [^Pane pane {:keys [min-width min-height pref-width pref-height max-width max-height]}]
  (when (some? min-width)  (.setMinWidth  pane (double min-width)))
  (when (some? min-height) (.setMinHeight pane (double min-height)))
  (when (some? pref-width) (.setPrefWidth pane (double pref-width)))
  (when (some? pref-height) (.setPrefHeight pane (double pref-height)))
  (when (some? max-width)  (.setMaxWidth  pane (double max-width)))
  (when (some? max-height) (.setMaxHeight pane (double max-height))))

(defn- apply-props!
  [^Pane pane {:keys [svg-content scale-mode on-svg-click] :as props}]
  (let [state (.get (.getProperties pane) ::state)]
    (when (contains? props :svg-content)  (set-svg-content! state pane svg-content))
    (when (contains? props :scale-mode)   (set-scale-mode!  state pane scale-mode))
    (when (contains? props :on-svg-click) (set-on-click!     pane on-svg-click))
    (apply-region-props! pane props)))

;; =============================================================================
;; Lifecycle
;; =============================================================================

(def svg-pane-lifecycle
  (reify lifecycle/Lifecycle
    (create [_ desc _opts]
      (let [pane (make-pane)]
        (apply-props! pane desc)
        pane))
    (advance [_ pane desc _opts]
      (apply-props! pane desc)
      pane)
    (delete [_ _pane _opts]
      ;; nothing to dispose beyond GC
      nil)))

;; =============================================================================
;; Public API
;; =============================================================================

(defn svg-view
  "Declarative SVG component for cljfx.

   Props:
   - :svg-content  SVG string to display
   - :scale-mode   one of :contain (default) :cover :fit-width :fit-height :stretch
   - :on-svg-click (fn [{:keys [button-id]}])
   - Standard Region props (:pref-width, :pref-height, etc.)"
  [{:keys [svg-content scale-mode on-svg-click] :as props}]
  (-> {:fx/type svg-pane-lifecycle
       :svg-content svg-content
       :scale-mode (or scale-mode :contain)
       :on-svg-click on-svg-click}
      (merge (dissoc props :fx/type :svg-content :scale-mode :on-svg-click))))

(comment
  (require '[cljfx.api :as fx]
           '[aeonik.controlmap.gui.svg-component :as svgc])
  ;; => nil
  (def test-svg
    "<svg viewBox=\"0 0 100 100\">
     <rect x=\"5\"  y=\"5\"  width=\"40\" height=\"40\" fill=\"#4B9\"/>
     <rect id=\"btn1\" class=\"button-box\" x=\"55\" y=\"55\" width=\"40\" height=\"40\" fill=\"#D55\"/>
   </svg>")
  ;;
  ;; => #'user/test-svg
  (eval-on-fx!
   #(fx/create-component
     {:fx/type :stage
      :showing true
      :width 480 :height 360
      :title "svg-view smoke test"
      :scene {:fx/type :scene
              :root {:fx/type svgc/svg-view
                     :svg-content test-svg
                     :scale-mode :contain
                     :on-svg-click (println "CLICK:")
                     :pref-width 400 :pref-height 300}}})))
