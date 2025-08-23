(ns aeonik.controlmap.gui.image-view
  "Resizable image view utilities for the GUI"
  (:require
   [cljfx.api :as fx]
   [cljfx.fx.image-view :as fx.image-view])
  (:import
   [javafx.scene.image Image ImageView]
   [javafx.geometry Orientation]))

(def ^:private ext-with-image-view-props
  (fx/make-ext-with-props fx.image-view/props))

(defn resizable-image-view
  "Creates a resizable image view that maintains aspect ratio"
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

(comment
  ;; Example usage:
  (resizable-image-view {:image {:is (clojure.java.io/input-stream "path/to/image.png")}}))