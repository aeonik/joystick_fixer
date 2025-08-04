(ns aeonik.controlmap.gui
  (:require [clojure.string :as str]
            [cljfx.api :as fx]
            [cljfx.ext.web-view :as fx.ext.web-view])
  (:import [javafx.scene.web WebEvent]))

;; Application state
(def *state
  (atom
   {:title nil
    :status nil}))

(defn handle-status-change [status]
  (cond
    (str/starts-with? status "clicked:")
    (let [id (subs status (count "clicked:"))]
      (println "Performing Clojure logic for click on:" id))

    (str/starts-with? status "hovered:")
    (let [id (subs status (count "hovered:"))]
      (println "Hover detected on:" id))

    :else
    (println "Unhandled status message:" status)))

(add-watch *state ::status-watcher
           (fn [_ _ old new]
             (let [old-status (:status old)
                   new-status (:status new)]
               (when (not= old-status new-status)
                 (handle-status-change new-status)))))

;; View function that creates the GUI components
(defn view [{:keys [title status]}]
  {:fx/type :stage
   :showing true
   :title (str title)
   :scene
   {:fx/type :scene
    :root
    {:fx/type :v-box
     :fill-width true
     :children
     [{:fx/type fx.ext.web-view/with-engine-props
       :desc {:fx/type :web-view
              :max-width Double/MAX_VALUE
              :max-height Double/MAX_VALUE}
       :v-box/vgrow :always
       :props {:url (str "file://" (.getAbsolutePath (java.io.File. "out/updated_vpc_mongoose_t50cm3.svg")))
               :on-title-changed #(swap! *state assoc :title %)
               :on-status-changed #(swap! *state assoc :status (.getData ^WebEvent %))}}
      {:fx/type :label
       :text (str status)}]}}})

;; Renderer creation
(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc #'view)))

;; Mount the renderer to the application state
(fx/mount-renderer *state renderer)
