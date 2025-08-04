(ns aeonik.controlmap.gui
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cljfx.api :as fx]
            [cljfx.ext.web-view :as fx.ext.web-view])
  (:import [javafx.scene.web WebEvent]))

;; --- Application State ---
(def svg-files
  (->> (file-seq (io/file "out"))
       (filter #(and (.isFile %)
                     (str/ends-with? (.getName %) ".svg")))
       (sort-by #(.getName %))
       vec))

(def *state
  (atom
   {:svg-files svg-files
    :active-file (first svg-files)
    :status nil}))

;; --- Status Handler ---
(defn handle-status-change [status]
  (cond
    (str/starts-with? status "clicked:")
    (let [id (subs status (count "clicked:"))]
      (println "Clicked:" id))

    (str/starts-with? status "hovered:")
    (let [id (subs status (count "hovered:"))]
      (println "Hovered:" id))

    :else
    (println "Unknown status message:" status)))

(add-watch *state ::status-watcher
           (fn [_ _ old new]
             (let [old-status (:status old)
                   new-status (:status new)]
               (when (not= old-status new-status)
                 (handle-status-change new-status)))))

;; --- View Function ---
(defn view [{:keys [svg-files active-file status]}]
  {:fx/type :stage
   :showing true
   :title (str (.getName ^java.io.File active-file))
   :scene
   {:fx/type :scene
    :root
    {:fx/type :tab-pane
     :side :top
     :tabs (mapv
            (fn [^java.io.File f]
              {:fx/type :tab
               :id (.getPath f)
               :text (.getName f)
               :closable false
               :on-selection-changed
               (fn [e]
                 (let [^javafx.scene.control.Tab tab (.getSource e)]
                   (when (.isSelected tab)
                     (swap! *state assoc :active-file f))))
               :content
               {:fx/type fx.ext.web-view/with-engine-props
                :desc {:fx/type :web-view
                       :max-width Double/MAX_VALUE
                       :max-height Double/MAX_VALUE}
                :props {:url (str "file://" (.getAbsolutePath f))
                        :on-title-changed (fn [_]) ;; you could update title separately
                        :on-status-changed
                        (fn [^WebEvent evt]
                          (swap! *state assoc :status (.getData evt)))}}})
            svg-files)}}})

;; --- Renderer ---
(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc #'view)))

;; --- App Start ---
(fx/mount-renderer *state renderer)
