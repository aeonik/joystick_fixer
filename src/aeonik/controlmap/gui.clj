(ns aeonik.controlmap.gui
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [cljfx.api :as fx]
            [cljfx.ext.web-view :as fx.ext.web-view]
            [aeonik.controlmap.core :as core]
            [aeonik.controlmap.state :as state])
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
    :status nil
    :filter-text ""
    :unmapped-actions (core/empty-input-bindings state/actionmaps)}))

(defn handle-status-change [old new]
  (let [old-file (:active-file old)
        new-file (:active-file new)
        old-status (:status old)
        new-status (:status new)
        filename (.getName ^java.io.File new-file)]

    (cond
      ;; Tab switched
      (not= old-file new-file)
      (do
        (swap! *state assoc :status nil)
        (println "Switched tabs to:" filename))

      ;; Status changed in same tab
      (and new-status (not= old-status new-status))
      (cond
        (str/starts-with? new-status "clicked:")
        (let [id (subs new-status (count "clicked:"))]
          (println "Clicked:" id "in file:" filename))

        (str/starts-with? new-status "hovered:")
        (let [id (subs new-status (count "hovered:"))]
          (println "Hovered:" id "in file:" filename))

        :else
        (println "Unknown status message from" filename ":" new-status)))))

(add-watch *state ::status-watcher
           (fn [_ _ old new]
             (handle-status-change old new)))

(defn unmapped-actions-panel [filter-text unmapped-actions]
  (let [all-unmapped unmapped-actions
        filtered (if (str/blank? filter-text)
                   all-unmapped
                   (filter #(str/includes? (:action %) filter-text) all-unmapped))]
    {:fx/type :v-box
     :spacing 10
     :padding 10
     :children
     [{:fx/type :label
       :text "unmapped actions"
       :style "-fx-font-size: 16px; -fx-font-weight: bold;"}

      {:fx/type :text-field
       :prompt-text "filter..."
       :text filter-text
       :on-text-changed #(swap! *state assoc :filter-text %)}

      {:fx/type :list-view
       :v-box/vgrow :always
       :items (mapv :action filtered)
       :pref-height 300
       :pref-width 350}]}))

;; --- View Function ---
(defn view [{:keys [svg-files active-file status filter-text unmapped-actions]}]
  {:fx/type :stage
   :showing true
   :title (str (.getName ^java.io.File active-file))
   :scene
   {:fx/type :scene
    :root
    {:fx/type :h-box
     :spacing 20
     :padding 10
     :children
     [{:fx/type :tab-pane
       :side :top
       :h-box/hgrow :always
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
                          :on-status-changed
                          (fn [^WebEvent evt]
                            (swap! *state assoc :status (.getData evt)))}}})
              svg-files)}
      ;; Right-side global unmapped actions list
      (unmapped-actions-panel filter-text unmapped-actions)]}}})

;; --- Renderer ---
(def renderer
  (fx/create-renderer
   :middleware (fx/wrap-map-desc #'view)))

;; --- App Start ---
(fx/mount-renderer *state renderer)
