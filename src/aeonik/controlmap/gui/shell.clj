(ns aeonik.controlmap.gui.shell
  (:require [cljfx.api :as fx]
            [aeonik.controlmap.gui.core :as ui]
            [aeonik.controlmap.core :as core]
            [aeonik.controlmap.state :as state]
            [aeonik.controlmap.svg :as svg]
            [clojure.java.io :as io])
  (:import [javafx.scene.image Image]))

;; -----------------------------------------------------------------------------
;; Side-effecting functions
;; -----------------------------------------------------------------------------

(defn load-base-context
  "Load the base context from your state/persistence layer"
  []
  (state/get-context))

(defn load-hickory-svgs
  "Load and process SVGs into hickory format"
  [ctx]
  (core/update-all-svgs ctx))

(defn generate-svgs!
  "Write SVG files to disk"
  [ctx]
  (core/generate-all-svgs! ctx))

(defn load-icon
  "Load a JavaFX Image from a path. Returns the Image object."
  [path]
  (try
    (if (io/resource path)
      (Image. (io/input-stream (io/resource path)))
      (Image. path))
    (catch Exception e
      (println "Warning: Could not load icon:" path)
      nil)))

;; -----------------------------------------------------------------------------
;; Effect handler
;; -----------------------------------------------------------------------------

(defn apply-effects!
  "Execute side effects described by the pure core.
   Effects are data structures like [:effect/reload-context] etc."
  [effects {:keys [*state renderer]}]
  (doseq [[effect-type & args] effects]
    (case effect-type
      :effect/reload-context
      (do
        (println "Reloading context...")
        (let [base (load-base-context)
              hsvg (load-hickory-svgs base)
              ctx  (ui/prepare-context base (System/getProperty "user.dir") hsvg)
              new-state (ui/create-initial-state ctx)]
          (reset! *state new-state)))

      :effect/export-svgs
      (do
        (println "Exporting SVGs...")
        (generate-svgs! (first args))
        (println "SVGs exported successfully"))

      :effect/status-changed
      ;; Just log or handle status changes as needed
      (when-let [status (first args)]
        (println "Status:" status))

      ;; Unknown effect - log it
      (println "Unknown effect:" effect-type))))

;; -----------------------------------------------------------------------------
;; Event dispatcher
;; -----------------------------------------------------------------------------

(defn create-event-handler
  "Creates an event handler that processes events through the pure reducer"
  [*state renderer]
  (fn [event]
    (let [current-state @*state
          {:keys [state effects]} (ui/process-event current-state event)]
      ;; Update state atom
      (reset! *state state)
      ;; Apply any side effects
      (when (seq effects)
        (apply-effects! effects {:*state *state :renderer renderer})))))

;; -----------------------------------------------------------------------------
;; Icon extension for cljfx
;; -----------------------------------------------------------------------------

;; cljfx needs to know how to handle :icon/path in the stage description
(def icon-extension
  "Extension to handle icon paths in stage descriptions"
  {:on-instance-lifecycle-event
   (fn [^javafx.stage.Stage stage event]
     (when (and (= :stage (:fx/type event))
                (= :created (:fx/event event)))
       (when-let [icon-paths (get-in event [:fx/desc :icons])]
         (let [icons (keep (fn [icon-spec]
                             (when (and (vector? icon-spec)
                                        (= :icon/path (first icon-spec)))
                               (load-icon (second icon-spec))))
                           icon-paths)]
           (when (seq icons)
             (.addAll (.getIcons stage) icons))))))})

;; -----------------------------------------------------------------------------
;; Application lifecycle
;; -----------------------------------------------------------------------------

(defn start!
  "Initialize and start the GUI application"
  []
  (println "Starting Control Mapper GUI...")

  ;; Load initial data
  (let [base-context (load-base-context)
        hickory-svgs (load-hickory-svgs base-context)
        prepared-ctx (ui/prepare-context base-context
                                         (System/getProperty "user.dir")
                                         hickory-svgs)
        initial-state (ui/create-initial-state prepared-ctx)

        ;; Create state atom
        *state (atom initial-state)

        ;; Create renderer with event handler
        event-handler (create-event-handler *state nil) ; Will update with renderer
        renderer (fx/create-renderer
                  :middleware (fx/wrap-map-desc ui/root-view)
                  :opts {:fx.opt/map-event-handler event-handler})]

    ;; Update event handler with renderer reference
    (let [handler-with-renderer (create-event-handler *state renderer)]
      ;; Mount and start rendering
      (fx/mount-renderer *state renderer))

    (println "GUI started successfully")

    ;; Return app handle for REPL interaction
    {:*state *state
     :renderer renderer
     :reload! (fn []
                (apply-effects! [[:effect/reload-context]]
                                {:*state *state :renderer renderer}))}))

;; -----------------------------------------------------------------------------
;; REPL helpers
;; -----------------------------------------------------------------------------

(defn stop!
  "Stop the renderer"
  [{:keys [renderer]}]
  (when renderer
    (fx/unmount-renderer renderer)
    (println "GUI stopped")))

(defn restart!
  "Restart the application"
  [app]
  (when app
    (stop! app))
  (start!))

;; -----------------------------------------------------------------------------
;; Main entry point
;; -----------------------------------------------------------------------------

(defn -main [& args]
  (fx/on-fx-thread
   (start!)))

;; For REPL development:
(comment
  (def app (start!))
  (stop! app)
  (restart! app)

  ;; Inspect current state
  @(:*state app)

  ;; Manually reload context
  ((:reload! app))

  ;; Manually trigger an event
  (let [handler (get-in (:renderer app) [:opts :fx.opt/map-event-handler])]
    (handler {:event/type ::ui/toggle-unmapped})))
