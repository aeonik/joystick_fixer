(ns aeonik.controlmap.gui
  "Main entry point for the GUI system - re-exports from organized sub-namespaces"
  (:require
   [aeonik.controlmap.gui.main :as main]
   [aeonik.controlmap.gui.svg-viewer :as svg-viewer]
   [aeonik.controlmap.gui.image-view :as image-view]))

;; Re-export main GUI functions
(def start! main/start!)
(def stop! main/stop!)
(def restart! main/restart!)
(def -main main/-main)

;; Re-export SVG viewer functions
(def make-buttons-transparent! svg-viewer/make-buttons-transparent!)
(def svg-pane-str-scaled svg-viewer/svg-pane-str-scaled)
(def find-button-id svg-viewer/find-button-id)

;; Re-export image view functions
(def resizable-image-view image-view/resizable-image-view)

(comment
  ;; This namespace serves as the main entry point for the GUI system
  ;; All the actual functionality is organized in sub-namespaces:
  ;;
  ;; - aeonik.controlmap.gui.main - Core GUI application logic
  ;; - aeonik.controlmap.gui.svg-viewer - SVG viewing and manipulation
  ;; - aeonik.controlmap.gui.image-view - Resizable image view utilities
  ;;
  ;; Usage:
  ;; (require '[aeonik.controlmap.gui :as gui])
  ;; (gui/start!)
  ;; (gui/stop!)
  ;;
  ;; Or access specific functionality:
  ;; (require '[aeonik.controlmap.gui.svg-viewer :as svg])
  ;; (svg/make-buttons-transparent! img)
  )
