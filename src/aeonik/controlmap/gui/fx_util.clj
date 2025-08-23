(ns aeonik.controlmap.gui.fx-util
  (:import [javafx.application Platform]))

(defn on-fx-sync
  "Run f on the FX thread, starting the toolkit if needed (no Swing). Returns f's result."
  [f]
  (if (Platform/isFxApplicationThread)
    (f)
    (let [p (promise)]
      (try
        (Platform/runLater #(deliver p (f)))         ;; toolkit already started
        (catch IllegalStateException _               ;; toolkit not started yet
          (Platform/startup #(deliver p (f)))))
      @p)))
