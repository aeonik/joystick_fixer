(ns user
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [clojure.string :as str]
            [cljfx.dev]
            [clojure.tools.namespace.repl :as namespace]
            [zprint.core :as z]))

(comment (clerk/serve! {:host "localhost" :browse true :watch-paths ["src"]}))

(def zprint-code-viewer
  {:name         `zprint-code-viewer
   :render-fn    'nextjournal.clerk.render/render-code
   :transform-fn (comp v/mark-presented
                       #(update-in % [:nextjournal/render-opts :language] (fn [lang] (or lang "clojure")))
                       (clerk/update-val (fn [v] (str/trim (with-out-str (z/zprint v {:map {:comma? true :indent 0 :justify? true}}))))))})

(require '[clj-async-profiler.core :as prof])

(defonce profiler-ui-server nil)

(defn start-profiler-ui!
  ([] (start-profiler-ui! 8080))
  ([port]
   (or profiler-ui-server
       (alter-var-root #'profiler-ui-server
                       (fn [server]
                         (or server
                             (prof/serve-ui port)))))))

(comment
  (start-profiler-ui!)

  (prof/start)

  (prof/stop))

(comment
  (require '[portal.api :as inspect])

  (def portal-instance
    (or (first (inspect/sessions))
        (inspect/open {:portal.colors/theme :portal.colors/gruvbox})))

  (add-tap #'inspect/submit))
