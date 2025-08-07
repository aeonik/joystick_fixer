(ns user
  (:require [nextjournal.clerk :as clerk]
            [nextjournal.clerk.viewer :as v]
            [clojure.string :as str]
            [cljfx.dev]
            [zprint.core :as z]))

(comment (clerk/serve! {:host "localhost" :browse true :watch-paths ["src"]}))

(def zprint-code-viewer
  {:name         `zprint-code-viewer
   :render-fn    'nextjournal.clerk.render/render-code
   :transform-fn (comp v/mark-presented
                       #(update-in % [:nextjournal/render-opts :language] (fn [lang] (or lang "clojure")))
                       (clerk/update-val (fn [v] (str/trim (with-out-str (z/zprint v {:map {:comma? true :indent 0 :justify? true}}))))))})
