(ns aeonik.graph
  (:require [loom.graph :as lg]
            [thomasa.morpheus.core :as m]
            [clojure.set :as set]
            [ubergraph.core :as uber]
            [ubergraph.alg :as alg])
  (:import (java.time LocalDateTime)))

(def arguments ["src/aeonik/joystick_fixer/core.clj"])
(def exclude-regexp (re-pattern "clojure.core/.*|:clj-kondo/unknown-namespace/.*"))

(defn- ext-vars-for-var [graph var all-internal-vars]
  (let [all-subgraph-vars (m/->nodes (m/node->subgraph graph var))]
    (set/difference (set all-subgraph-vars)
                    (set (filter (set all-internal-vars) all-subgraph-vars)))))

(comment (let [analysis          (m/lint-analysis arguments)
               graph             (m/var-deps-graph analysis exclude-regexp)
               all-internal-vars (m/->vars analysis exclude-regexp)
               internal-vars     (if var [var] all-internal-vars)
               all-vars          (m/->nodes graph)
               ext-vars          (if var
                                   (ext-vars-for-var graph var all-internal-vars)
                                   (set/difference (set all-vars) (set internal-vars)))]))

(def analysis (m/lint-analysis arguments))
(def graph (m/var-deps-graph analysis nil exclude-regexp))
(def all-internal-vars (m/->vars analysis exclude-regexp))
;; (def internal-vars (if var [var] all-internal-vars))
(def all-vars (m/->nodes graph))
;; (def ext-vars (if var
;;                 (ext-vars-for-var graph var all-internal-vars)
;;                 (set/difference (set all-vars) (set internal-vars))))

(def uber-graph (uber/multidigraph (:adj graph)))

(uber/pprint (uber/remove-nodes uber-graph "aeonik.joystick-fixer.core/"))

(def new-graph (uber/remove-nodes uber-graph "aeonik.joystick-fixer.core/"))
(uber/pprint new-graph)

(uber/viz-graph new-graph
                {:save
                 {:filename (str "/home/dave/Projects/joystick_fixer/graphs/" (LocalDateTime/now) "-uber-graph.svg")
                  :format   :svg}
                 :rankdir "TB"
                 :nodesep "1.0"
                 :ranksep "3.0"
                 :splines "polyline"
                 :dir "forward"
                 :layout  :dot})

(uber/viz-graph new-graph
                {:save
                 {:filename (str "/home/dave/Projects/joystick_fixer/graphs/" (LocalDateTime/now) "-uber-graph.svg")
                  :format   :svg}
                 :layout  :sfdp
                 :beautify "true"
                 :K "0.01"
                 :overlap "prism"
                 :overlap_scaling "8"
                 :splines "polyline"})

(uber/viz-graph new-graph
                {:save
                 {:filename (str "/home/dave/Projects/joystick_fixer/graphs/" (LocalDateTime/now) "-uber-graph.svg")
                  :format   :svg}
                 :layout  :neato
                 :overlap "prism"
                 :overlap_scaling "10"})
