(ns aeonik.controlmap.gui.core
  "Pure, stateless GUI core. No atoms, no I/O, no JavaFX object construction."
  (:require [clojure.string :as str]
            [cljfx.api :as fx]
            [cljfx.ext.web-view :as fx.ext.web-view]
            [aeonik.controlmap.core :as core]
            [aeonik.controlmap.svg :as svg]))

;; -----------------------------------------------------------------------------
;; Pure context prep
;; -----------------------------------------------------------------------------

(defn prepare-context
  "Pure: given a base context map, a base-path string, and a map of hickory svgs,
   return context with :svgs replaced by absolute-URL svg strings."
  [{:keys [svgs] :as base-context} base-path hickory-svgs]
  (let [svg-strings (into {}
                          (map (fn [[k hsvg]]
                                 [k (-> hsvg
                                        (svg/make-urls-absolute base-path)
                                        svg/hickory->svg-string)]))
                          hickory-svgs)]
    (-> base-context
        (assoc :svgs svg-strings))))

(defn create-initial-state
  "Pure: derives initial GUI state from a supplied, already-prepared context."
  [context]
  (let [available-svgs (set (keys (:svgs context)))
        instances-with-svgs (->> (:instances context)
                                 (filter (fn [[_ svg-id]] (contains? available-svgs svg-id)))
                                 (into []))
        unmapped (core/find-unmapped-actions (:actionmaps context))
        active (some-> instances-with-svgs first first)]
    {:context context
     :instances instances-with-svgs
     :active-instance active
     :status nil
     :filter-text ""
     :unmapped-actions unmapped
     :show-unmapped? true
     ;; NOTE: icon is just a path (data). Shell turns it into a JavaFX Image.
     :icon-path "images/gui_icon3_transparent.png"}))

;; -----------------------------------------------------------------------------
;; Pure state transforms
;; -----------------------------------------------------------------------------

(defn update-filter-text [state new-text]
  (assoc state :filter-text (or new-text "")))

(defn toggle-unmapped [state]
  (update state :show-unmapped? not))

(defn set-active-instance [state instance-id]
  (assoc state :active-instance instance-id))

(defn set-status [state status-str]
  (assoc state :status status-str))

;; -----------------------------------------------------------------------------
;; Pure queries
;; -----------------------------------------------------------------------------

(defn filtered-unmapped-actions [{:keys [filter-text unmapped-actions]}]
  (let [ft (-> filter-text (or "") str/lower-case)]
    (if (str/blank? ft)
      unmapped-actions
      (filterv #(str/includes? (str/lower-case %) ft) unmapped-actions))))

;; -----------------------------------------------------------------------------
;; Pure views (data only). No Image objects, no I/O.
;; -----------------------------------------------------------------------------

(defn unmapped-actions-panel [state]
  (let [filtered (filtered-unmapped-actions state)]
    {:fx/type :v-box
     :spacing 10 :padding 10
     :min-width 300 :pref-width 350
     :children
     [{:fx/type :label
       :text "Unmapped Actions"
       :style "-fx-font-size:16px; -fx-font-weight:bold;"}
      {:fx/type :label
       :text (format "%d total, %d shown"
                     (count (:unmapped-actions state))
                     (count filtered))
       :style "-fx-text-fill: gray;"}
      {:fx/type :text-field
       :prompt-text "Filter actions..."
       :text (:filter-text state)
       :on-text-changed {:event/type ::set-filter-text}}
      {:fx/type :list-view
       :v-box/vgrow :always
       :items (mapv core/clean-action-name filtered)}]}))

(defn instance-tab [state instance-id svg-id]
  (let [context (:context state)
        display-name (core/svg-id->display-name context svg-id)
        svg (get-in context [:svgs svg-id])]
    {:fx/type :tab
     :text (format "[%d] %s" instance-id display-name)
     :closable false
     :on-selection-changed {:event/type ::set-active-instance
                            :instance-id instance-id}
     :content {:fx/type fx.ext.web-view/with-engine-props
               :desc {:fx/type :web-view
                      :pref-width 800
                      :pref-height 600}
               :props {:content svg
                       :on-status-changed {:event/type ::set-status}}}}))

(defn svg-tab-pane [state]
  {:fx/type :tab-pane
   :h-box/hgrow :always
   :tab-closing-policy :unavailable
   :tabs (mapv (fn [[instance-id svg-id]]
                 (instance-tab state instance-id svg-id))
               (:instances state))})

(defn control-toolbar [state]
  {:fx/type :tool-bar
   :items
   [{:fx/type :button
     :text "🔄 Reload"
     :tooltip {:fx/type :tooltip :text "Reload all mappings"}
     :on-action {:event/type ::reload-context}}
    {:fx/type :separator}
    {:fx/type :toggle-button
     :text "📋 Unmapped"
     :selected (boolean (:show-unmapped? state))
     :tooltip {:fx/type :tooltip :text "Show/hide unmapped actions"}
     :on-action {:event/type ::toggle-unmapped}}
    {:fx/type :separator}
    {:fx/type :button
     :text "📂 Export SVGs"
     :tooltip {:fx/type :tooltip :text "Generate SVG files"}
     :on-action {:event/type ::export-svgs}}
    {:fx/type :separator}
    {:fx/type :label
     :text (format "Instances: %d" (count (:instances state)))}]})

(defn root-view [state]
  ;; icon-path is just data; shell maps it to JavaFX Image.
  {:fx/type :stage
   :showing true
   :title (format "Control Mapper - Instance %s" (or (:active-instance state) "None"))
   :width 1400
   :height 900
   :scene {:fx/type :scene
           :root {:fx/type :v-box
                  :children
                  [(control-toolbar state)
                   {:fx/type :h-box
                    :v-box/vgrow :always
                    :spacing 10
                    :padding 10
                    :children
                    (cond-> [(svg-tab-pane state)]
                      (:show-unmapped? state)
                      (conj (unmapped-actions-panel state)))}]}}})

;; -----------------------------------------------------------------------------
;; Pure event reducer -> returns {:state new-state :effects [..]}
;; Effects are *descriptions* only; runner performs them.
;; -----------------------------------------------------------------------------

(defn process-event
  "Pure reducer. Returns {:state s' :effects [:effect ...]}.
   Recognized effects (all *data*):
   - [:effect/reload-context]                ; ask shell to refresh base context+hickory-svgs
   - [:effect/export-svgs (:context state)]  ; ask shell to write svgs to disk
   - [:effect/status-changed <string>]       ; status changes from WebEvent
  "
  [state {:keys [event/type fx/event instance-id] :as ev}]
  (case type
    ::set-filter-text {:state (update-filter-text state event)}
    ::toggle-unmapped {:state (toggle-unmapped state)}
    ::set-active-instance {:state (set-active-instance state instance-id)}
    ::reload-context {:state state
                      :effects [[:effect/reload-context]]}
    ::export-svgs {:state state
                   :effects [[:effect/export-svgs (:context state)]]}
    ::set-status {:state (set-status state (some-> event .getData str))
                  :effects [[:effect/status-changed (some-> event .getData str)]]}
    {:state state}))

;; -----------------------------------------------------------------------------
;; Renderer factory (pure)
;; -----------------------------------------------------------------------------

(defn renderer
  "Purely constructs a renderer that expects a *value* state (caller supplies)
   and returns a view description. Caller mounts & re-renders explicitly."
  []
  (fx/create-renderer
   :middleware (fx/wrap-map-desc (fn [s] (root-view s)))))

