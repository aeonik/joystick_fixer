(ns aeonik.controlmap.core
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [tupelo.forest :as f]
   [tupelo.parse.xml :as tx]))

(def actionmaps (-> "actionmaps.xml"
                    io/resource
                    io/reader
                    tx/parse-streaming))

(def svg (-> "svg/panel_1_boxes.svg"
             io/resource
             io/reader
             tx/parse-streaming))

(def instance->svg
  {1 "svg/alpha_L.svg"
   3 "svg/alpha_R.svg"
   4 "svg/panel_3.svg"              ; SharKa‑50 panel
   5 "svg/vpc_mongoose_t50cm3.svg"
   6 "svg/panel_1.svg"
   7 "svg/panel_2.svg"})

(def product->svg
  {#"L-VPC Stick MT-50CM2"     "svg/alpha_L.svg"
   #"R-VPC Stick MT-50CM2"     "svg/alpha_R.svg"
   #"VPC SharKa-50 Panel"      "svg/panel_3.svg"
   #"VPC Panel #1"             "svg/panel_1.svg"
   #"VPC Panel #2"             "svg/panel_2.svg"
   #"MongoosT-50CM3"           "svg/vpc_mongoose_t50cm3.svg"})

(def svg-roots
  (into {}
        (map (fn [[_ fname]]
               (let [resource (io/resource fname)]
                 (if resource
                   [fname (-> resource
                              io/reader
                              tx/parse-streaming)]
                   (do
                     (println "Resource not found:" fname)
                     [fname nil]))))
             instance->svg)))

(defn find-joystick-ids
  "Extracts joystick IDs and their corresponding SVGs from the given actionmaps.

  Parameters:
  - actionmaps: The actionmaps XML parsed structure.
  - product->svg: A mapping of product regex patterns to their corresponding SVG files.

  Returns:
  A map where the keys are joystick instance numbers and the values are their corresponding SVGs."
  [actionmaps product->svg]
  (f/with-forest (f/new-forest)
    (-> actionmaps
        f/add-tree-enlive
        (f/find-hids [:** {:type "joystick"}]) ; HIDs of every joystick
        (->>
         (keep (fn [hid]
                 (let [node (f/hid->node hid)
                       inst (some-> node :instance parse-long)
                       prod (:Product node)] ; may be nil!
                   (when (and inst prod)       ; <-- guard
                     (when-let [[_ svg]
                                (some (fn [[re s]]
                                        (when (re-find re prod) [re s]))
                                      product->svg)]
                       [inst svg])))))
         (into {})))))

(f/with-forest (f/new-forest)
  (-> actionmaps
      (f/add-tree-enlive)
      (f/find-paths [:** :rebind])
      f/format-paths))

(f/with-forest (f/new-forest)
  (-> actionmaps
      (f/add-tree-enlive)
      (f/find-paths [:** {:input "js4_ "}])
      f/format-paths))

(f/with-forest (f/new-forest)
  (-> actionmaps
      (f/add-tree-enlive)
      (f/find-paths-with [:** {:input :*}]
                         #(str/starts-with? (f/hid->attr (last %) :input) "js5_"))
      f/format-paths))

(defn extract-input-action-mappings
  "Extracts input-action mappings from the actionmaps by traversing the tree structure
   and fetching the corresponding input and action name for each rebind path.

   Returns a vector of maps containing input and action pairs."
  [actionmaps]
  (f/with-forest (f/new-forest)
    (-> actionmaps
        f/add-tree-enlive
        (f/find-paths [:** :rebind])
        (->>
         (map (fn [path]
                (let [child  (f/hid->node (peek path))
                      parent (f/hid->node (peek (pop path)))]
                  {:input  (:input child)
                   :action (:name  parent)})))
         (into [])))))

(defn parse-input [s]
  (when-let [[_ inst btn] (re-find #"js(\d+)_button(\d+)" s)]
    {:instance (parse-long inst)
     :btn      (parse-long btn)}))

;; ChatGPT Dreck below
(comment
  (defn apply-binding-to-svg!
    [svg-root {:keys [input action] :as _binding}]
    ;; parse the Star‑Citizen input string first
    (when-let [{:keys [btn]} (parse-input input)] ; btn nil ⇒ ignore axis bindings
      (when btn
        ;; make sure path ops run in the SAME forest that svg-root lives in
        (binding [f/*forest* (:forest svg-root)] ; `:forest` is added by add-tree-enlive
          (doseq [hid (f/find-hids svg-root
                                   [:** {:tag   :text
                                         :attrs {:data-for (str "btn_" btn)}}])]
            (f/set-node (f/hid->node hid) [action])))))
    svg-root)

  (defn btn-hids
    "Return a (possibly empty) seq of HIDs for <text data‑for=\"btn_N\"> nodes."
    [svg-root btn]
    (f/with-forest (f/new-forest)
      (f/find-hids (f/add-tree-enlive svg-root)
                   [:** :data-for "btn_24"])))

  (let [svg-root (svg-roots "svg/panel_1.svg")]
    (btn-hids svg-root 10))

  (defn apply-all-bindings-to-svg!
    [svg-root bindings]
    ;; run all path ops inside the forest the svg-root lives in
    (binding [f/*forest* (:forest svg-root)]
      (doseq [{:keys [input action]} bindings
              :let [{:keys [btn]} (parse-input input)
                    _              (when btn (println "Binding input:" input "with action:" action "and button:" btn))]
              :when btn]                ; skip axis entries
        (doseq [hid (f/find-hids svg-root
                                 [:** {:tag   :text
                                       :attrs {:data-for (str "btn_" btn)}}])]
          (f/set-node (f/hid->node hid) [action])))
      svg-root))

  (let [bindings  (extract-input-action-mappings actionmaps)
        svg-root  (first svg-roots)]
    (apply-all-bindings-to-svg! svg-root bindings))

  (defn apply-binding! [{:keys [input action]}]
    (when-let [{:keys [instance btn]} (parse-input input)]
      (when-let [svg-root (svg-roots (instance->svg instance))]
        (when btn
          (doseq [lbl (f/find-hid svg-root [:** {:tag :text
                                                 :attrs {:data-for (str "btn_" btn)}}])]
            (f/set-node lbl [action]))))))

  (f/with-forest (f/new-forest)
    (let [root-hid (f/add-tree-enlive actionmaps)
          bindings  (->> (f/find-paths root-hid [:** :rebind])
                         (map (fn [path]
                                (let [child  (f/hid->node (peek path))
                                      parent (f/hid->node (peek (pop path)))]
                                  {:input  (:input child)
                                   :action (:name  parent)})))
                         (into []))]
      (doseq [b bindings]
        (apply-binding! b))
      ;; finally, write each mutated SVG back to disk
      ))

  (doseq [b (extract-input-action-mappings actionmaps)]
    (apply-binding! b)))
