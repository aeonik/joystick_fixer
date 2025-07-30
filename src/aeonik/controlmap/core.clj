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

(defn find-joystick-bindings
  "Returns a list of enlive structures containing the action maps of a joystick. Needs to be filtered to be useful"
  [actionmaps js-num]
  (let [prefix (str "js" js-num "_")]
    (f/with-forest (f/new-forest)
      (map f/bush->enlive (-> actionmaps
                              (f/add-tree-enlive)
                              (f/find-paths-with [:** {:input :*}]
                                                 #(str/starts-with? (f/hid->attr (last %) :input) prefix))
                              (f/format-paths))))))
(comment
  (->> (find-joystick-bindings actionmaps 5)
       (map #(html/select % [:actionmap])))

  (->> (find-joystick-bindings actionmaps 5)
       (map #(html/select % [[:action (html/attr? :name)]])))

  (->> (find-joystick-bindings actionmaps 5)
       (mapcat #(html/select % [[:action (html/attr? :name)]]))
       (map #(html/attr-values % :name)))

  (->> (find-joystick-bindings actionmaps 5)
       (mapcat #(html/select % [[:action (html/attr? :name)]]))
       (into {} (map (fn [action]
                       [(get-in action [:attrs :name])
                        (-> action
                            (html/select [:rebind])
                            first
                            (get-in [:attrs :input]))]))))

  (-> xml-resource
      (html/select [:action])))

(defn joystick-action-mappings
  "Returns a vector of maps with action names, joystick inputs, and SVG button mappings."
  [actionmaps joystick-num]
  (->> (find-joystick-bindings actionmaps joystick-num)
       (mapcat #(html/select % [[:action (html/attr? :name)]]))
       (map (fn [action]
              (let [input (-> action
                              (html/select [:rebind])
                              first
                              (get-in [:attrs :input]))
                    button-num (when input
                                 (second (re-find #"button(\d+)" input)))]
                {:action (get-in action [:attrs :name])
                 :input input
                 :svg-input (when button-num (str "btn_" button-num))})))))

(comment
  (joystick-action-mappings actionmaps 1)

  (html/select svg [[:text (html/attr= :data-for "btn_27")]])

  (html/at svg
           [[:text (html/attr= :data-for "btn_27")]]
           (html/content "test"))

  (let [mappings (joystick-action-mappings actionmaps 1)]
    (reduce (fn [svg-doc {:keys [svg-input action]}]
              (if svg-input
                (html/at svg-doc
                         [[:text (html/attr= :data-for svg-input)]]
                         (html/content action))
                svg-doc))
            svg
            mappings))

  (->> (let [mappings (joystick-action-mappings actionmaps 1)]
         (reduce (fn [svg-doc {:keys [svg-input action]}]
                   (if svg-input
                     (html/at svg-doc
                              [[:text (html/attr= :data-for svg-input)]]
                              (html/content action))
                     svg-doc))
                 svg
                 mappings))
       html/emit*
       (apply str)
       (spit "output.svg")))

(defn update-svg-with-joystick-mappings
  "Updates SVG text elements with joystick action mappings."
  [svg actionmaps joystick-num]
  (let [mappings (joystick-action-mappings actionmaps joystick-num)]
    (reduce (fn [svg-doc {:keys [svg-input action]}]
              (if svg-input
                (html/at svg-doc
                         [[:text (html/attr= :data-for svg-input)]]
                         (html/content action))
                svg-doc))
            svg
            mappings)))

(comment (def updated-svg (update-svg-with-joystick-mappings svg actionmaps 2))

         (->> updated-svg
              html/emit*
              (apply str)
              (spit "updated-panel.svg"))

         (let [instance 4
               svg-location (instance->svg instance)
               svg (get svg-roots svg-location)
               updated-svg (update-svg-with-joystick-mappings svg actionmaps instance)]
           (->> updated-svg
                html/emit*
                (apply str)
                (spit "updated-panel.svg"))))

(defn parse-input [s]
  (when-let [[_ inst btn] (re-find #"js(\d+)_button(\d+)" s)]
    {:instance (parse-long inst)
     :btn      (parse-long btn)}))

(defn extract-button-from-id
  "Extract button number from id like 'lbl_btn_41' -> 41"
  [id]
  (when-let [[_ btn] (re-find #"lbl_btn_(\d+)" id)]
    (parse-long btn)))

(defn generate-all-updated-svgs! [actionmaps]
  (doseq [[instance svg-location] instance->svg]
    (when-let [svg (get svg-roots svg-location)]
      (let [updated-svg (update-svg-with-joystick-mappings svg actionmaps instance)
            output-filename (str "out/updated_" (last (clojure.string/split svg-location #"/")))]
        (->> updated-svg
             html/emit*
             (apply str)
             (spit output-filename))
        (println "Generated:" output-filename "for instance" instance)))))

(defn -main []
  (let [actionmaps (-> "actionmaps.xml"
                       io/resource
                       io/reader
                       tx/parse-streaming)]
    (generate-all-updated-svgs! actionmaps)))
