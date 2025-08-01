(ns aeonik.joystick-fixer.qjoypad
  (:require [instaparse.core :as instaparse]
            [instaparse.combinators :as cmb]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.java.shell :as sh]
            [clojure.pprint :refer [pprint]]
            [malli.core :as m]
            [ubergraph.core :as g])
  (:import [java.nio.file Files Paths]
           (org.apache.commons.io FileUtils)))

(def file "~/qjoypad_virpils.lyt")

;; Define a schema for a joystick.
(def joystick-schema
  [:map
   [:joystick-number :int]
   [:buttons
    [:vector [:map
              [:button-number :int]
              [:type :string]
              [:type-number :int]]]]])

(def joystick-test-data
  {:joystick-number 4
   :buttons [{:button-number 22, :type "key", :type-number 67}
             {:button-number 30, :type "key", :type-number 64}]})
(comment (m/validate joystick-schema joystick-test-data)

         (def file (slurp "/home/dave/qjoypad_virpils.lyt")))

(def joystick-tranforms {1 2
                         2 3
                         3 1
                         4 4})
(def qjoypad-ebnf "file = header joystick+
                   header = '# QJoyPad 4.3 Layout File' <newlines>
                   joystick = <'Joystick'> <space> joystick_number <space> <'{'> <newlines> key_map?+ <'}'> <newlines>
                   key_map = <tab> joystick_button type <newlines>?
                   joystick_button = <'Button'> <space> number <':'> <space>
                   type =  (#'\\w+' <space> type_button)
                   type_button = number
                   <space> = ' '
                   <newlines> = #'\\n+'
                   <tab> = #'\\t'
                   <joystick_number> = number
                   <number> = #'[0-9]+'")
(def qjoypad-parser (instaparse/parser qjoypad-ebnf :output-format :hiccup))

(comment (cmb/ebnf qjoypad-ebnf))
(def qjoypad-grammar-combinator
  {:file (cmb/cat
          (cmb/nt :header)
          (cmb/plus (cmb/nt :joystick)))
   :header (cmb/cat
            (cmb/string "# QJoyPad 4.3 Layout File")
            (cmb/nt :newlines))
   :joystick (cmb/cat
              (cmb/string "Joystick")
              (cmb/nt :space)
              (cmb/nt :joystick_number)
              (cmb/nt :space)
              (cmb/string "{")
              (cmb/nt :newlines)
              (cmb/star (cmb/nt :key_map))
              (cmb/string "}")
              (cmb/nt :newlines))
   :key_map (cmb/cat
             (cmb/nt :tab)
             (cmb/nt :joystick_button)
             (cmb/nt :type)
             (cmb/opt (cmb/nt :newlines)))
   :joystick_button (cmb/cat
                     (cmb/string "Button")
                     (cmb/nt :space)
                     (cmb/nt :number)
                     (cmb/string ":")
                     (cmb/nt :space))
   :type (cmb/cat
          (cmb/regexp "\\w+")
          (cmb/nt :space)
          (cmb/nt :type_button))
   :type_button (cmb/nt :number)
   :joystick_number (cmb/nt :number)
   :number (cmb/regexp "\\d+")
   :tab (cmb/string "\t")
   :newlines (cmb/regexp "\\n+")
   :space (cmb/string " ")})

(def qjoypad-parser-combinator (instaparse/parser qjoypad-grammar-combinator :start :file :output-format :enlive))

(def transformations
  {:file            (fn [header & joysticks] {:header header, :joysticks joysticks})
   :joystick        (fn [number & key_maps] {:joystick-number number, :key-maps key_maps})
   :key_map         (fn [joystick-button & types] {:joystick-button joystick-button, :types types})
   :joystick_button (fn [number] {:button-number number})
   :type            (fn [type-name & type-buttons] {:type-name type-name, :type-buttons type-buttons})
   :type_button     (fn [number] {:type-button-number number})})

(def transformations2
  {:file            (fn [& [header & joysticks]] {:header header, :joysticks joysticks})
   :joystick        (fn [& [number & key_maps]] {:joystick-number number, :key-maps key_maps})
   :key_map         (fn [& [joystick-button & types]] {:joystick-button joystick-button, :types types})
   :joystick_button (fn [& [number]] {:button-number number})
   :type            (fn [& [type-name & type-buttons]] {:type-name type-name, :type-buttons type-buttons})
   :type_button     (fn [& [number]] {:type-button-number number})})

(defn parse-qjoypad
  [file & {:as options}]
  (let [default-options {:grammar qjoypad-ebnf :output-format :hiccup}
        all-options (merge default-options options)
        grammar (get all-options :grammar)
        parse-options (select-keys all-options [:unhide]) ; select other keys if needed
        parser-options (dissoc all-options :unhide) ; remove keys passed to parse
        parser (instaparse/parser grammar parser-options)]
    (instaparse/parse parser file parse-options)))

(comment (parse-qjoypad file :grammar qjoypad-ebnf :output-format :hiccup)
         (parse-qjoypad file :grammar qjoypad-ebnf :output-format :enlive :unhide :all))

(def parse-tree (instaparse/parse qjoypad-parser file :unhide :all))
(def parse-tree-combinator (instaparse/parse qjoypad-parser-combinator file :unhide :all))

(comment (instaparse/transform transformations2 parse-tree))

(comment (-> file
             (instaparse/parse qjoypad-parser)
             (instaparse/transform transformations)))

(defn serialize-parse-tree
  "Unparses the tree
  This function currently requires enlive format"
  [tree]
  (cond
    (map? tree) (serialize-parse-tree (get tree :content))
    (coll? tree) (reduce str (map serialize-parse-tree tree))
    :else tree))

(def unparsed-tree (serialize-parse-tree parse-tree))

;; Compare strings
(comment (= file unparsed-tree))

;; Trying to come up with a way to transform arbitrary grammars
(defn transform-joystick [joystick-transforms tree]
  (clojure.walk/postwalk
   (fn [node]
     (if (and (vector? node) (= :joystick (first node)))
       (let [old-joystick-number (second node)
             new-joystick-number (joystick-transforms old-joystick-number)]
         (assoc node 1 new-joystick-number))
       node))
   tree))

(comment (transform-joystick joystick-tranforms parse-tree))

;; Text slinging, avoid this if possible, but it can work if the grammar is simple enough, or tolerant to index changes
(defn replace-text [text start end new-text]
  (str (subs text 0 start) new-text (subs text end)))

(def metadata {:instaparse.gll/start-index 27, :instaparse.gll/end-index 81})
(def modified-file-content
  (replace-text file
                (:instaparse.gll/start-index metadata)
                (:instaparse.gll/end-index metadata)
                "replacement text"))

(comment (defn replace-joystick-numbers-in-text [text parse-tree]
           (let [replacement-ranges (->> parse-tree
                                         (tree-seq vector? seq)
                                         (filter #(= :joystick_number (first %)))
                                         (map instaparse/span))
                 sorted-ranges (sort-by first replacement-ranges)
                 replacements (map (fn [[start end]]
                                     (let [old-number (Integer/parseInt (subs text start end))
                                           new-number (get joystick-transforms old-number)]
                                       (str new-number)))
                                   sorted-ranges)
                 parts (map (fn [[start end]] (subs text start end))
                            (partition 2 1 (interleave (cons 0 (map second sorted-ranges)) (cons (dec (count text)) nil))))]
             (apply str (interleave parts replacements)))))

;; Print with metadata
(comment (pprint (meta parse-tree))

         ;; Get a joystick from the hiccup parse tree using keywords :file and :joystick
         (get-in parse-tree [:header :joystick])

         (get-in parse-tree [2])
         (meta (get-in parse-tree [2])))

(comment
  (instaparse/span parse-tree)

  (instaparse/add-line-and-column-info-to-metadata file parse-tree)
  (def annotated-tree (instaparse/add-line-and-column-info-to-metadata file parse-tree)))
