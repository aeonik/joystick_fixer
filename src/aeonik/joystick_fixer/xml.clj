(comment (ns aeonik.joystick-fixer.xml
           (:require [clojure.data.xml :as xml]
                     [clojure.xml]
                     [clojure.data.xml :as dxml]
                     [clojure.string :as str]
                     [clojure.data :as data]
                     [clojure.zip :as zip]
                     [tupelo.parse.xml :as tpx]
                     [tupelo.core :as t]
                     [tupelo.forest :as tf]
                     [aeonik.joystick-fixer.common :as common]
                     [clojure.pprint :refer [pprint]]
                     [instaparse.core :as instaparse]))

         (def file (slurp "resources/layout_virpil_control_panels_half_fixed_exported.xml"))

         (def xml-ebnf
           "document = element+
   element  = space?+ '<' tag_name attribute* ('>' eol? content* '</' tag_name '>' | '/>') eol?
   content  = element | text
   text     = #'[^<>]+'
   attribute = space attr_name '=' '\"' attr_value '\"'
   <attr_name> = #'[a-zA-Z_:][\\w:.-]*'
   <attr_value> = #'[^\"<>]*'
   <tag_name> = #'[a-zA-Z_:][\\w:.-]*'
   <space>    = #'\\s*'
   <eol>      = #'(\\r\\n|\\r|\\n)*'")

         (def parser (instaparse.core/parser xml-ebnf))
         (def parser (instaparse/parser xml-ebnf :output-format :enlive))

         (def instaparse-xml (instaparse/parse parser file))
         (comment (def tupelo-parse (tpx/parse file))
                  (def tupelo-raw-parse (tpx/parse-raw file)))
         (def xml-parse-str (xml/parse-str file))
         (xml/emit-str (tpx/parse file))

         (count file)
         (count (str/split-lines file))
         (count (str instaparse-xml))
         (count (str tupelo-raw-parse))
         (count (str xml-parse-str))

         (common/serialize-parse-tree instaparse-xml)

         (common/diff-lines file (common/serialize-parse-tree instaparse-xml))

         (take 5 (str/split-lines file))
         (take 2 (instaparse/parse parser file))

         (defn line-sort [string]
           (->> string
                (str/split-lines)
                (sort)
                (str/join "\n")))

         (common/diff-lines (line-sort file) (line-sort (clojure.data.xml/indent-str (tpx/parse file))))

         (data/diff (line-sort file) (line-sort (clojure.data.xml/indent-str (tpx/parse file))))

         (comment
           "Experimenting with converting instaparse parse trees to enlive parse trees but with the content converted to keywords instead of strings.
  Like how Tupelo does it."
           (def xml-ebnf
             "document = element+
            element  = '<' tag_name attribute* ('>' eol? content* '</' tag_name '>' | '/>') eol? {:tag :element}
            content  = element | text
            text     = #'[^<>]+' {:tag :text}
            attribute = space attr_name '=' '\"' attr_value '\"' {:tag :attribute}
            attr_name = #'[a-zA-Z_:][\\w:.-]*' {:tag :attr-name}
            attr_value = #'[^\"<>]*' {:tag :attr-value}
            tag_name = #'[a-zA-Z_:][\\w:.-]*' {:tag :tag-name}
            space    = #'\\s*'
            eol      = #'(\\r\\n|\\r|\\n)*'")

           (def parser (instaparse.core/parser xml-ebnf :auto-whitespace nil))

           (defn- transform-attribute [attr]
             (zipmap [:attr-name :attr-value] (map :content attr)))

           (defn- transform-element [element]
             (let [tag-name (first (:content element))
                   attributes (->> (:content element)
                                   (drop 1)
                                   (take-while #(= :attribute (:tag %)))
                                   (map transform-attribute)
                                   (into {}))
                   contents (->> (:content element)
                                 (drop (+ 1 (count attributes)))
                                 (filter #(not= :text (:tag %)))
                                 (map transform-element))]
               {:tag tag-name, :attrs attributes, :content contents}))

           (defn- transform-tree [tree]
             (if (map? tree)
               (transform-element tree)
               (if (vector? tree)
                 (map transform-tree tree)
                 tree)))

           (defn parse-xml [xml]
             (-> xml
                 parser
                 transform-tree))

           (println "In this version of the code:\n\n    The attribute, tag_name, attr_name, attr_value, element and text rules have :tag metadata associated with them. This metadata is used to identify the type of each node in the parse tree.\n\n    The transform-attribute function transforms an attribute node into a map with keys :attr-name and :attr-value.\n\n    The transform-element function transforms an element node into a map with keys :tag, :attrs and :content. It separates attributes from other content and puts them into the :attrs map.\n\n    The transform-tree function applies these transformations recursively to all nodes in the parse tree.\n\n    The parse-xml function applies the parser to an XML string and then transforms the resulting parse tree.\n\nThis should give you a parse tree in the desired format. You can then adjust your serialize-parse-tree function to handle this format. Please note that this solution still doesn't capture every detail of the original XML (like order of attributes and whitespace inside tags), but it should get you closer to what you're trying to achieve."))

         (comment
           "I want to keep references to the original nodes in the parse tree, so I'm going to add a :orig-node key to each node in the parse tree. This key will contain the original node from the parse tree. I'm going to use this key to identify nodes in the parse tree when I'm transforming it."
           (defn- transform-attribute [attr]
             (assoc (zipmap [:attr-name :attr-value] (map :content attr)) :orig-node attr))

           (defn- transform-element [element]
             (let [tag-name (first (:content element))
                   attributes (->> (:content element)
                                   (drop 1)
                                   (take-while #(= :attribute (:tag %)))
                                   (map transform-attribute)
                                   (into {}))
                   contents (->> (:content element)
                                 (drop (+ 1 (count attributes)))
                                 (filter #(not= :text (:tag %)))
                                 (map transform-element))]
               (assoc {:tag tag-name, :attrs attributes, :content contents} :orig-node element)))

           (defn- transform-tree [tree]
             (if (map? tree)
               (transform-element tree)
               (if (vector? tree)
                 (map transform-tree tree)
                 tree)))

           (defn parse-xml [xml]
             (-> xml
                 parser
                 transform-tree))))
