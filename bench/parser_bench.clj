(ns parser-bench
  (:require
   [clojure.data.xml :as xml]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [criterium.core :as crit]
   [hickory.core :as h]
   [instaparse.core :as insta :refer [defparser]]
   [net.cgrand.xml :as exml]
   [tupelo.parse.xml :as txml]
   [riveted.core :as vtd])
  (:import
   [java.io ByteArrayInputStream ByteArrayOutputStream]))

(def xml-name-pattern
  ;; (?u) enables Unicode-aware categories so \x{...} matches high planes correctly.
  (str
   "(?u)"
   "("
   "[:A-Z_a-z"
   "\\u00C0-\\u00D6"
   "\\u00D8-\\u00F6"
   "\\u00F8-\\u02FF"
   "\\u0370-\\u037D"
   "\\u037F-\\u1FFF"
   "\\u200C-\\u200D"
   "\\u2070-\\u218F"
   "\\u2C00-\\u2FEF"
   "\\u3001-\\uD7FF"
   "\\uF900-\\uFDCF"
   "\\uFDF0-\\uFFFD"
   "]"
   "|[\\x{10000}-\\x{EFFFF}]"
   ")"
   "("
   "[:A-Z_a-z"
   "\\u00C0-\\u00D6"
   "\\u00D8-\\u00F6"
   "\\u00F8-\\u02FF"
   "\\u0370-\\u037D"
   "\\u037F-\\u1FFF"
   "\\u200C-\\u200D"
   "\\u2070-\\u218F"
   "\\u2C00-\\u2FEF"
   "\\u3001-\\uD7FF"
   "\\uF900-\\uFDCF"
   "\\uFDF0-\\uFFFD"
   "\\-\\.0-9"
   "\\u00B7"
   "\\u0300-\\u036F"
   "\\u203F-\\u2040"
   "]"
   "|[\\x{10000}-\\x{EFFFF}]"
   ")*"))

(def xml-ebnf
  "Evaluation count : 6 in 6 samples of 1 calls.
             Execution time mean : 340.122463 ms
    Execution time std-deviation : 26.585187 ms
   Execution time lower quantile : 304.205100 ms ( 2.5%)
   Execution time upper quantile : 367.695569 ms (97.5%)
                   Overhead used : 1.965770 ns"
  "document = prolog? element+

(* --- prolog / pre-root stuff --- *)
<prolog>   = xmldecl? (comment)*

<xmldecl>  = <'<?'> (!'?>' NAME (attribute)* )* <'?>'>
<comment>  = <'<!--'> (!'-->' NAME)* <'-->'>

(* --- elements --- *)
element    = empty-elem / node
node       = start-tag (TEXT | element)* end-tag

<SELF_CLOSE> = '/>'

<start-tag>  = <'<'> NAME (attribute)* !SELF_CLOSE <'>'>
<end-tag>    = <'</'> NAME <'>'>
empty-elem = <'<'> NAME (attribute)* <SELF_CLOSE>

attribute  = NAME <'='> (dq | sq)
<dq> = <'\"'> #'[^\"]*' <'\"'>     (* inner text only, quotes hidden *)
<sq> = <'\\''> #'[^\\']*' <'\\''>  (* inner text only, quotes hidden *)

NAME = #'[A-Za-z_][A-Za-z0-9_.:-]*'
TEXT = #'[^<]+'")

(def xml-ebnf
  "Evaluation count : 6 in 6 samples of 1 calls.
             Execution time mean : 229.278345 ms
    Execution time std-deviation : 21.785572 ms
   Execution time lower quantile : 206.227078 ms ( 2.5%)
   Execution time upper quantile : 258.165723 ms (97.5%)
                   Overhead used : 1.965770 ns"
  "document = prolog? element comment*

   (* --- prolog / pre-root stuff --- *)
   <prolog> = xmldecl comment*
   xmldecl = <'<?xml'> (attribute)* <'?>'>
   comment = <'<!--'> comment-text <'-->'>
   <comment-text> = #'(?:[^-]|-(?!-))*'

   (* --- elements --- *)
   element = empty-elem | node
   node = start-tag content end-tag
   <content> = (char-data | element | comment)*

   (* --- tags --- *)
   <start-tag> = <'<'> tag-name (attribute)* <'>'>
   <end-tag> = <'</'> tag-name <'>'>
   empty-elem = <'<'> tag-name (attribute)* <'/>'>

   (* --- attributes --- *)
   attribute = attr-name <'='> attr-value
   <attr-value> = dq-string | sq-string
   <dq-string> = <'\"'> dq-content <'\"'>
   <sq-string> = <\"'\"> sq-content <\"'\">
   dq-content = #'[^\"]*'
   sq-content = #'[^\\']*'

   (* --- names and text --- *)
   tag-name = #'[A-Za-z_][A-Za-z0-9_.:-]*'
   attr-name = #'[A-Za-z_][A-Za-z0-9_.:-]*'
   char-data = #'[^<]+'")

(def xml-ebnf
  "Evaluation count : 6 in 6 samples of 1 calls.
             Execution time mean : 277.224294 ms
    Execution time std-deviation : 13.980255 ms
   Execution time lower quantile : 260.203663 ms ( 2.5%)
   Execution time upper quantile : 292.098419 ms (97.5%)
                   Overhead used : 1.965770 ns"
  "document = xmldecl? element trailing

   (* Collapse everything possible into regexes to avoid character-by-character parsing *)
   xmldecl = #'<\\?xml[^?>]*\\?>'
   trailing = #'(?:<!--(?:[^-]|-(?!-))*-->)*'

   (* Use ordered choice with most common case first *)
   element = node / empty-elem / comment

   (* Avoid building tokens character by character - use full regexes *)
   empty-elem = '<' tag-name attrs '/>'
   node = '<' tag-name attrs '>' content '</' tag-name '>'
   comment = #'<!--(?:[^-]|-(?!-))*-->'

   (* Use greedy regex for text content *)
   <content> = (#'[^<]+' / element / comment)*

   (* Minimize ambiguity - attrs as optional single regex when possible *)
   <attrs> = attr*
   attr = attr-name '=' attr-value

   (* String literals are faster than regexes for fixed strings *)
   <attr-value> = dq-value / sq-value
   dq-value = '\"' #'[^\"]*' '\"'
   sq-value = \"'\" #\"[^']*\" \"'\"

   (* Single regex for entire token *)
   tag-name = #'[A-Za-z_][A-Za-z0-9_.:-]*'
   attr-name = #'[A-Za-z_][A-Za-z0-9_.:-]*'")

(def xml-ebnf
  "Evaluation count : 6 in 6 samples of 1 calls.
             Execution time mean : 271.690583 ms
    Execution time std-deviation : 8.064689 ms
   Execution time lower quantile : 259.873377 ms ( 2.5%)
   Execution time upper quantile : 280.773349 ms (97.5%)
                   Overhead used : 1.965770 ns"
  "document = prolog element misc-end

   (* Prolog - be specific to avoid backtracking *)
   <prolog> = xmldecl? misc
   xmldecl = '<?' xml-pi '?>'
   <xml-pi> = 'xml' version-info encoding-decl? standalone-decl?
   version-info = 'version' eq ('\"1.0\"' | \"'1.0'\" | '\"1.1\"' | \"'1.1'\")
   encoding-decl = 'encoding' eq enc-name
   standalone-decl = 'standalone' eq ('\"yes\"' | \"'yes'\" | '\"no\"' | \"'no'\")
   <eq> = '='
   <enc-name> = '\"' #'[A-Za-z][A-Za-z0-9._-]*' '\"' | \"'\" #'[A-Za-z][A-Za-z0-9._-]*' \"'\"

   (* Misc - comments and PIs that can appear between elements *)
   <misc> = (comment | pi)*
   <misc-end> = (comment | pi)*

   (* Elements - use ordered choice with most common first *)
   element = empty-element | start-tag content end-tag

   (* Start with a single character lookahead to branch early *)
   <content> = (char-data? ((element | reference | cdata | pi | comment) char-data?)*)

   (* Character data - grab as much as possible in one go *)
   char-data = #'[^<&]+'

   (* Tags - be very specific to avoid backtracking *)
   start-tag = '<' name attrs '>'
   end-tag = '</' name '>'
   empty-element = '<' name attrs '/>'

   (* Attributes - minimize rule depth *)
   <attrs> = attr*
   attr = name eq att-value
   <att-value> = ('\"' att-chars-dq '\"') | (\"'\" att-chars-sq \"'\")
   <att-chars-dq> = #'[^<&\"]*'
   <att-chars-sq> = #\"[^<&']*\"

   (* Names - single regex *)
   name = #'[A-Za-z_:][A-Za-z0-9._:-]*'

   (* References - be specific *)
   reference = entity-ref | char-ref
   entity-ref = '&' #'[A-Za-z_:][A-Za-z0-9._:-]*' ';'
   char-ref = '&#' #'[0-9]+' ';' | '&#x' #'[0-9a-fA-F]+' ';'

   (* CDATA sections *)
   cdata = '<![CDATA[' cdata-content ']]>'
   <cdata-content> = #'(?:[^\\]]|\\](?!\\]>))*'

   (* Comments - single regex *)
   comment = #'<!--(?:[^-]|-(?!->))*-->'

   (* Processing instructions *)
   pi = '<?' pi-target pi-content '?>'
   <pi-target> = #'[A-Za-z_:][A-Za-z0-9._:-]*'
   <pi-content> = #'(?:[^?]|\\?(?!>))*'")

(def xml-ebnf
  "Evaluation count : 6 in 6 samples of 1 calls.
             Execution time mean : 202.247320 ms
    Execution time std-deviation : 20.903381 ms
   Execution time lower quantile : 187.398149 ms ( 2.5%)
   Execution time upper quantile : 236.362589 ms (97.5%)
                   Overhead used : 1.965770 ns

Found 1 outliers in 6 samples (16.6667 %)
	low-severe	 1 (16.6667 %)
 Variance from outliers : 30.6246 % Variance is moderately inflated by outliers"
  "document = prolog? element misc?

   (* Simplified regexes for speed *)
   prolog = #'<\\?xml[^?]*\\?>'
   misc = #'(?:<!--[^-]*(?:-[^-]+)*-->|<\\?[^?]*\\?>)*'

   (* Structure *)
   element = node / empty-elem
   empty-elem = '<' name attrs '/>'
   node = '<' name attrs '>' content '</' name '>'

   (* Simplified content matching *)
   <content> = (element / text / special)*
   text = #'[^<]+'
   special = #'<[!?][^>]*>'

   (* Attributes *)
   <attrs> = attr*
   attr = name '=' value
   value = #'\"[^\"]*\"|\\'[^\\']*\\''

   (* Name *)
   name = #'[A-Za-z_:][\\w.:-]*'")

(def xml-ebnf
  "document = prolog? element

   (* Simplified regexes for speed *)
   prolog = #'<\\?xml[^?]*\\?>'
   misc = #'(?:<!--[^-]*(?:-[^-]+)*-->|<\\?[^?]*\\?>)*'

   (* Structure *)
   element = node / empty-elem
   empty-elem = '<' name attrs '/>'
   node = '<' name attrs '>' content '</' name '>'

   (* Simplified content matching *)
   <content> = (element / text / special)*
   text = #'[^<]+'
   special = #'<[!?][^>]*>'

   (* Attributes *)
   <attrs> = attr*
   attr = name '=' value
   value = #'\"[^\"]*\"|\\'[^\\']*\\''

   (* Name *)
   name = #'[A-Za-z_:][\\w.:-]*'")

(def xml-ebnf
  "Fastest so far
  Evaluation count : 6 in 6 samples of 1 calls.
             Execution time mean : 183.743822 ms
    Execution time std-deviation : 15.819462 ms
   Execution time lower quantile : 167.386103 ms ( 2.5%)
   Execution time upper quantile : 206.460558 ms (97.5%)
                   Overhead used : 1.965770 ns"
  "document = prolog? element misc?
   
   (* Simplified regexes for speed *)
   prolog = #'<\\?xml[^?]*\\?>'
   misc = #'(?:<!--[^-]*(?:-[^-]+)*-->|<\\?[^?]*\\?>)+'
   
   (* Structure *)
   element = node / empty-elem 
   empty-elem = '<' name attrs '/>'
   node = '<' name attrs '>' content '</' name '>'
   
   (* Simplified content matching *)
   <content> = (element / text / special)*
   text = #'[^<]+'
   special = #'<[!?][^>]*>'
   
   (* Attributes *)
   <attrs> = attr*
   attr = name '=' value
   value = #'\"[^\"]*\"|\\'[^\\']*\\''
   
   (* Name *)
   name = #'[A-Za-z_:][\\w.:-]*'")

(def xml-ebnf
  "Evaluation count : 6 in 6 samples of 1 calls.
             Execution time mean : 162.710464 ms
    Execution time std-deviation : 18.470878 ms
   Execution time lower quantile : 138.239040 ms ( 2.5%)
   Execution time upper quantile : 183.225717 ms (97.5%)
                   Overhead used : 1.965770 ns
"
  (str
   "document = prolog? element W? misc?

   (* Simplified regexes for speed *)
   prolog = #'<\\?xml[^?]*\\?>' W
   misc = #'(?:<!--[^-]*(?:-[^-]+)*-->|<\\?[^?]*\\?>)+'

   (* Structure *)
   element = node | empty-elem
   empty-elem = '<' Name W? attrs? '/>'
   node = '<' Name W? attrs? '>' content '</' Name '>'

   (* Simplified content matching *)
   <content> = (element | text | special)*
   text = #'[^<]+'
   special = #'<[!?][^>]*>'
  
   (* Attributes *)
   <attrs> = attr+
   attr = Name '=' value W?
   value = #'\"[^\"]*\"|\\'[^\\']*\\''

   (* Name *)
   Name = #'" xml-name-pattern "'
   W = #'\\s+'"))

(require '[instaparse.core :as insta])

(comment (def p (insta/parser (slurp "bench/ebnf.ebnf")))
         (def xml (io/resource "actionmaps_slimmed.xml")))

(def p (insta/parser xml-ebnf :output-format :enlive :auto-whitespace :standard))
(def xml (io/resource "actionmaps2.xml"))
(def s (slurp xml))

(comment
  (xml/parse (io/input-stream xml))
  (hickory.core/parse (slurp (io/input-stream xml)))
  (txml/parse (slurp (io/input-stream xml)))
  (exml/parse (io/input-stream xml))
  (insta/parse p s)

  (take 5 (insta/parses p s :partial true))

  (crit/quick-bench (insta/parse p s)))

(defn- read-bytes [res]
  (with-open [in (io/input-stream res)
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn- info [label details]
  (println (format "\n==> %-30s | %s" label (str/join " | " details))))

;; --- E2E: exactly your forms, prints context for each ----------------------

(defn quick-bench-e2e! []
  (let [bytes (read-bytes xml)
        s-len (with-open [in (io/input-stream xml)] (count (slurp in)))]
    (println "Benchmarking with I/O in loop"
             (format "(%,d bytes file; %,d chars via slurp)" (alength bytes) s-len))

    (info "data.xml/parse" ["input: stream" "fresh stream each iter" "includes file I/O"])
    (crit/quick-bench (xml/parse (io/input-stream xml)))

    (info "hickory.core/parse" ["input: string (slurp stream)" "fresh slurp each iter" "includes file I/O"])
    (crit/quick-bench (h/parse (slurp (io/input-stream xml))))

    (info "tupelo.parse.xml/parse" ["input: string (slurp stream)" "fresh slurp each iter" "includes file I/O"])
    (crit/quick-bench (txml/parse (slurp (io/input-stream xml))))

    (info "net.cgrand.xml/parse" ["input: stream" "fresh stream each iter" "includes file I/O"])
    (crit/quick-bench (exml/parse (io/input-stream xml)))

    (info "instaparse/parse" ["input: string (slurp url)" "fresh slurp each iter" "includes resource I/O"])
    (crit/quick-bench (insta/parse p (slurp xml)))))

(defn- my-read-bytes [res]
  (with-open [in (io/input-stream res)
              out (ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn- my-info [label details]
  (println (format "\n==> %-30s | %s" label (str/join " | " details))))

(defn quick-bench-core! []
  (let [bytes (my-read-bytes xml)
        s     (with-open [in (io/input-stream xml)] (slurp in))
        s-len (count s)
        new-stream #(ByteArrayInputStream. bytes)]
    (println "Benchmarking with preloaded inputs"
             (format "(%,d bytes; %,d chars)" (alength bytes) s-len))

    (my-info "data.xml/parse" ["input: stream" "preloaded bytes" "no file I/O in loop"])
    (crit/quick-bench (xml/parse (new-stream)))

    (my-info "riveted.core/nav" ["input: stream" "preloaded bytes" "no file I/O in loop"])
    (crit/quick-bench (vtd/navigator s))

    (my-info "hickory.core/parse" ["input: string" "preloaded slurp" "no file I/O in loop"])
    (crit/quick-bench (h/parse s))

    (my-info "tupelo.parse.xml/parse" ["input: string" "preloaded slurp" "no file I/O in loop"])
    (crit/quick-bench (txml/parse s))

    (my-info "net.cgrand.xml/parse" ["input: stream" "preloaded bytes" "no file I/O in loop"])
    (crit/quick-bench (exml/parse (new-stream)))

    (my-info "instaparse/parse" ["input: string" "preloaded slurp" "no file I/O in loop"])
    (crit/quick-bench (insta/parse p s))))

(with-out-str (quick-bench-core!))
;; Benchmarking with preloaded inputs (72,480 bytes; 72,480 chars)

;; ==> data.xml/parse                 | input: stream | preloaded bytes | no file I/O in loop
;; Evaluation count : 17988 in 6 samples of 2998 calls.
;;              Execution time mean : 34.407070 µs
;;     Execution time std-deviation : 1.774565 µs
;;    Execution time lower quantile : 32.482984 µs ( 2.5%)
;;    Execution time upper quantile : 36.169653 µs (97.5%)
;;                    Overhead used : 1.965770 ns

;; ==> riveted.core/nav               | input: stream | preloaded bytes | no file I/O in loop
;; Evaluation count : 4134 in 6 samples of 689 calls.
;;              Execution time mean : 144.463424 µs
;;     Execution time std-deviation : 6.470947 µs
;;    Execution time lower quantile : 137.759713 µs ( 2.5%)
;;    Execution time upper quantile : 154.558652 µs (97.5%)
;;                    Overhead used : 1.965770 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 13.8889 % Variance is moderately inflated by outliers

;; ==> hickory.core/parse             | input: string | preloaded slurp | no file I/O in loop
;; Evaluation count : 594 in 6 samples of 99 calls.
;;              Execution time mean : 1.044326 ms
;;     Execution time std-deviation : 66.133235 µs
;;    Execution time lower quantile : 1.003844 ms ( 2.5%)
;;    Execution time upper quantile : 1.156814 ms (97.5%)
;;                    Overhead used : 1.965770 ns

;; Found 1 outliers in 6 samples (16.6667 %)
;; 	low-severe	 1 (16.6667 %)
;;  Variance from outliers : 14.8628 % Variance is moderately inflated by outliers

;; ==> tupelo.parse.xml/parse         | input: string | preloaded slurp | no file I/O in loop
;; Evaluation count : 30 in 6 samples of 5 calls.
;;              Execution time mean : 24.566869 ms
;;     Execution time std-deviation : 2.234351 ms
;;    Execution time lower quantile : 22.384155 ms ( 2.5%)
;;    Execution time upper quantile : 27.073164 ms (97.5%)
;;                    Overhead used : 1.965770 ns

;; ==> net.cgrand.xml/parse           | input: stream | preloaded bytes | no file I/O in loop
;; Evaluation count : 66 in 6 samples of 11 calls.
;;              Execution time mean : 14.353139 ms
;;     Execution time std-deviation : 2.083402 ms
;;    Execution time lower quantile : 11.560601 ms ( 2.5%)
;;    Execution time upper quantile : 16.693059 ms (97.5%)
;;                    Overhead used : 1.965770 ns

;; ==> instaparse/parse               | input: string | preloaded slurp | no file I/O in loop
;; Evaluation count : 6 in 6 samples of 1 calls.
;;              Execution time mean : 232.098738 ms
;;     Execution time std-deviation : 18.094521 ms
;;    Execution time lower quantile : 210.191754 ms ( 2.5%)
;;    Execution time upper quantile : 249.792890 ms (97.5%)
;;                    Overhead used : 1.965770 ns
