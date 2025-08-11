(ns xml.roundtrip
  (:require [riveted.core :as vtd])
  (:import  (com.ximpleware VTDNav XMLModifier)))

(set! *warn-on-reflection* true)

;; --- low-level helpers ------------------------------------------------------

(defn ^VTDNav vnav [nav] (.-nav ^riveted.core.Navigator nav))

(defn ->utf8 ^bytes [s] (.getBytes (str s) "UTF-8"))

(defn charw
  "Byte width of one character for current doc encoding (1 for UTF-8, 2 for UTF-16)."
  ^long [^VTDNav n]
  (if (>= (.getEncoding n) VTDNav/FORMAT_UTF_16BE) 2 1))

;; getOffsetBeforeTail is protected in VTD, reflect it
(let [m (doto (.getDeclaredMethod VTDNav "getOffsetBeforeTail" (into-array Class []))
          (.setAccessible true))]
  (defn- before-tail-offset ^long [^VTDNav n]
    (long (.invoke m n (object-array [])))))

;; --- patch state ------------------------------------------------------------

(defrecord Patch [^XMLModifier xm ^VTDNav n inserts ns-aware?])
;; inserts: {byte-offset -> [byte-array ...]} (sorted-map for stable order)

(defn start-patch
  "Bind a patcher to any riveted navigator `node`.
   Optional ns-aware? should match how you created the navigator."
  ([node] (start-patch node false))
  ([node ns-aware?]
   (let [n (vnav node)]
     (->Patch (XMLModifier. n) n (atom (sorted-map)) ns-aware?))))

(defn- add-insert! [^Patch p ^long off ^bytes bs]
  (swap! (:inserts p) update off (fnil conj []) bs)
  p)

;; --- surgical edits ---------------------------------------------------------

(defn set-attr!
  "Lossless UPDATE of an existing attribute value (preserves quotes/spacing/order).
   Throws if attr is missing. Use `upsert-attr!` to create."
  [^Patch p node k v]
  (let [tok (.getAttrVal ^VTDNav (vnav node) (name k))]
    (when (neg? tok)
      (throw (ex-info "Attribute not found" {:attr k})))
    (.updateToken ^XMLModifier (:xm p) tok (str v))
    p))

(defn upsert-attr!
  "Lossless upsert. If attr exists -> update its value.
   If missing -> insert ` key=\"val\"` into the start tag *without* reflow.
   Works for both normal and empty elements (<x/>)."
  [^Patch p node k v]
  (let [^VTDNav n (vnav node)
        kname    (name k)
        tok      (.getAttrVal n kname)]
    (if (neg? tok)
      (let [w     (charw n)
            ;; For normal elements: before '>' (offsetAfterHead points after it)
            head  (.getOffsetAfterHead n)
            pos   (if (neg? head)
                    ;; Empty element: compute before '/>' inside the start tag:
                    ;; element fragment covers the whole `<tag .../>`
                    (let [r (.getElementFragment n)
                          off (bit-and r 0xFFFFFFFF)
                          len (unsigned-bit-shift-right r 32)]
                      ;; insert at: end minus 2*charw (just before '/')
                      (+ off (- len (* 2 w))))
                    ;; Non-empty: just before '>'
                    (- (* head w) w))]
        (add-insert! p pos (->utf8 (str " " kname "=\"" v "\""))))
      (do (.updateToken ^XMLModifier (:xm p) tok (str v)) p))))

(defn insert-after-head!
  "Insert raw XML as the first content inside the element (`<tag ...>HERE</tag>`).
   Fails on empty elements (<x/>)."
  [^Patch p node xml]
  (let [^VTDNav n (vnav node)
        head (.getOffsetAfterHead n)]
    (when (neg? head)
      (throw (ex-info "Cannot insert into empty element (<x/>). Use elem-level insert or add content then promote." {:node node})))
    (add-insert! p (* head (charw n)) (->utf8 xml))))

(defn insert-before-tail!
  "Insert raw XML right before the closing tag (`<tag ...>...HERE</tag>`).
   If element has no content, falls back to insert-after-head!."
  [^Patch p node xml]
  (let [^VTDNav n (vnav node)
        off (before-tail-offset n)]
    (if (neg? off)
      (insert-after-head! p node xml)
      (add-insert! p (* off (charw n)) (->utf8 xml)))))

(defn insert-before-elem!
  "Insert raw XML as a sibling before this element."
  [^Patch p node xml]
  (let [^VTDNav n (vnav node)
        r (.getElementFragment n)
        off (bit-and r 0xFFFFFFFF)]
    (add-insert! p off (->utf8 xml))))

(defn insert-after-elem!
  "Insert raw XML as a sibling after this element."
  [^Patch p node xml]
  (let [^VTDNav n (vnav node)
        r (.getElementFragment n)
        off (bit-and r 0xFFFFFFFF)
        len (unsigned-bit-shift-right r 32)]
    (add-insert! p (+ off len) (->utf8 xml))))

(defn replace-element!
  "Replace the ENTIRE element (start tag + content + end tag) with raw XML."
  [^Patch p node xml]
  (let [^VTDNav n (vnav node)
        r (.getElementFragment n)
        off (bit-and r 0xFFFFFFFF)
        len (unsigned-bit-shift-right r 32)]
    (.removeContent ^XMLModifier (:xm p) off len)
    (.insertBytesAt ^XMLModifier (:xm p) off (->utf8 xml))
    p))

(defn replace-content!
  "Replace ONLY the content (between start/end tags) with raw XML."
  [^Patch p node xml]
  (let [^VTDNav n (vnav node)
        r (.getContentFragment n)
        off (bit-and r 0xFFFFFFFF)
        len (unsigned-bit-shift-right r 32)]
    (.removeContent ^XMLModifier (:xm p) off len)
    (.insertBytesAt ^XMLModifier (:xm p) off (->utf8 xml))
    p))

;; --- commit -----------------------------------------------------------------

(defn commit!
  "Apply all buffered inserts and return {:bytes bs :doc (vtd/navigator bs ns-aware?)}.
   Round-trip guarantee: everything outside edited spans is byte-identical."
  [^Patch p]
  (let [^XMLModifier xm (:xm p)]
    ;; Flush buffered inserts in ascending offsets
    (doseq [[off chunks] @(:inserts p)]
      (let [buf (byte-array (reduce + (map alength chunks)))]
        (loop [pos 0, cs chunks]
          (if (seq cs)
            (let [^bytes c (first cs), n (alength c)]
              (System/arraycopy c 0 buf pos n)
              (recur (+ pos n) (rest cs)))
            (.insertBytesAt xm off buf)))))
    (with-open [baos (java.io.ByteArrayOutputStream.)]
      (.output xm baos)
      (let [bs (.toByteArray baos)]
        {:bytes bs
         :doc   (vtd/navigator bs (:ns-aware? p))}))))

(require '[riveted.core :as vtd]
         '[xml.roundtrip :as rt])

(def bs  (slurp "resources/actionmaps_slimmed.xml" :encoding "UTF-8" :byte-array true))
(def nav (vtd/navigator bs))
(def am  (first (vtd/select nav :actionmap)))

(-> (rt/start-patch am)
    (rt/upsert-attr! am :name "player-42")     ; update-or-insert attr (works on <x/> too)
    (rt/insert-after-head! am "<note>hi</note>")
    (rt/insert-before-tail! am "<tail/>")
    (rt/commit!)
    ((fn [{:keys [bytes doc]}]
       (spit "out.xml" (String. bytes "UTF-8"))
       doc)))
