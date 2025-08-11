(ns xml.roundtrip
  (:require [riveted.core :as vtd])
  (:import (com.ximpleware VTDNav XMLModifier)))

(set! *warn-on-reflection* true)

(defn ^VTDNav vnav [nav] (.-nav ^riveted.core.Navigator nav))

(defrecord Patch [^XMLModifier xm ^VTDNav n inserts ns-aware?])

(defn start-patch
  "Bind a patcher to any riveted navigator `node`.
   Optional ns-aware? should match how you created the original navigator."
  ([node] (start-patch node false))
  ([node ns-aware?]
   (->Patch (XMLModifier. (vnav node)) (vnav node) (atom (sorted-map)) ns-aware?)))

(defn set-attr!
  "Lossless update of an existing attribute value (preserves quotes/spacing/order)."
  [^Patch p node k v]
  (let [tok (.getAttrVal ^VTDNav (vnav node) (name k))]
    (when (neg? tok)
      (throw (ex-info "Attribute not found" {:attr k})))
    (.updateToken ^XMLModifier (:xm p) tok (str v))
    p))

(defn insert-after-head!
  "Insert raw XML right after the start tag `<tag ...>`."
  [^Patch p node xml]
  (let [off (.getOffsetAfterHead ^VTDNav (vnav node))]
    (when (neg? off)
      (throw (ex-info "Cannot insert into empty element (<x/>). Use element-level insert/promote." {:node node})))
    (swap! (:inserts p) update off (fnil conj []) (.getBytes (str xml)))
    p))

;; Reflective helper for 'before tail' since it's protected in VTD-XML
(let [m (doto (.getDeclaredMethod VTDNav "getOffsetBeforeTail" (into-array Class []))
          (.setAccessible true))]
  (defn- before-tail-offset ^long [^VTDNav n]
    (long (.invoke m n (object-array [])))))

(defn insert-before-tail!
  "Insert raw XML right before the closing tag `</tag>` (or as first content if empty)."
  [^Patch p node xml]
  (let [n (vnav node)
        off (before-tail-offset n)]
    (if (neg? off)
      (insert-after-head! p node xml)
      (swap! (:inserts p) update off (fnil conj []) (.getBytes (str xml)))))
  p)

(defn commit!
  "Apply all buffered edits and return {:bytes bs :doc (vtd/navigator bs ns-aware?)}."
  [^Patch p]
  (let [^XMLModifier xm (:xm p)]
    ;; Flush buffered inserts in ascending byte offset order
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

(comment
  (require '[riveted.core :as vtd]
           '[xml.roundtrip :as rt])

  (def bs  (slurp "resources/actionmaps_slimmed.xml" :encoding "UTF-8" :byte-array true))
  (def nav (vtd/navigator bs))
  (def am  (first (vtd/select nav :actionmap)))

  (-> (rt/start-patch am)
      (rt/set-attr! am :name "player-42")
      (rt/insert-after-head! am "<note>hi</note>")
      (rt/insert-before-tail! am "<tail/>")
      (rt/commit!)
      ((fn [{:keys [bytes doc]}]
         (spit "out.xml" (String. bytes "UTF-8"))
         doc))))
