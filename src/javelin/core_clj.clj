;; Copyright (c) Alan Dipert and Micha Niskin. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns javelin.core-clj
  (:refer-clojure :exclude [accessor])
  (:require
    [riddley.compiler           :refer [locals]]
    [riddley.walk               :refer [walk-exprs]]
    [clojure.data.priority-map  :refer [priority-map]]
    [javelin.protocols :refer [ICell]]))

(declare cell? cell input? lens? notify-watches)

(def ^:private ^:dynamic *tx* nil)
(def ^:private last-rank (atom 0))

(defn bf-seq [branch? children root]
  (let [walk (fn walk [queue]
               (when-let [node (peek queue)]
                 (lazy-seq
                  (cons node (walk (into (pop queue)
                                         (when (branch? node)
                                           (children node))))))))]
    (walk (conj clojure.lang.PersistentQueue/EMPTY root))))

(defn- propagate* [pri-map]
  (when-let [next (first (peek pri-map))]
    (let [popq  (pop pri-map)
          old   (.prev next)
          new   (if-let [f (.thunk next)] (f) (.state next))
          diff? (not= new old)]
      (when diff? (set! (.prev next) new) (notify-watches next old new))
      (recur (if-not diff? popq (reduce #(assoc %1 %2 (.rank %2)) popq (.sinks next)))))))

(defn  deref*     [x]   (if (cell? x) @x x))
(defn- next-rank  [ ]   (swap! last-rank inc))
(defn- cell->pm   [c]   (priority-map c (.rank c)))
(defn- add-sync!  [c]   (swap! *tx* assoc c (.rank c)))
(defn- safe-nth   [c i] (try (nth c i) (catch Exception _)))
(defn- propagate! [c]   (if *tx* (doto c add-sync!) (doto c (-> cell->pm propagate*))))


;; Clojure doesn't have ClojureScript's equivalent of cljs.core.IWatchable,
;;  instead it has IRef, which only specifies add-watch and remove-watch. Instead
;;  of re-implementing IWatchable, we just implement the one function we need.
;;  This should be removed if Clojure ever gets an IWatchable protocol.
(defprotocol INotifyWatches
  (notify-watches [this oldval newval] "Calls all watchers with this, oldval and newval."))

;; Clojure deftypes have immutable fields by default, whereas the opposite is
;; true for ClojureScript. As such, we have to manually implement the setter for
;; the cell's sources and watches. The cleanest way to do this I've found is via
;; a protocol.
(defprotocol IMutableSources
  (sources [this] [this newval] "With one argument, returns the formula cell's
  sources. With two arguments, sets the sources to newval and returns newval."))

(defprotocol IMutableWatches
  (watches [this] [this k] [this k f]))

(deftype Cell [metadata state rank prev sinks thunk update
               ^:volatile-mutable sources
               ^:volatile-mutable watches]
  Object
  (toString [this] (pr-str "#<Cell: " (.state this) ">"))

  IMutableSources
  (sources [this] sources)
  (sources [this newval] (set! sources newval))

  clojure.lang.IMeta
  (meta [this] metadata)

  clojure.lang.IObj
  (withMeta [this m] (Cell. metadata state rank prev sources sinks thunk watches update))

  clojure.lang.IDeref
  (deref [this] (.state this))

  clojure.lang.IRef
  (addWatch      [this k f] (set! watches (assoc watches k f)))
  (removeWatch   [this k]   (set! watches (dissoc watches k)))

  INotifyWatches
  (notify-watches [this o n] (doseq [[key f] watches] (f key this o n)))

  clojure.lang.IAtom
  (reset [this x]
    (cond (lens? this)  ((.update this) x)
          (input? this) (do (set! (.state this) x) (propagate! this))
          :else         (throw (Error. "can't swap! or reset! formula cells")))
    (.state this))
  (swap [this f]        (reset! this (f (.state this))))
  (swap [this f a]      (reset! this (f (.state this) a)))
  (swap [this f a b]    (reset! this (f (.state this) a b)))
  (swap [this f a b xs] (reset! this (apply f (.state this) a b xs)))


  )

(defn destroy-cell! [^Cell this]
  (let [srcs (.sources this)]
    (.sources this)
    (doseq [src (filter cell? srcs)]
      (.sinks src (disj (.sinks src) this)))))

(defn set-formula! [this & [f srcs]]
  (destroy-cell! this)
  (.sources this (if f (conj (vec srcs) f) (vec srcs)))
  (doseq [source (filter cell? (.sources this))]
    (.sinks source (conj (.sinks source) this))
    (if (> (.rank source) (.rank this))
      (doseq [dep (bf-seq identity #(.sinks %) source)]
        (.rank dep (next-rank)))))
  (let [compute #(apply (deref (peek %)) (map deref (pop %)))
        thunk  #(reset! this (compute (.sources this)))]
    (if f
      (do (println this) (remove-watch this ::cell))
      (do (println this) (add-watch this ::cell (fn [_ c _ _] (propagate! c)))))
    (.thunk this (if f thunk #(deref this)))
    (doto this propagate!)))

(defn cell?      [c]      (when (= (type c) Cell) c))
(defn formula?   [c]      (when (and (cell? c) (.thunk c)) c))
(defn lens?      [c]      (when (and (cell? c) (.update c)) c))
(defn input?     [c]      (when (and (cell? c) (not (formula? c))) c))
(defn set-cell!  [c x]    (set! (.state c) x) (set-formula! c nil nil))
(defn formula    [f]      (fn [& sources] (set-formula! (cell ::none) f sources)))
(defn lens       [c f]    (let [c ((formula identity) c)] (set! (.update c) f) c))
(defn cell
  ([x]      (set-formula! (->Cell nil x (next-rank) x [] #{} nil {} nil)))
  ([x meta] (set-formula! (->Cell meta x (next-rank) x [] #{} nil {} nil))))

(defmethod print-method Cell [o ^java.io.Writer w]
  (.write w (str "#<Cell: " (pr-str (.state o) ">"))))

(def specials (into #{} (keys (. clojure.lang.Compiler specials))))

(defmacro cell= [expr]
  (let [hoist   (atom [])
        local   #(symbol (name %))
        core?   #(= "clojure.core" (namespace %))
        skip?   #(or
                   (not (contains? &env %))
                   (contains? specials %)
                   (core? %))
        walk!   #(do (if-not (skip? %)
                       (do (swap! hoist conj %) (local %))
                       %))
        walked  (walk-exprs symbol? walk! expr)
        hoisted (distinct @hoist)]
    `((formula (fn ~(mapv local hoisted) ~walked)) ~@hoisted)))

(defmacro defc  [name val]  `(def ~name (cell ~val)))
(defmacro defc= [name expr] `(def ~name (cell= ~expr)))
