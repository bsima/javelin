(ns javelin.protocols)

(defprotocol INotifyWatches
  "Clojure doesn't have ClojureScript's equivalent of cljs.core.IWatchable,
  instead it has IRef, which only specifies add-watch and remove-watch. Instead
  of re-implementing IWatchable, we just implement the one function we need.
  This should be removed if Clojure ever gets an IWatchable protocol."
  (notify-watches [this oldval newval] "Calls all watchers with this, oldval and newval."))

(defprotocol ICell
  "Protocol for types that implement the cell abstraction."
  (destroy-cell!* [this] "Destroys a cell.")
  (set-formula!*  [this f srcs] "Sets a formula to f and srcs")
  (cell?*     [c]   "Is this a cell?")
  (formula?*  [c]   "Is this a formula?")
  (lens?*     [c]   "Is this a lens?")
  (input?*    [c]   "Is this an input into a cell?")
  (set-cell!* [c x] "Set the state of cell c to x.")
  (formula*   [f]   "Create a formula. Also known as a `lift'.")
  (lens*      [c f] "Create a new lens on cell c with formula f.")
  (cell*      [x] [x meta] "Create a new cell with contents x, and optional metadata."))
