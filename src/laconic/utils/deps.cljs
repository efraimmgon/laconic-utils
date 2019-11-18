(ns laconic.utils.deps
  (:require
    [clojure.set :refer [difference intersection]]
    [clojure.string :as string]
    [dommy.core :as dommy :refer-macros [sel sel1]]
    [laconic.utils.sort :refer [topological-sort]]
    [reagent.core :as r]
    [re-frame.core :as rf]))

; Script loaders defer script execution  until the file and any 
; required dependencies needed by the module are loaded. Most caches 
; the modules as well, so i'ts loaded only once, no matter how many times 
; it's requested.

; How do I define a dependency?
; A dependency must have an identification and a source file.
; This definition can be internal, through a function call, or I can
; rely on re-frame to do it.

(defonce config 
  (atom {:deps/deps {}
         :deps/dependency-graph []
         :deps/loaded #{}}))

(defn add-deps! 
  "Define a dependency for loading later. Takes a set of maps 
  of :dep/id, :dep/src (if script), :dep/href (if css), :dep/deps."
  [deps]
  (->> (reduce (fn [acc dep]
                 (assoc acc (:dep/id dep) dep))
          {} deps)
       (swap! config update :deps/deps
          merge)))

(defn unload-deps! 
  "Takes a coll of deps ids and removes those elements of the window."
  [deps]
  (doseq [id deps]
    (dommy/remove! (sel1 (keyword (str "#" id))))))

; - The order of the values is not important, because if any of the deps
; rely on ther deps, this was already declared at add-deps!.
; - Each dependency may have a dependency, and so forth. Therefore it is 
; necessary to firstly build a tree of the correct order to load them.
; - Keep in mind that a dependency may already be loaded.
; - TODO: make it assynchronous.
; - NOTE: What do I do about css classes? Often stylesheets will interfere with
; one another. The simpler solution seems to load only what is 
; stricly required for each page to function.
; The issue with is that it might be inefficient.
(defn with-deps 
  "Assynchronously load dependencies to the window. Takes a set of deps.
  Its values must be the desired ids provided to `add-deps!`.
  Arg map: {:deps dep-map*
             :loading component
             :loaded component}"
  [{:keys [deps loading loaded]}]
  (when (seq deps)
    (unload-deps! (difference (:deps/loaded @config) deps))
    (swap! config update :deps/loaded intersection deps))
  (r/create-class
    {:component-did-mount
     (fn [_]
      (doseq [id (->> deps
                      (select-keys (:deps/deps @config))
                      (reduce-kv #(assoc % %2 (:dep/deps %3)) {})
                      topological-sort)]
        (assert (-> @config :deps/deps id) 
                (str "Must declare " id " dependency first with `add-deps!`."))
        (when-not ((:deps/loaded @config) id)
          (let [dep (-> @config :deps/deps id)
                [tag attrs] (if (:dep/src dep)
                              [:script, {:id (:dep/id dep) :src (:dep/src dep)}]
                              [:link,   {:id (:dep/id dep)  :href (:dep/href dep)}])
                elt (reduce-kv dommy/set-attr!
                               (dommy/create-element tag)
                               attrs)]
            (dommy/append! (sel1 :body) elt)
            (set! (.-onload elt) #(do (swap! config update :deps/loaded 
                                             conj id)
                                      (println "Loaded:" id)))))))
     :reagent-render
     ;; NOTE: removed the keys from the fn, not sure this will alter
     ;; the repainting of the component.
     (fn [_]
       (cond (empty? deps) loaded
             (= deps (:deps/loaded @config)) loaded
             :else loading))}))