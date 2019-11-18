(ns laconic.utils.core
  (:require
    [clojure.string :as string]
    [dommy.core :as dommy :refer-macros [sel sel1]]
    [reagent.core :as r]
    [re-frame.core :as rf]))

; --------------------------------------------------------------
; Dependencies Management
; --------------------------------------------------------------

(defn unload-deps! [deps]
  (doseq [dep deps]
    (dommy/remove! (sel1 (keyword (str "#" (:id dep)))))))

(defn load-dep! [tag attrs loaded-count]
  (let [elt (reduce-kv dommy/set-attr!
                       (dommy/create-element tag)
                       attrs)]
    (dommy/append! (sel1 :body) elt)
    (set! (.-onload elt) #(swap! loaded-count inc))))

(defn load-deps! [deps loaded-count]
  (doseq [dep deps]
    (cond 
      (string/includes? (:type dep) "javascript")
      (load-dep! :script dep loaded-count)
      
      (string/includes? (:type dep) "css")
      (load-dep! :link dep loaded-count))))
      

(defn with-deps
  "Loads a supplied sequence of Javascript or CSS files and renders a 
  component during loading and another component as soon as every 
  dependency is loaded.
  Besides the required dependencies, the user must also provide the set of 
  already loaded deps.
  deps is a map of the attrs of the element to be loaded. (:id, :type, :src/:href)

   Arg map: {:deps dep-map*
             :loading component
             :loaded component}"
  [{:keys [deps loading loaded]}]
  (let [loaded-deps (rf/subscribe [:query [:page/loaded-deps]])
        depsset (set deps)
        loaded-count (r/atom 0)]
    (r/create-class
     {:component-did-mount
      (fn [_]
        (let [not-loaded (remove (or @loaded-deps #{}) deps)
              not-required (clojure.set/difference (or @loaded-deps #{}) 
                                                   depsset)]
          (if (seq not-loaded)
            (do
              (load-deps! not-loaded loaded-count)
              (apply println "Loaded:" (map :id not-loaded)))
            (reset! loaded-count (count deps)))
          (rf/dispatch [:set [:page/loaded-deps] depsset])
          (unload-deps! not-required)))
      :reagent-render
      (fn [{:keys [deps loading loaded]}]
        (cond (empty? deps) 
              loaded
              
              (= @loaded-count (count deps))
              loaded
              
              :else loading))})))
