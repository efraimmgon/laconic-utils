(ns laconic.utils.core
  (:require
    [laconic.utils.dom :as dom]
    [re-frame.core :as rf]))

; --------------------------------------------------------------
; Dependencies Management
; --------------------------------------------------------------

#_
{:pages {:loaded-deps ...
         :main/deps deps-coll*
         :admin/deps deps-coll*}}

(defn load-deps! [deps]
  (doseq [dep deps]
    (case (:type dep)
      :js  (dom/add-script! dep)
      :css (dom/add-style! dep))))

(defn unload-deps! [deps]
  (doseq [dep deps]
    (dom/remove-elt! (keyword (str "#" (:id dep))))))

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
        done? (atom false)]
    (r/create-class
     {:component-did-mount
      (fn [_]
        (let [not-loaded (remove @loaded-deps deps)
              not-required (clojure.set/difference @loaded-deps depsset)]
          (load-deps! not-loaded)
          (unload-deps! not-required)
          (rf/dispatch [:set [:page/loaded-deps] depsset])
          (apply println "Loaded:" (map :id not-loaded))
          (reset! done? true)))
      :reagent-render
      (fn [{:keys [deps loading loaded]}]
        (if (empty? deps)
          loaded
          (if @done? loaded loading)))})))
