(ns laconic.utils.events
  (:require
    [clojure.string :as string]
    [re-frame.core :as rf]))

(def base-interceptors
  [(when ^boolean js/goog.DEBUG rf/debug)
   rf/trim-v])


(defn keyword-or-int [x]
  (let [parsed (js/parseInt x)]
    (if (int? parsed)
      parsed
      (keyword x))))

(defn vec-of-keys [x]
  (if (vector? x)
    x
    (mapv keyword-or-int
          (if (qualified-keyword? x)
            (into (string/split (namespace x) ".")
                  (string/split (name x) "."))
            (string/split (name x) ".")))))

(rf/reg-sub
  :query
  (fn [db [_ path]]
    (get-in db (vec-of-keys path))))

(rf/reg-event-db
  :set
  base-interceptors
  (fn [db [path val]]
    (assoc-in db (vec-of-keys path) val)))

(rf/reg-event-db
  :update
  base-interceptors
  (fn [db [path f & args]]
    (apply update-in db (vec-of-keys path) f args)))
