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
  base-interceptors
  :query
  (fn [db [path]]
    (get-in db (vec-of-keys path))))

(rf/reg-event-db
  base-interceptors
  :set
  (fn [db [path val]]
    (assoc-in db (vec-of-keys path) val)))

(rf/reg-event-db
  base-interceptors
  :update
  (fn [db [path f]]
    (update-in db (vec-of-keys path) f)))
