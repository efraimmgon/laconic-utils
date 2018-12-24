(ns laconic.utils.dom
  (:require
   [dommy.core :as dommy :refer-macros [sel sel1]]))

; --------------------------------------------------------------
; DOM Manipulation
; --------------------------------------------------------------

(defn- set-attrs! [elt opts]
  (reduce (fn [elt [attr val]]
            (dommy/set-attr! elt attr val))
          elt opts))

(defn add-style!
  "Append a link tag to body. `opts` must include at least the
   `:href` attribute"
  [opts]
  (let [default-attrs {:rel "stylesheet" :type "text/css"}
        elt (set-attrs! (dommy/create-element :link)
                        (into default-attrs opts))]
    (dommy/append! (sel1 :body) elt)))

(defn add-script!
  "Append a script tag to body. `opts` must include at least the
   `:src` attribute."
  [opts]
  (let [default-attrs {:type "text/javascript"}
        elt (set-attrs! (dommy/create-element :script)
                        (merge default-attrs opts))]
    (dommy/append! (sel1 :body) elt)))

(defn remove-elt!
  "Remove a tag from the document by the id attribute."
  [id]
  (dommy/remove! (sel1 id)))
