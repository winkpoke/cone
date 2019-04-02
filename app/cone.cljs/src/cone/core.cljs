(ns cone.core
    (:require
      [reagent.core :as r]))


;; -------------------------
;; Model

(defonce cones-diameter '(0.5 1.0 1.5 2.0 2.5 3.0))

(defonce status #{:idle :rotating :on-position :treating})

(def db (r/atom {:cones '(1 3 4)
                 :pos nil
                 :status :idle}))

;; -------------------------
;; Message

(defonce message #{:open-patient :next-cone :start-treat :finish-treat})


;; -------------------------
;; Views

(defn simple-component []
  [:div
   [:p "I am a component!"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red "] "text."]])

(defn cone-control []
  [:nav.menu
   [:button "Open"]
   [:button "Treat"]
   [:button "Finish"]
   [:button "Next Cone"]])

(defn patient-info []
  [:div 
   [:h4 "Patient information:"]
   [:p "Name: John"]
   [:p "Sex: Male"]
   [:p "#Cones: 3"]]
  )

(defn cone-status []
  [:div
   [:h4 "Cone status:"]
   (let [cones (:cones @db)] 
     (for [i (range (count cones))
           :let [n (nth cones i)]]
     [:div 
      [:h5 (str "#" (inc i) " Treatment")]
      [:p {:style {:color "green"}} 
       (str "Cone" "#" n ":   ϕ" (nth cones-diameter n) "mm")]
      ]
     ))
   ]
  )

(defn home-page []
  [:div [:h2 "Welcome to Cone Monitor"]
        [patient-info]
        [cone-status]
        [cone-control]
   ])

;; -------------------------
;; Initialize app

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (mount-root))
