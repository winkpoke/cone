(ns cone.core
    (:require
      [reagent.core :as r]))


;; -------------------------
;; Model

(defonce cones-diameter '(0.5 1.0 1.5 2.0 2.5 3.0))

(defonce status #{:idle :ready :switching :on-position})

(def db (r/atom {:cones '(1 3 4)
                 :n 0 
                 :status :idle}))

;; -------------------------
;; Message

(defonce message #{:open-patient :start-treat :finish-treat 
                   :toggle-on :toggle-off})

(defn start-treat []
  (swap! db assoc  :status :ready))

(defn snd [msg]
  (condp = msg
    :start-treat (start-treat)
    )
  )

;; -------------------------
;; Views

(defn simple-component []
  [:div.ui.disabled {:style {:backgroundColor "#CCCCCC"}}
   [:p "I am a component!"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red "] "text."]])

(defn cone-control []
  [:nav.menu
   [:button "Open"]
   [:button {:on-click #(snd :start-treat)} "Treats"]
   [:button "Finish"]
   [:label (str (:status @db))]
   ])

(defn patient-info []
  [:div 
   [:h4 "Patient information:"]
   [:p "Name: John"]
   [:p "Sex: Male"]
   [:p "#Cones: 3"]]
  )

(defn greyout? [n]
  (let [status #{:idle :switching}] 
    (or (contains? status (:status @db))
        (not= n (:n @db))))
  )

(defn greyout []
  {:style {:backgroundColor "#CCCCCC"}}
  )

(defn toggle [label disable?]
 [:div.ui.toggle.checkbox.disabled
  [:input {:type "checkbox" :name "public" :disabled disable?}]
  [:label (or label "")]
  ]
)

(defn cone-status []
  [:div 
   [:h4 "Cone status:"]
   (let [cones (:cones @db)] 
     (for [i (range (count cones))
           :let [n (nth cones i)]]
     [:div (when (greyout? i) (greyout)) 
      [:h5 (str "#" (inc i) " Treatment")]
      [:p {:style {:color "green"}} 
       (str "Cone" "#" n ":   ϕ" (nth cones-diameter n) "mm")]
      [toggle "On/Off" (greyout? i)]
      [:p "-------------------------------"]
      ]
     ))
   ]
  )

(defn home-page []
  [:div [:h2 "Welcome to Cone Monitor"]
        [simple-component]
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
