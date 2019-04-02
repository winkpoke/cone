(ns cone.core
    (:require
      [reagent.core :as r]))


;; -------------------------
;; Model

(defonce cones-diameter '(0.5 1.0 1.5 2.0 2.5 3.0))

(defonce status #{:idle :ready :switching :on-position})

(def raw-data {:patient-info {:name "John" :sex "Male"}
               :cones '(1 3 4)})


(defn init-model [data]
  (letfn [(no-of-cones [] (count (:cones data)))
          (nth-cone [n] (nth (:cones data) n))] 
    {:patient-info (:patient-info data)
     :cones (vec (for [i (range (no-of-cones))]
                   {:n i :cone-pos (nth-cone i) :on? false :status :idle}))}))

;; (def db (r/atom {:cones '(1 3 4)
;;                  :n 0 
;;                  :status :idle}))

(def db (r/atom (init-model raw-data)))

(defn no-of-cones [data]
  (count (:cones data)))

(defn nth-cone [data n]
  (get (:cones data) n))

(defn nth-cone-pos [data n]
  (:cone-pos (nth-cone data n)))

(defn nth-cone-on? [data n]
  (:on? (nth-cone data n)))

(defn nth-cone-status [data n]
  (:status (nth-cone data n)))

(defn current-cone-no [data]
  (let [cones (:cones data)]
    (loop [n 0 cone (get cones 0)]
      (if (:on? cone)
        n
        (recur (inc n) (get cones (inc n)))
        )
      )
    )
  )
(defn toggle-cone! [db n]
  (swap! db update-in [:cones n :on?] not))

(defn set-cone-status! [db n status]
  (swap! db assoc-in [:cones n :status] status)
  )
(defn set-current-cone! [db n]
  (let [i (current-cone-no @db)]
    (when-not (== n i) 
      (letfn [(update-all [db i n] 
                (toggle-cone! db i)
                (set-cone-status! db i :idle)
                (toggle-cone! db n)
                (set-cone-status! db n :ready))] 
        (swap! db update-all i n)))))


;(print @db)
;(println (:cones @db))
;(println (no-of-cones @db))

;; -------------------------
;; Message

(defonce message #{:open-patient :start-treat :finish-treat 
                   :toggle-cone :toggle-on :toggle-off})

(defn start-treat []
  (toggle-cone! db 0)
  (set-cone-status! db 0 :ready)
  (swap! db assoc :current 0)
  (println @db)
  )

(defn toggle-cone []
  ()
  )

(defn snd [msg & rest]
  (condp = msg
    :start-treat (start-treat)
    :toggle-cone (toggle-cone)
    )
  )

;; -------------------------
;; Views

(defn greyout? [n]
  (let [status #{:idle :switching}] 
    (or (contains? status (:status @db))
        (not= n (:n @db))))
  )

(defn greyout []
  {:style {:backgroundColor "#CCCCCC"}}
  )

(defn simple-component []
  [:div.ui.disabled (greyout) ;{:style {:backgroundColor "#CCCCCC"}}
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
   [:p (str "Name: " (get-in @db [:patient-info :name]))]
   [:p "Sex: Male"]
   [:p (str "#Cones: " (no-of-cones @db))]]
  )

(defn toggle [label id enable?]
 [:div.ui.toggle.checkbox.disabled
  [:input {:id id
           :type "checkbox" 
           :name "public" 
           :disabled (not enable?)
           :on-click (fn [this]
                       (js/console.log (-> this .-target .-checked))
                       (let [id (js/parseInt (-> this .-target .-id))
                             checked (-> this .-target .-checked)]
                         (str 12)
                         )
                       (snd :toggle-cone))
           }]
  [:label (or label "")]
  ]
)

(defn cone-status []
  [:div 
   [:h4 "Cone status:"]
   (let [cones (:cones @db)] 
     (doall 
       (for [i (range (no-of-cones @db))
           :let [n (nth-cone-pos @db i)]]
         [:div (merge {:key i}  (when-not (nth-cone-on? @db i) (greyout))) 
          [:h5 (str "#" (inc i) " Treatment")]
          [:p {:style {:color "green"}} 
           (str "Cone" "#" n ":   ϕ" (nth cones-diameter n) "mm")]
          [toggle "On/Off" (str i) (nth-cone-on? @db i)]
          [:p (str (nth-cone-status @db i))]
          [:p "-------------------------------"]
          ]
         )))
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
