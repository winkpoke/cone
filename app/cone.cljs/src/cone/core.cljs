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
        (recur (inc n) (get cones (inc n)))))))

(defn patient-name [data]
  (get-in data [:patient-info :name]))

(defn patient-sex [data]
  (get-in data [:patient-info :sex]))

(defn toggle-cone [data n]
  (update-in data [:cones n :on?] not))

(defn toggle-cone! [db n]
  (swap! db toggle-cone n))

(defn set-cone-status [data n status]
  (assoc-in data [:cones n :status] status))

(defn set-cone-status! [db n status]
  (swap! db set-cone-status n status))

(defn set-current-cone! [db n]
  (println (str "current cone#" (current-cone-no @db)
                  " set cone#" n
                  " n cones: " (no-of-cones @db)
                  "\n" @db))
  (let [i (current-cone-no @db)]
    (when-not (== n i) 
      (letfn [(update-all [db i n] 
                (toggle-cone! db i)
                (set-cone-status! db i :idle)
                (when (< n (no-of-cones @db)) 
                  (do
                    (println "----------------")
                    (toggle-cone! db n))
                    (set-cone-status! db n :ready)))] 
        ;(swap! db update-all i n)
        (update-all db i n)
        (println @db)
        ))))


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

(defn toggle-on [n]
  (js/console.log "toggle-on cone#" n)
  )

(defn toggle-off [n]
  (js/console.log "toggle-off cone#" n)
  (set-current-cone! db (inc n))
  )

(defn finish-treat []
  (js/window.alert "Treatment finished")
  )

(defn open-patient-electron []
  (let [electron (js/require "electron")
        remote (.-remote electron)
        dialog (.-dialog remote)
        ]
    (.showOpenDialog dialog #(println %))
    )
  )
(defn open-patient-html []
  
  )

(defn open-patient []
  (try
    (open-patient-electron) 
    (catch :default e
      (js/window.alert "open patient")))
  )

(defn snd [msg & rst]
  (condp = msg
    :start-treat (start-treat)
    :toggle-on (apply toggle-on rst)
    :toggle-off (apply toggle-off rst)
    :finish-treat (finish-treat)
    :open-patient (open-patient)
    ))

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
  [:div.ui.disabled (greyout)
   [:p "I am a component!"]
   [:p.someclass
    "I have " [:strong "bold"]
    [:span {:style {:color "red"}} " and red "] "text."]])

(defn open-button []
  [:button {:on-click #(snd :open-patient)} "Open"]
;  [:div.ui
;   [:label.ui "Open"] 
;   [:input#file-picker {:type "file" }]]
  
  )

(defn start-treat-button []
  [:button {:on-click #(snd :start-treat)} "Treat"])

(defn finish-treat-button []
  [:button {:on-click #(snd :finish-treat)} "Finish"])

(defn cone-control []
  [:nav.menu
   [open-button]
   [start-treat-button]
   [finish-treat-button]
   [:label (str (:status @db))]])

(defn patient-info []
  [:div.ui.label 
   [:h4 "Patient information:"]
   [:div.label.ui (str "Name: " (patient-name @db))]
   [:br]
   [:span (str "Sex: " (patient-sex @db))]
   [:br]
   [:span (str "#Cones: " (no-of-cones @db))]
   [:br]
   ])

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
                         (if checked 
                           (snd :toggle-on id)
                           (snd :toggle-off id))))}]
  [:label (or label "")]])

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
          ])))])

(defn tool-bar []
  [:div.sixteen.wide.column
   [:div.ui.icon.menu
    [:a.item {:on-click #(snd :open-patient)}
     [:i.folder.open.icon]
     ]
    ]
   ]
  )

(defn home-page []
  [:div.ui.grid 
   [tool-bar]
   [:div.sixteen.wide.column
    [:div.huge.ui.label "Welcome to Cone Monitor"]]
;  [simple-component]
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
