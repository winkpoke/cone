(ns cone.core
  (:require
    [reagent.core :as r]
    [taoensso.timbre :as timbre
      :refer-macros [log  trace  debug  info  warn  error  fatal  report
                     logf tracef debugf infof warnf errorf fatalf reportf
                     spy get-env]]))

;; -------------------------
;; Model

(defonce cones-diameter '(0.5 1.0 1.5 2.0 2.5 3.0))

(defonce status #{:idle :ready :switching :on-position})




(def raw-data {:patient-info {:institution "ZS Hospital"
                              :name "Zhang San" 
                              :sex "M"
                              :id "0001101"
                              :age 65
                              :physician "Wang"
                              :study-description "ABDOMEN"
                              :treatment-unit "TrueBeam1"
                              :cone-Sequence "Cone#1/Cone#3/Cone#4"
                              }
               :cones '(1 3 4)})

(defn init-model [data]
  (info "init-model [data] with data: ", data)
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
  (let [cones (:cones data)
        cones-number (no-of-cones data)]
    (loop [n 0 cone (get cones 0)]
      (if (or (:on? cone) (>= n cones-number))
        n
        (recur (inc n) (get cones (inc n)))))))

(defn cone-treated? [data i]
  (let [n (no-of-cones data)
        current (current-cone-no data)] 
    (if (>= n current) false true))
  )

(defn patient-name [data]
  (get-in data [:patient-info :name]))

(defn patient-sex [data]
  (get-in data [:patient-info :sex]))

(defn patient-id [data]
  (get-in data [:patient-info :id]))

(defn toggle-cone [data n]
  (update-in data [:cones n :on?] not))

(defn toggle-cone! [db n]
  (swap! db toggle-cone n))

(defn set-cone-status [data n status]
  (assoc-in data [:cones n :status] status))

(defn set-cone-status! [db n status]
  (swap! db set-cone-status n status))

(defn set-current-cone! [db n]
  (info (str "current cone#" (current-cone-no @db)
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
                    (toggle-cone! db n))
                    (set-cone-status! db n :ready)))] 
        (update-all db i n)
        ))))

;; -------------------------
;; Message

(defonce message #{:open-patient :start-treat :finish-treat 
                   :toggle-on :toggle-off})

(defn start-treat []
  (js/window.alert "start-treat")
  (info "start-treat []")
  (toggle-cone! db 0)
  (set-cone-status! db 0 :ready)
  (swap! db assoc :current 0))

(defn toggle-on [n]
  (info "toggle-on cone#" n))

(defn toggle-off [n]
  (info "toggle-off cone#" n)
  (set-current-cone! db (inc n)))

(defn finish-treat []
  (info "finish-treat patient id: " (patient-id @db))
  (js/window.alert "Treatment finished"))

(defn open-patient-electron []
  (let [electron (js/require "electron")
        remote (.-remote electron)
        dialog (.-dialog remote)]
    (.showOpenDialog dialog #(println %))))

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
  [:button.ui.button {:on-click #(snd :start-treat)} "Treat"])

(defn finish-treat-button []
  [:button.ui.button {:on-click #(snd :finish-treat)} "Finish"])

(defn cone-control []
  [:nav.menu
   ;[open-button]
   [:br]
   [start-treat-button]
   [finish-treat-button]
   ])

(defn patient-info []
  [:div
   [:h4.ui.large.label "Patient information:"]
   [:div.ui.list
    [:div.item
     [:i.angle.right.icon]
     [:div.content "Institution: Zhong-Shan Hospital"]]
    [:div.item
     [:i.angle.right.icon]
     [:div.content (str "Name: " (patient-name @db))]]
    [:div.item
     [:i.angle.right.icon]
     [:div.content "Patient ID: " (patient-id @db)]]
    [:div.item
     [:i.angle.right.icon]
     [:div.content "Patient Sex: M"]]
    [:div.item
     [:i.angle.right.icon]
     [:div.content "Patient Age: 65"]]
    [:div.item
     [:i.angle.right.icon]
     [:div.content "Physician: Wang"]]
    [:div.item
     [:i.angle.right.icon]
     [:div.content "Study Description: ABDOMEN"]]
    [:div.item
     [:i.angle.right.icon]
     [:div.content "Treatment Unit: TrueBeam1"]]
    [:div.item
     [:i.angle.right.icon]
     [:div.content "Cone Sequence: Cone#1/Cone#3/Cone#4"]]]])

(defn toggle [label id enable?]
 [:div.ui.toggle.checkbox.disabled 
  [:input {:style {:backgroundColor "red"}
           :id id
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
   [:div.ui.ordered.steps
    (let [cones (:cones @db)] 
     (doall 
       (for [i (range (no-of-cones @db))
           :let [n (nth-cone-pos @db i)]]
         [:div (merge {:key i :class (if (cone-treated? @db i) 
                                       "completed step"
                                       "active step")}  
                      (when-not (nth-cone-on? @db i) (greyout))) 
          [:div.content
           [:div.title (str "Cone" "#" n) ]
            [:div.description 
             (str "Diameter ϕ" (nth cones-diameter n) "mm")]
            [:div.description 
             (str "Status: " (name (nth-cone-status @db i)))]
            [:div.description [:br]]
            [toggle "On/Off" (str i) (nth-cone-on? @db i)]]])))]])

(defn tool-bar []
  [:div.sixteen.wide.column
   [:div.ui.icon.menu
    [:a.item {:on-click #(snd :open-patient)}
     [:i.folder.open.icon]]]])

(defn home-page []
  [:div.ui.grid 
   [tool-bar]
   [:div.sixteen.wide.column
    [:div.huge.ui.label "Welcome to Cone Monitor"]]
   [:div.three.wide.column
     [patient-info]]
   [:div.twelve.wide.column
    [cone-status]
    [cone-control]]])

;; -------------------------
;; Initialize app

(defn middleware-time-stamp [data]
  (letfn [(time-stamp [] (.toISOString (js/Date.)))]
    (assoc data :vargs (cons (time-stamp) (:vargs data)))))

(defn mount-root []
  (r/render [home-page] (.getElementById js/document "app")))

(defn init! []
  (timbre/merge-config! {:middleware [middleware-time-stamp]})
  (mount-root))
