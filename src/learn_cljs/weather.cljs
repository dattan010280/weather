(ns ^:figwheel-hooks learn-cljs.weather
  (:require
    [ajax.core :as ajax]
    [goog.dom :as gdom]
    [reagent.dom :as rdom]
    [reagent.core :as r]
    [clojure.string :as st]
    [learn-cljs.sortutils :as su]))

(defonce app-state (r/atom {:title "Forecasted Temperature"
                            :refresh-time "3"
                            :sort {:sorttype "country" :sortdirection "ascending"}
                            :data nil
                            :header ["Country" "City" "City Type"
                                     "Current Temperature" "Next 12 hours"
                                     "Next 24 hours" "Next 3 days"
                                     "Next 7 days"]}))

; FIXME: This should put in settings.edn file in resource
(def api-key "7cc0c5e314a570152bb4a2bd3423fac6")

(defn- replace-double-quotes
  [s]
  (if s
  (st/replace s #"\"" "")))

(defn- despace [s]
  (if s
  (st/replace s " " "_")))

(defn handle-forecast-city [resp city]
    (let [current (get-in resp ["list" 0 "main" "temp"])
          next-12h (get-in resp ["list" 4 "main" "temp"])
          next-24h (get-in resp ["list" 8 "main" "temp"])
          next-3d (get-in resp ["list" 24 "main" "temp"])
          next-5d (get-in resp ["list" 39 "main" "temp"])
          city-key (keyword (despace city))]
        (swap! app-state assoc-in [:data city-key :current] (rand-int 200))
        (swap! app-state assoc-in [:data city-key :next-12h] next-12h)
        (swap! app-state assoc-in [:data city-key :next-24h] next-24h)
        (swap! app-state assoc-in [:data city-key :next-3d] next-3d)
        (swap! app-state assoc-in [:data city-key :next-5d] next-5d)))

(defn testtest []
  (js/alert (rand-int 1000)))

(defn error-handler [{:keys [cod message]}]
  (js/console.log  (str "something bad happened: " cod " " message)))

(def unknown-cities ["Episkopi Cantonment" "Nukuʻalofa" "Ngerulmud" "St. Peter Port"])

(defn get-forecast! [city]
  (if (some #{city} unknown-cities)
    nil
  (ajax/GET "http://api.openweathermap.org/data/2.5/forecast"
            {:params {"q" city
                      "units" "imperial" ;; alternatively, use "metric"
                      "appid" api-key}
             :handler (fn [r] (handle-forecast-city r city))
             :error-handler error-handler})))

(defn get-forecast-all! []
  (for [data (vals (:data @app-state))]
  (get-forecast! (:city data))))

(defn- map-city [item]
  (let [[country city city-type] item]
    {(keyword (despace (replace-double-quotes city))) {:country (replace-double-quotes country)
                                                       :city (replace-double-quotes city)
                                                       :city-type (replace-double-quotes city-type)}}))

(defn handle-city [resp]
  (let [list-cities (st/split resp #"\n")
        list-separated-cities (map #(st/split % #",") (rest list-cities))
        city-list (map #(map-city %) list-separated-cities)
        city-map (reduce conj {} city-list)]
    (doall (swap! app-state assoc :data city-map)
           (doall (get-forecast-all!)))))

(defn get-city! []
  (ajax/GET "https://raw.githubusercontent.com/icyrockcom/country-capitals/master/data/country-list.csv"
            {:params {}
             :handler handle-city}))

(defonce add-city (get-city!))

(defn str->int [str] (if (int?  (js/parseInt str)) (js/parseInt str) 3))

(defonce refresh (r/atom (js/setInterval #(doall (get-forecast-all!)) (* 60000
                                                                 (str->int (:refresh-time @app-state))))))

(defn refresh-time-handler [event]
  (do (js/clearInterval @refresh)
      (swap! app-state assoc :refresh-time (.. event -target -value))
      (reset! refresh (js/setInterval #(doall (get-forecast-all!)) (* 60000
                                                      (str->int (.. event -target -value)))))
      ))

(defn sort-temp-handler []
  (let [sort-info (:sort @app-state)
        sort-type (:sorttype sort-info)
        sort-direction (:sortdirection sort-info)]
    (cond
      (and (= sort-type "country") (= sort-direction "ascending")) (su/sort-country-asc app-state)
      (and (= sort-type "country") (= sort-direction "descending")) (su/sort-country-desc app-state)
      (and (= sort-type "city") (= sort-direction "ascending")) (su/sort-city-asc app-state)
      (and (= sort-type "city") (= sort-direction "descending")) (su/sort-city-desc app-state)
      (and (= sort-type "current-temp") (= sort-direction "ascending")) (su/sort-current-temp-asc app-state)
      (and (= sort-type "current-temp") (= sort-direction "descending")) (su/sort-current-temp-desc app-state))))

(defn title []
  [:h1 (:title @app-state)])

(defn refresh-time []
  [:div {:class "refresh-time"}
   [:h3 "Refresh Time (min) : "]
   [:input {:type "text"
            :placeholder "3"
            :value (:refresh-time @app-state)
            :on-change refresh-time-handler}]
   ])

(defn sort-temp []
  [:div {:class "sort"}
   [:h3 "Sort : "]
   [:select {:on-change #(swap! app-state assoc-in [:sort :sorttype] (-> % .-target .-value))}
    [:option {:value "country"} "country"]
    [:option {:value "city"} "city"]
    [:option {:value "current-temp"} "current-temp"]]
   [:select {:on-change #(swap! app-state assoc-in [:sort :sortdirection] (-> % .-target .-value))}
    [:option {:value "ascending"} "ascending"]
    [:option {:value "descending"} "descending"]]
   [:button {:on-click sort-temp-handler} "Go"]
   ])

(defn rows []
  [:table
    [:tr {:key "rheader"}
     (for [h (:header @app-state)]
       [:th {:key h} h])]
   [:tbody {:key "tbody"}
    (for [info (:data @app-state)]
      [:tr {:key (str "row-" (:city (get info 1)))}
       [:td {:key (str "col-" (:city (get info 1)) "-1")} (replace-double-quotes (:country (get info 1)))]
       [:td {:key (str "col-" (:city (get info 1)) "-2")} (replace-double-quotes (:city (get info 1)))]
       [:td {:key (str "col-" (:city (get info 1)) "-3")} (replace-double-quotes (:city-type (get info 1)))]
       [:td {:key (str "col-" (:city (get info 1)) "-4")} (:current (get info 1))]
       [:td {:key (str "col-" (:city (get info 1)) "-5")} (:next-12h (get info 1))]
       [:td {:key (str "col-" (:city (get info 1)) "-6")} (:next-24h (get info 1))]
       [:td {:key (str "col-" (:city (get info 1)) "-7")} (:next-3d (get info 1))]
       [:td {:key (str "col-" (:city (get info 1)) "-8")} (:next-5d (get info 1))]])]])

(defn app []
  [:div {:class "app"}
   [title]
   [refresh-time]
   [sort-temp]
   [rows]])

(defn mount-app-element []
  (rdom/render [app] (gdom/getElement "app")))

(mount-app-element)

(defn ^:after-load on-reload []
  (mount-app-element))