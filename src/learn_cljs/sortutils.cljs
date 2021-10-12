(ns learn-cljs.sortutils)

(defn sort-country-asc [s]
  (let [data (:data @s)
        data-sorted (into (sorted-map-by (fn [key1 key2]
                                           (compare
                                             (get-in data [key1 :country])
                                             (get-in data [key2 :country]))))
                          data)]
    (swap! s assoc :data data-sorted)))

(defn sort-country-desc [s]
  (let [data (:data @s)
        data-sorted (into (sorted-map-by (fn [key1 key2]
                                           (compare
                                             (get-in data [key2 :country])
                                             (get-in data [key1 :country]))))
                          data)]
    (swap! s assoc :data data-sorted)))

(defn sort-current-temp-asc [s]
  (let [data (:data @s)
        data-sorted (into (sorted-map-by (fn [key1 key2]
                                           (compare
                                             (get-in data [key1 :current])
                                             (get-in data [key2 :current]))))
                          data)]
    (swap! s assoc :data data-sorted)))

(defn sort-current-temp-desc [s]
  (let [data (:data @s)
        data-sorted (into (sorted-map-by (fn [key1 key2]
                                           (compare
                                             (get-in data [key2 :current])
                                             (get-in data [key1 :current]))))
                          data)]
    (swap! s assoc :data data-sorted)))

(defn sort-city-asc [s]
  (let [data (:data @s)
        data-sorted (into (sorted-map-by <) data)]
    (swap! s assoc :data data-sorted)))

(defn sort-city-desc [s]
  (let [data (:data @s)
        data-sorted (into (sorted-map-by >) data)]
    (swap! s assoc :data data-sorted)))