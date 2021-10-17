(ns clojack.core
  (:gen-class)
  (:require [ring.adapter.jetty :as ring-jetty]
            [ring.middleware.json :refer [wrap-json-response]]
            [ring.util.response :refer [response]]
            [clojack.deck :refer [get-deck]]
            [reitit.ring :as ring]))


(defn not-owned-pred [card]
  (= (:belongs card) 1))

(defn make-owned-pred [player]
  (fn [card]
    (=
     (:belongs card)
     player)))

(defn assign-card-to-player [card-to-assign player]
  (fn [card]
    (if
      (=
       (:id card)
       (:id card-to-assign))
      (assoc card :belongs player)
      card)))
  
(defn hand-val-reducer [accum card]
      (+
       accum
       (:value card)))
      

(defn ace? [card]
  (=
    (:value card)
    1))

(defn has-aces? [hand]
  (> (count (filter ace? hand)) 0))


(defn hit [player deck]
  (let [available-cards (filter not-owned-pred deck)
        random-card (rand-nth available-cards)]
    (map (assign-card-to-player random-card player) deck)))
    
(defn get-player-hand [player deck]
  (filter (make-owned-pred player) deck))

(defn get-player-hand-val [player deck]
  (let [player-hand (get-player-hand player deck)
        raw-value (reduce hand-val-reducer 0 player-hand)]
    (if (and (< raw-value 12) 
          (has-aces? player-hand))
      (+ raw-value 10)
      raw-value)))

(defn hand-busted? [player deck]
  (> (get-player-hand-val player deck) 21))

(hand-busted? 2 [{:value 11 :belongs 2} {:value 3 :belongs 2} {:value 2 :belongs 2}])

(defn healthcheck [_]
  (response {:body {:message "ok"}}))

(defn get-deck-handler [_]
  (response {:body {:deck get-deck}}))

(def app
 (ring/ring-handler
   (ring/router
     ["/" 
       ["deck" {:get (wrap-json-response get-deck-handler)}]
       ["" {:get (wrap-json-response healthcheck)}]])))

(defn start []
  (ring-jetty/run-jetty app {:port 3001}))
                             

(defn -main
  []
  (start))
