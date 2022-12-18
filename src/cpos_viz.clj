(ns cpos-viz
  (:require [clojure.string :as str]

            [convex.clj :as clj]
            [convex.cell  :as cell :refer [*] :rename {* cell}]
            [convex.key-pair :as kp]
            [convex.cvm :as cvm]

            [io.github.humbleui.canvas :as canvas]
            [io.github.humbleui.core :as core]
            [io.github.humbleui.paint :as paint]
            [io.github.humbleui.ui :as ui :refer [label]]
            [io.github.humbleui.window :as window]
            [io.github.humbleui.typeface :as typeface]
            [io.github.humbleui.font :as font]
            [io.github.humbleui.cursor :as cursor]

            [colors :as colors :refer [->color]])
  (:import [convex.core.data ARecord AccountStatus AccountKey PeerStatus SignedData]
           [convex.core.lang RT Juice]
           [convex.core Order Peer Belief MergeContext Constants Block State]))

(defn new-block [ts txs]
  (Block/of ts (into-array SignedData txs)))

(defn merge-beliefs [peer beliefs]
  (.mergeBeliefs peer (into-array Belief beliefs)))

(defn share-beliefs
  "Each peer merges its belief with all other peers"
  [peers]
  (let [shared-beliefs (mapv (fn [p] (.getBelief p)) peers)]
    (mapv
      (fn [p]
        (merge-beliefs p shared-beliefs))
      peers)))

;;; todo: allow random seed, be more efficient
(defn share-gossip
  "Each peer merges its belief with `num-gossips` other peers"
  [peers num-gossips]
  (mapv
    (fn [p]
      (let [others (vec (disj (set peers) p))]
        (->> others
             (shuffle)
             (take num-gossips)
             (map #(.getBelief %))
             (merge-beliefs p))))
    peers))

;;; todo: allow repeatable timestamp.
(defn propose-txs [peers peer txs]
  (let [signed-txs (mapv (fn [tx] (.sign peer tx)) txs)
        peer-index (reduce (fn [ix p] (if (= p peer) (reduced ix) (inc ix))) 0 peers)
        new-ts     (System/currentTimeMillis)
        block      (new-block new-ts signed-txs)
        peers      (assoc (vec peers) peer-index (.proposeBlock peer block))]
    peers))

(defn init-peers [num-peers]
  (let [peer-maps (for [i    (range num-peers)
                        :let [pair (kp/ed25519)
                              k (kp/account-key pair)
                              a (cell/address i)]]
                    {:acct        (AccountStatus/create (* (inc i) 1000000) k)
                     :keypair     pair
                     :key         k
                     :address     a
                     :peer-status (PeerStatus/create a (* (inc i) 100000))})
        globals   (.assoc
                    Constants/INITIAL_GLOBALS
                    2 ;;global_juice_price index
                    (cell/long 1))
        state     (State/create
                    (cell/vector (map :acct peer-maps))
                    (cell/blob-map (map (juxt :key :peer-status) peer-maps))
                    globals
                    (cell/blob-map))]
    (mapv (fn [{:keys [keypair] :as x}]
            (Peer/create keypair state))
      peer-maps)))

;;; Note: sequence number is wrong but I don't think it currently matters for this
(defn random-transfer
  "Random peer will send transaction to a different random peer"
  [peers]
  (let [p      (rand-nth peers)
        others (vec (disj (set peers) p))]
    (propose-txs peers p
      [(cell/transfer
         (.getController p)
         1
         (.getController (rand-nth others))
         100)])))

(defn init-state [peer-count]
  (let [peers (init-peers peer-count)]
    {:peers      peers
     :peer-color (zipmap
                   (map (fn [p] (str (.getPeerKey p))) peers)
                   (->> colors/colors
                        (sort-by val)
                        (drop 10)
                        (take 70)
                        keys
                        shuffle))}))

(defn button [& {:keys [text on-click]}]
  (ui/halign 0.0
    (ui/button on-click
      (ui/valign 0.5
        (ui/label text)))))

;;; todo: move these to cursors
(def gossip-count (atom {:text "5"}))
(def tx-count (atom {:text "2"}))
(def peer-count (atom {:text "10"}))

(defn sidebar [state]
  (ui/with-context
    {:hui.button/bg         (paint/fill (->color :gray))
     :hui.button/bg-hovered (paint/fill (->color :gray 150))}
    (ui/row
      (ui/padding 10
        (ui/focus-controller
          (ui/column
            (ui/row
              (button
                :text "reset"
                :on-click (fn []
                            (reset! state (init-state (parse-long
                                                        (:text @peer-count)) ))))
              (ui/gap 6 0)
              (ui/width 50
                (ui/text-field {} peer-count)))

            (ui/gap 0 10)

            (ui/row
              (button
                :text "share all"
                :on-click (fn [] (swap! state update :peers share-beliefs))))
            (ui/gap 0 10)


            (ui/row
              (button
                {:on-click (fn []
                             (dotimes [_ (parse-long (:text @tx-count))]
                               (swap! state update :peers random-transfer)))
                 :text     "random tx"})
              (ui/gap 6 0)
              (ui/width 50
                (ui/text-field {} tx-count)))


            (ui/gap 0 10)
            (ui/row
              (button
                {:on-click (fn [] (swap! state update :peers share-gossip
                                    (parse-long (:text @gossip-count))))
                 :text     "gossip"})
              (ui/gap 6 0)
              (ui/width 50
                (ui/text-field {} gossip-count)))


            (ui/gap 0 10)
            (button
              {:on-click (fn []
                           (swap! state update :peers
                             (fn [peers]
                               (let [txs     (parse-long (:text @tx-count))
                                     gossips (parse-long (:text @gossip-count))]
                                 (-> (reduce
                                       (fn [peers _]
                                         (random-transfer peers))
                                       peers
                                       (range txs))
                                     (share-gossip gossips))

                                 #_(-> (random-transfer peers)
                                       )))))
               :text "step!"})))))))

(defn content [state]
  (ui/dynamic ctx [{:keys [peers peer-color]} @state]
    (ui/vscrollbar
      (do #_ui/vscroll
          (ui/column

            (let [min-cpoint (apply min
                               (map (fn [p]
                                      (.getConsensusPoint p))
                                 peers))]
              (list
                (for [[peer-ix peer] (map-indexed vector peers)
                      :let
                      [peer-key (.getPeerKey peer)
                       ;; todo: peer-color fn should coerce to str?
                       bg (->color (peer-color (str (.getPeerKey peer))) 255 )
                       belief (.getBelief peer)
                       order (.getPeerOrder peer)
                       ppoint (.getProposalPoint order)
                       cpoint (.getConsensusPoint order)
                       min-block-width 5
                       blocks (->>
                                (map-indexed
                                  (fn [ix o]
                                    [ix
                                     (str (.getAccountKey o))
                                     (.getValue o)])
                                  (.getBlocks order))
                                (drop (- min-cpoint min-block-width)))

                       ]]
                  (ui/padding 0 5 0 5
                    (ui/row
                      (ui/width 70
                        (ui/rect (paint/fill bg)
                          (ui/center
                            (ui/padding 5 (ui/label {:paint (paint/fill (->color :white))}
                                            (str "peer" peer-ix))))))
                      (ui/gap 5 30)
                      (for [[ix acct-key block] blocks
                            :let                [color (-> acct-key peer-color ->color)
                                                 _ (assert color)]]
                        (list
                          (ui/stack
                            (ui/valign 0.5
                              (ui/rect (paint/fill color)
                                (ui/gap 30 30)))
                            (ui/center
                              (ui/label
                                {:paint (paint/fill (->color :white))}
                                ix)))
                          (ui/gap 2 0)
                          (ui/rect
                            (paint/fill (->color :black (if (= (inc ix) cpoint) 255 0)))
                            (ui/gap 5 30))
                          (ui/gap 2 0)
                          (ui/rect
                            (paint/fill (->color :red (if (= (inc ix) ppoint) 255 0)))
                            (ui/gap 5 30))
                          (ui/gap 2 0)))))))))))))

(defn vline []
  (ui/rect (paint/fill (->color :black )) (ui/gap 1 0)))

(defn ui [state]
  (ui/row
    [:stretch 10
     (ui/clip
       (ui/rect (paint/fill (->color :blue 10))
         (ui/padding 10
           (content state))))]
    (vline)
    [:stretch 2
     (ui/rect (paint/fill (->color :red 10))
       (ui/padding 10
         (sidebar state)))]))

(defonce *window (atom nil))
(defonce *app (atom
                (ui/default-theme
                  {}
                  (ui/center (ui/gap 1 1)))))
(defonce *state (atom (init-state 10)))

(defn set-ui! [state]
  (reset! *app
    (ui/default-theme {}
      (ui/padding 10 ;;glitchy at the edges...
        (ui state)))))


(defn redraw! []
  (some-> *window deref window/request-frame))

(add-watch *state ::redraw-peers
  (fn [_ _ _ state]
    (redraw!)))


(defn -main [& args]
  (ui/start-app!
    (reset! *window
      (ui/window
        {:title    "convex consensus visual"
         :bg-color 0xFFFFFFFF
         :width 1500
         :height 600}
        *app)))
  (set-ui! *state)
  (redraw!))



(comment
  (-main)
  )
