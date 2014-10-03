(ns clojurefun.core
  (:require
   [medley.core :refer :all]
   [clojure.pprint :refer []])
  (:gen-class))


(defn fib [x]
  (cond
   (= x 0) []
   (= x 1) [1]
   (= x 2) [1 1]
   :else (let [fib-1 (fib (- x 1))
               c (count fib-1)
               fib1 (nth fib-1 (- c 1))
               fib2 (nth fib-1 (- c 2))]
           (conj fib-1 (+ fib1 fib2)))))

(comment
  :spades
  :diamonds
  :hearts
  :clubs
  1 13)

{:suit :spades :value 1}

;; lists for face-down

{:draw ()
 :discard ()
 :spades ()
 :clubs ()
 :diamonds ()
 :hearts ()
 :face-up [(list {:suit :spades :value 1}) (list {:suit :hearts :value 9})]
 :face-down [() (list {:suit :clubs :value 5})]
}

(defn win? [game-state]
  (= 52 (+ (count (:spades game-state))
           (count (:clubs game-state))
           (count (:diamonds game-state))
           (count (:hearts game-state)))))


(defn deck-take [n deck]
  [(take n deck) (drop n deck)])

(def suits [:spades :clubs :diamonds :hearts])

(defn generate-card [s v] {:suit s :value v})

(defn generate-deck []
  (for [s suits
        v (range 1 14)]
      (generate-card s v)))


(defn generate-game []
  (let [deck (shuffle (generate-deck))
        aces [:spades (), :clubs (), :diamonds (), :hearts () ] 
        face-up [[] [] [] [] [] [] []]
        face-down [[] [] [] [] [] [] []]
        discard []]
    {:aces aces, :deck deck, :face-up face-up, :face-down face-down, :discard discard}))


(defn deck-drop [d n]
  (drop 5 d))


(defn new-game []
  (let [deck (shuffle (generate-deck))
        [face-up deck] (loop [deck deck
                              face-up []]
                       (if (= 7 (count face-up))
                         [face-up deck]
                         (let [[f deck] (deck-take (count face-up) deck)]
                           (recur deck (conj face-up f)))))
        [stack-cards deck] (deck-take 7 deck)
        face-down (mapv list stack-cards)
        aces { :spades (), :clubs (), :diamonds (), :hearts () }
        discard ()
        ]
    {:face-up face-up
     :face-down face-down
     :deck deck
     :aces aces
     :discard discard}))

(comment
  ;; move top of deck to face-down or aces - done
  ;; move top n cards from face-up to face-up - done
  ;; move top card from face-up to aces - done
  ;; move top of deck to discard - done
  ;; move top of aces to face-up - done
  )



{:n 4 :from [:deck] :to [:face-down 2]}

;;(update-in game [:face-down 2] conj (get-in game [:deck]))

(defn all-moves []
  (lazy-cat
   [{:n 1 :from [:deck] :to [:discard]}] ;; move 1 card from deck to
   ;; discard
   (for [suit [:spades :clubs :diamonds :hearts]]
     {:n 1 :from [:deck] :to [:aces suit]})
   (for [i (range 7)]
     {:n 1 :from [:deck] :to [:face-up i]})
   (for [i (range 7) suit suits]
     {:n 1 :from [:face-up i] :to [:aces suit]})
   (for [i (range 7) suit suits]
     {:n 1 :from [:aces suit] :to [:face-up i]})
   (for [c (range 1 14) f (range 7) t (range 7)]
     {:n c :from [:face-up f] :to [:face-up t]})))
;; Add all the moves listed above

(comment

  )


(defn valid? [game move]
  (and  (<= (:n move) (count  (get-in game (:from move))))
        (cond
         (= :aces (first (:to move)))
         (let [card (first (get-in game (:from move)))
               top (first (get-in game (:to move)))]
           (and
            (= 1 (:n move)) ;;Only move 1 card to aces
            (= (second (:to move)) (:suit card))
            (if (nil? top)
              (= 1 (:value card))
              (= (inc (:value top)) (:value card)))))

         :else
         true)


        ;; If the ace stack is empty, then we don't have to check the
        ;; top card, but we can only put an Ace down.  If there is a
        ;; card in the Ace stack, then the one we are moving to must
        ;; be exactly 1 greater in value.  N must be 1.
                ))


;;Write valid, using the cases above

(defn aces-valid? [game move]
 (and
         (= :aces (first (:to move)))
         (let [card (first (get-in game (:from move)))
               top (first (get-in game (:to move)))]
           (and
            (= 1 (:n move)) ;;Only move 1 card to aces
            (= (second (:to move)) (:suit card))
            (if (nil? top)
              (= 1 (:value card))
              (= (inc (:value top)) (:value card)))))))



(defn play-move [game move]
  (assert (valid? game move))
  (update-in 
   (update-in game (:to move) #(concat %2 %1) (take (:n move) (get-in game (:from move))))
   (:from move) #(drop %2 %1) (:n move)))



{
     :discard ()
     :spades ()
     :clubs ()
     :diamonds ()
     :hearts ()

     :face-up [(list {:suit :spades :value 1}) (list {:suit :hearts :value 9})]
     :face-down [() (list {:suit :clubs :value 5})]}


