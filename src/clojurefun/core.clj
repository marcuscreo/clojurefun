(ns clojurefun.core
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

:spades
:diamonds
:hearts
:clubs
1 13

{:suit :spades
 :value 1}

;; lists for stacks

{:draw ()

 :discard ()

 :spades ()
 :clubs ()
 :diamonds ()
 :hearts ()

 :lines [(list {:suit :spades :value 1}) (list {:suit :hearts :value 9})]
 :stacks [() (list {:suit :clubs :value 5})]
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

(defn new-game []
  (let [deck (generate-deck)
        [lines deck] (loop [deck deck
                            accum []]
                       (if (= 7 (count accum))
                         [accum deck]
                         (let [[line deck] (deck-take (count accum) deck)]
                           (recur deck (conj accum line)))))

(vec (for [x (range 7)]
                     (take x deck)))
        
    {:draw ()

     :discard ()

     :spades ()
     :clubs ()
     :diamonds ()
     :hearts ()

     :lines [(list {:suit :spades :value 1}) (list {:suit :hearts :value 9})]
     :stacks [() (list {:suit :clubs :value 5})]}))
