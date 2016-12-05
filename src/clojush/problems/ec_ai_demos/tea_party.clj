;; tea_party.clj
;; an example problem for clojush, a Push/PushGP system written in Clojure
;; Elsa Browning, brow3924@morris.umn.edu, 2016
;; Kyle Hakala, hakal053@morris.umn.edu, 2016
;; Dan Woeste,
;; Nic McPhee, mcphee@morris.umn.edu, 2016

(ns clojush.problems.ec-ai-demos.tea-party
  (:use [clojush.pushgp.pushgp]
        [clojush.random]
        [clojush pushstate interpreter]
        clojush.instructions.common))

;;;;;;;;;;;;
;; We are having a party with amounts of tea and candy. Return the int outcome
;; of the party encoded as 0=bad, 1=good, or 2=great. A party is good (1) if
;; both tea and candy are at least 5. However, if either tea or candy is at
;; least double the amount of the other one, the party is great (2). However,
;; in all cases, if either tea or candy is less than 5, the party is always bad (0).
;; Taken from CodingBat: http://codingbat.com/prob/p177181

(def input-set
  [[6, 8] ;1
   [3, 8] ;0
   [20, 6] ;2
   [12, 6] ;2
   [11, 6] ;1
   [11, 4] ;0
   [4, 5] ;0
   [5, 5] ;1
   [6, 6] ;1
   [5, 10] ;2
   [5, 9] ;1
   [10, 4] ;0
   [10, 20] ;2
   ])

(defn expected-output
  [inputs]
  (let [[tea candy] inputs]
    (if (or (< candy 5)
            (< tea 5))
      0
      (if (or
            (>= candy (* 2 tea))
            (>= tea (* 2 candy)))
        2
        1
        )
      )
    )
  )

;(map expected-output input-set)


; Make a new push state, and then add every
; input to the special `:input` stack.
; You shouldn't have to change this.
(defn make-start-state
  [inputs]
  (reduce (fn [state input]
            (push-item input :input state))
          (make-push-state)
          inputs))

; The only part of this you'd need to change is
; which stack(s) the return value(s) come from.
(defn actual-output
  [program inputs]
  (let [start-state (make-start-state inputs)
        end-state (run-push program start-state)
        result (top-item :integer end-state)]
    result))

(defn abs [n]
  (if (< n 0)
    (- n)
    n))

(defn all-errors
  [program]
  (doall
    (for [inputs input-set]
      (let [expected (expected-output inputs)
            actual (actual-output program inputs)]
        (if (= actual :no-stack-item)
          1000
          (abs (- expected actual)))))))

(def atom-generators
  (concat
    ; Include all the instructions that act on integers and booleans
    ; Could have :exec here, but I just am limiting things to exec-if
    (registered-for-stacks [:integer :boolean])
    (list 'exec_if)
    ; A bunch of random numbers in case that's useful.
    ; (list (fn [] (lrand-int 100)))
    ; The three numeric constants that are specified in the problem statement
    (list 5 2)
    ; The two inputs
    (list 'in1 'in2)
    ))

(def argmap
  {:error-function all-errors
   :atom-generators atom-generators
   :population-size 500
   })
