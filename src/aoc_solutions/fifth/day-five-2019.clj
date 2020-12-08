(ns aoc-solutions.fifth.day-five-2019
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout]])
  (:gen-class))

(defn calc [op opnd1 opnd2]
  "calls op with opnd1 and opnd2 as arguments"
  (op opnd1 opnd2))

(defn first-op [program counter output current-base op1 op2 op3]
  "sums op1 and op2 and writes the result on position op3"
;  (println current-base op1 op2 op3)
  {:program (assoc program op3 (+ op1 op2))
   :output output
   :current-base current-base
   :counter (+ 4 counter)})

(defn second-op [program counter output current-base op1 op2 op3]
  "multiplies op1 and op2 and writes the result on position op3"
  {:program (assoc program op3 (* op1 op2))
   :output output
   :current-base current-base
   :counter (+ 4 counter)})

(defn third-op [program counter output current-base operand]
  "takes input and writes it on position designated by operand"
  "here input is being hardcoded to avoid having to respond to a prompt, possibly multiple times"
;  (println "inputting")
  (let [input 1]
    {:program (assoc program operand input)
     :output output
     :current-base current-base
     :counter (+ 2 counter)}))

(defn fourth-op [program counter output current-base operand]
  "return values in position operand"
;  (println "output" operand)
  {:program program
   :output (conj output operand)
   :current-base current-base
   :counter (+ 2 counter)})

(defn jump-if-true [program counter output current-base opnd1 opnd2]
  "this is fifth op. either moves program counter to opnd2, or increments it by 3"
  {:program program
   :output output
   :current-base current-base
   :counter (if (= 0 opnd1) (+ 3 counter) opnd2)})

(defn jump-if-false [program counter output current-base opnd1 opnd2]
  "this is the sixth op. either moves program counter to opnd2, or increments it by 3"
  {:program program
   :output output
   :current-base current-base
   :counter (if (= 0 opnd1) opnd2
                (+ counter 3))})

(defn less-than [program counter output current-base opnd1 opnd2 opnd3]
  {:counter (+ 4 counter)
   :output output
   :current-base current-base
   :program (assoc program opnd3 (if (< opnd1 opnd2) 1 0))})

(defn equalss [program counter output current-base opnd1 opnd2 opnd3]
  {:counter (+ 4 counter)
   :output output
   :current-base current-base
   :program (assoc program opnd3 (if (= opnd1 opnd2) 1 0))})

(defn relative-base [program counter output current-base op]
;  (println "updating current base" current-base op counter)
  {:program program
   :counter (+ counter 2)
   :output output
   :current-base (+ current-base op)})

(defn get-op-function [o]
  "return relevant operation given a number o to identify it"
  ([first-op
    second-op
    third-op
    fourth-op
    jump-if-true
    jump-if-false
    less-than
    equalss
    relative-base] (dec o)))

(defn read-param [program mode param current-base operation]
  "reads param from program depending on mode: 1 for by value, 0 by reference, 2 by relative mode [param + current-base]"
  (case mode
    0 (nth program param)
    1 param
    2 (nth program (+ current-base param))
    4 (+ current-base param)
    ))

(defn get-operation [x]
  "returns op-code [01, 02... and so on"
  (rem x 100))

(defn read-modes [input op-count op-code]
  "builds a list that holds ones and zeroes to indicate what mode"
  "to read a parameter in. first in this list, indicates the mode"
  "in which to read the first parameter to an operation, and so on."
  (let [ones (rem input 10)
        tens (quot (rem input 100) 10)
        hundreds (rem (quot input 100) 10)]
    (case op-code
      1 (list ones tens (if (= hundreds 2) 4 1))
      2 (list ones tens (if (= hundreds 2) 4 1))
      3 (list (if (= ones 2) 4 1))
      4 (list ones)
      5 (list ones tens)
      6 (list ones tens)
      7 (list ones tens (if (= hundreds 2) 4 1))
      8 (list ones tens (if (= hundreds 2) 4 1))
      9 (list ones))))

(defn get-function-operands [op-number program counter]
  "this is aware of how many operands an operation takes, and"
  "takes as many values from program, skiping one as it is"
  "where operation number is held inside program"
  (case op-number
    1 (take 3 (drop (inc counter) program))
    2 (take 3 (drop (inc counter) program))
    3 (take 1 (drop (inc counter) program))
    4 (take 1 (drop (inc counter) program))
    5 (take 2 (drop (inc counter) program))
    6 (take 2 (drop (inc counter) program))
    7 (take 3 (drop (inc counter) program))
    8 (take 3 (drop (inc counter) program))
    9 (take 1 (drop (inc counter) program))))

(defn execute 
  ([program inchan outchan counter current-base]
   (execute (conj (conj program inchan) outchan) counter [] current-base get-op-function 1))
  ([program counter output current-base]
   (execute counter output current-base get-op-function 1))
  ([program counter output current-base get-op-function dummy]
;   (println "counter" counter)
   (let [op-code (program counter)
         operation (get-operation op-code)]
     (if (= 99 operation)
       ;(last output)
       (do (println "finito")
           output)
       (let [op-function (get-op-function operation)
             function-operands (get-function-operands operation program counter)
             modes (read-modes (quot op-code 100) (count function-operands) operation)
             pair-mode-operand (map list modes function-operands)
             eval-operands (map #(read-param program (first %) (last %) current-base operation) pair-mode-operand)
             update-program (apply op-function (concat (list program counter output current-base) eval-operands))]
         (recur (:program update-program)
                (:counter update-program)
                (:output update-program)
                (:current-base update-program) get-op-function dummy))))))

(def input [3 225 1 225 6 6 1100 1 238 225 104 0 1102 45 16 225 2 65 191 224 1001 224 -3172 224 4 224 102 8 223 223 1001 224 5 224 1 223 224 223 1102 90 55 225 101 77 143 224 101 -127 224 224 4 224 102 8 223 223 1001 224 7 224 1 223 224 223 1102 52 6 225 1101 65 90 225 1102 75 58 225 1102 53 17 224 1001 224 -901 224 4 224 1002 223 8 223 1001 224 3 224 1 224 223 223 1002 69 79 224 1001 224 -5135 224 4 224 1002 223 8 223 1001 224 5 224 1 224 223 223 102 48 40 224 1001 224 -2640 224 4 224 102 8 223 223 1001 224 1 224 1 224 223 223 1101 50 22 225 1001 218 29 224 101 -119 224 224 4 224 102 8 223 223 1001 224 2 224 1 223 224 223 1101 48 19 224 1001 224 -67 224 4 224 102 8 223 223 1001 224 6 224 1 223 224 223 1101 61 77 225 1 13 74 224 1001 224 -103 224 4 224 1002 223 8 223 101 3 224 224 1 224 223 223 1102 28 90 225 4 223 99 0 0 0 677 0 0 0 0 0 0 0 0 0 0 0 1105 0 99999 1105 227 247 1105 1 99999 1005 227 99999 1005 0 256 1105 1 99999 1106 227 99999 1106 0 265 1105 1 99999 1006 0 99999 1006 227 274 1105 1 99999 1105 1 280 1105 1 99999 1 225 225 225 1101 294 0 0 105 1 0 1105 1 99999 1106 0 300 1105 1 99999 1 225 225 225 1101 314 0 0 106 0 0 1105 1 99999 7 226 677 224 102 2 223 223 1005 224 329 1001 223 1 223 8 226 677 224 1002 223 2 223 1005 224 344 101 1 223 223 8 226 226 224 1002 223 2 223 1006 224 359 101 1 223 223 1008 677 226 224 1002 223 2 223 1005 224 374 1001 223 1 223 108 677 677 224 1002 223 2 223 1005 224 389 1001 223 1 223 1107 226 677 224 1002 223 2 223 1006 224 404 101 1 223 223 1008 226 226 224 102 2 223 223 1006 224 419 1001 223 1 223 7 677 226 224 1002 223 2 223 1005 224 434 101 1 223 223 1108 226 226 224 1002 223 2 223 1005 224 449 101 1 223 223 7 226 226 224 102 2 223 223 1005 224 464 101 1 223 223 108 677 226 224 102 2 223 223 1005 224 479 1001 223 1 223 1007 677 226 224 1002 223 2 223 1006 224 494 1001 223 1 223 1007 677 677 224 1002 223 2 223 1006 224 509 1001 223 1 223 107 677 677 224 1002 223 2 223 1005 224 524 101 1 223 223 1108 226 677 224 102 2 223 223 1006 224 539 1001 223 1 223 8 677 226 224 102 2 223 223 1005 224 554 101 1 223 223 1007 226 226 224 102 2 223 223 1006 224 569 1001 223 1 223 107 677 226 224 102 2 223 223 1005 224 584 1001 223 1 223 108 226 226 224 102 2 223 223 1006 224 599 1001 223 1 223 107 226 226 224 1002 223 2 223 1006 224 614 1001 223 1 223 1108 677 226 224 1002 223 2 223 1005 224 629 1001 223 1 223 1107 677 677 224 102 2 223 223 1005 224 644 1001 223 1 223 1008 677 677 224 102 2 223 223 1005 224 659 101 1 223 223 1107 677 226 224 1002 223 2 223 1006 224 674 101 1 223 223 4 223 99 226])

(defn a []
  (execute input 0 [] 0 1))
