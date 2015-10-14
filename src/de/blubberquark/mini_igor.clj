(ns de.blubberquark.mini-igor
  (:require [clojure.core.unify :as unify]
            [de.blubberquark.anti-unify :as anti-unify])
  (:use [de.blubberquark.unify-util]))

;; Copyright (c) 2015 Robert Pfeiffer
;; This file is part of mini-igor, an inductive programming system
;; for clojure based on the IGOR II system.

;; For an easy-to follow overview of the IGOR algorithm, check out
;; http://www.cogsys.wiai.uni-bamberg.de/publications/pepm2010-tooldemo.pdf

;; constructive and matchable representations
;; TODO: put this into core.unify if possible

(defn s [n]
  (list `s n))

(defn cns [a b]
  (list `cns a b))

(defn int->peano [n]
  (if (= n 0) 0
      (s (int->peano (dec n)))))

(defn list->conses [l]
  (if (empty? l) nil
      (cns (first l) (list->conses (rest l)))))

(defn igor-mangle
  "convert clojure data-types into igor structure"
  [x]
  (cond (vector? x)  (list->conses (map igor-mangle x))
        (integer? x) (int->peano x)
        (list? x)    (map igor-mangle x)
        :else x))

(defn igor-mangle-args [x]
  (map igor-mangle x))

(defn igor-unmangle
  "convert igor data structures back into clojure"
  [x]
  (eval (clojure.walk/prewalk-replace {`s inc, `cns cons} x)))

;; induction of functions

(defn- split-example [x]
  [(vec (butlast x)) (last x)])

(defn- check-flow
  "check dataflow from induced program
  can the result be calculated from the given inputs?
  if not, throw an exception"

  [input output step-input step-output]
  (let [output-variables (harvest-vars output)
        input-variables (harvest-vars input)

        step-out (harvest-vars step-output)
        step-in (harvest-vars step-input)

        recur-possible?
        (clojure.set/subset? step-in input-variables)

        recur-needed?
        (and recur-possible?
             (not (clojure.set/subset?
                   output-variables
                   input-variables)))

        known-variables
        (if recur-possible?
          (clojure.set/union input-variables step-out)
          input-variables)

        unknown-variables
        (clojure.set/difference output-variables
                                known-variables)

        good? (empty? unknown-variables)]

    (when (not good?)
      (throw (new Exception (str "could not induce " unknown-variables
                                 " in " (print-str output)))))
    [recur-possible? recur-needed? good?]))

(defn- mini-igor-raw
  [examples]
  (let [examples (map (comp split-example igor-mangle-args) examples)
        [base-in base-out] (first examples)
        igor-input (for [[[in1 out1] [in2 out2]]
                         (partition 2 1 examples)]
                     [in2 in1 out1 out2])
        general (apply anti-unify/anti-unify igor-input)
        general (pretty-rename general)
        [params step-params step rule] general
        [_ recur-needed? good?]
        (check-flow params rule step-params step)

        induced (gensym 'induced)
        args (gensym 'args)]

    `(fn ~induced [& ~args]
       (pattern-match

        [~base-in ~args]
        ~base-out

        [~(vec params) ~args]
        ~(if (not recur-needed?)
           rule
           `(let [step# (~induced ~@step-params)]
              (pattern-match
               [~step step#]
               ~rule
               (throw (new IllegalArgumentException
                           (str "Step Matching Failed" step# '~step))))))
        (throw (new IllegalArgumentException
                    (str "Matching Failed" ~args '~params)))))))

(defn wrap-igor-fn [induced-fn]
  (fn [& args]
    (assert (grounded? args))
    (let [input (igor-mangle-args args)
          result (apply induced-fn input)]
      (igor-unmangle result))))

(defmacro mini-igor
  [& examples]
  `(wrap-igor-fn ~(mini-igor-raw examples)))

;;((mini-igor [0 1] [1 2] [2 3] [3 4]) 3)
;;((mini-igor [0 0] [1 0] [2 1] [3 1] [4 2] [5 2]) 6)
;;((mini-igor [7 0] [6 1] [5 2] [4 3] [3 4]) 2)
;;((mini-igor [0 7] [1 6] [2 5] [3 4] [4 3]) 5)
;;((mini-igor [0 0] [1 1] [2 0] [3 1] [4 0] [5 1] [6 0]) 7)
;;((mini-igor [[] 0] [[1] 1] [[0 1] 1] [[1 0 1] 2] [[1 1 0 1] 3]) [1 1 1 1])
;;((mini-igor [0 1 1] [1 1 2] [1 2 3] [3 2 5]) 2 3)
;;((mini-igor [0 1 1] [1 1 2] [2 1 3] [3 1 4]) 8 1)
;;((mini-igor [0 0] [1 3] [2 6] [3 9]) 5)
