(ns de.blubberquark.unify-util
  (:require [clojure.core.unify :as unify]
            [clojure.set]))

;; Copyright (c) 2015 Robert Pfeiffer
;; This file is part of mini-igor, an inductive programming system
;; for clojure based on the IGOR II system.

;; utilities for mini-igor and for core.unify in general

(defn trps
  "transpose arguments, a list of n lists becomes a list of n-tuples"
  [lists]
  (apply map list lists))

(def canned-vars '[?a ?b ?c ?d ?e ?f ?g ?h ?j ?k ?l ?m])

(defn fresh-var
  "generate a gensym for matching with clojure.core.unify"
  [] (symbol (str "?" (gensym))))

(defn harvest-vars
  "get a set of all unification variables occuring in a term"
  [struct]
  (set (filter unify/lvar? (flatten [struct]))))

(defn- matchable-vars
  "get a set of all unification variables or wildcards occuring in a term"
  [struct]
  (set (filter #(or (unify/lvar? %) (= % '_))   (flatten [struct]))))

(defn grounded? [term]
  (empty? (matchable-vars term)))

(defn alpha-rename
  "replace all core.unify variables with fresh-vars, but keep the structure"
  [pat]
  (let [vars (harvest-vars pat)
        sub (into {}
                  (for [v vars]
                    [v (symbol (str "?" (gensym)))]))]
    (unify/subst pat sub)))

(defn pretty-rename [pat]
  "replace all core.unify variables with short readable ones"
  (let [;pat (alpha-rename pat)
        vars (harvest-vars pat)
        sub (into {}
                  (for [[v sym] (trps [vars (concat canned-vars (repeat nil))])]
                    [v (or sym (fresh-var))]))]
    (unify/subst pat sub)))

(defn- unpack-map
  "turn a list of symbols into a hash-map
  useful for destructuring hash-maps in macros"
  [vars]
  (into {} (for [var vars] [var `(quote ~var)])))

(defmacro pattern-match
  "try to unify pattern and args
  if successful, bind vars from pattern and execute body"
  ([] nil)
  ([else] else)
  ([[pattern args] body & else]
   (let [;[pattern body] (alpha-rename [pattern body])
         variables (harvest-vars pattern)]
     `(let [args# ~args]
        (assert (grounded? args#))
        (if-let
            [~(unpack-map variables)
             (unify/unify args# '~pattern)]
          ~body
          (pattern-match ~@else))))))

(defn try-rewrite [val pat1 pat2]
  (let [[pat1 pat2] (alpha-rename [pat1 pat2])
        bindings (unify/unify pat1 val)]
    (if bindings
      (let
          [bindings2 (merge bindings (clojure.set/map-invert bindings))
           vars (harvest-vars pat1)
           bindings3 (into {}
                           (for [v vars]
                             [v (bindings2 v)]))]
        (unify/subst pat2 bindings3))
      val)))

(defn rewrite [val pat1 pat2]
  (let [[pat1 pat2] (alpha-rename [pat1 pat2])
        bindings (unify/unify pat1 val)
        bindings2 (merge bindings (clojure.set/map-invert bindings))
        vars (harvest-vars pat1)
        bindings3 (into {}
                        (for [v vars]
                          [v (bindings2 v)]))]
    (unify/subst pat2 bindings3)))

(defn rewriter [pat1 pat2]
  (let [[pat1 pat2] (alpha-rename [pat1 pat2])
        vars (harvest-vars pat1)]
    (fn [val]
      (if-let [bindings (unify/unify pat1 val)]
        (let [bindings2 (merge bindings (clojure.set/map-invert bindings))
              bindings3 (into {}
                              (for [v vars]
                                [v (bindings2 v)]))]
          (unify/subst pat2 bindings3))
        val))))
