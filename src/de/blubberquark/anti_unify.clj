(ns de.blubberquark.anti-unify
  (:require [clojure.core.unify :as unify])
  (:use [de.blubberquark.unify-util]))

;; Copyright (c) 2015 Robert Pfeiffer
;; This file is part of mini-igor, an inductive programming system
;; for clojure based on the IGOR II system.

;; This file implements an anti-unification algorithm that computes the
;; least general generalisation of two terms. The generalisation format
;; is compatible with clojure.core.unify
;; Anti-unification
;; (clojure.core.unify/unifier (anti-unify A B) A) => A

;; anti-unification algorithm

(defn- same-roots?
  "terms with the same roots can be anti-unified recursively"
  [terms]
  (and (every? seq? terms)
       (every? symbol? (map first terms))
       (apply = (map first terms))
       (apply = (map count terms))))

(defn- same-vecs?
  "n-tuples of the same length can be anti-unified recursively"
  [terms]
  (and (every? vector? terms)
       (apply = (map count terms))))

(declare anti-unify)

(defrecord variable [matches])

(defn- anti-unify-sub
  "anti-unify terms recursively.
  un-named variables are intruduced if necessary"
  [terms n]
  (cond
    (apply = terms)
    (first terms)

    (same-vecs? terms)
    (let [termlists (trps terms)
          subterms (map #(anti-unify-sub % (inc n)) termlists)]
      (vec subterms))

    (same-roots? terms)
    (let [common-root (first (first terms))
          termlists (trps (map rest terms))
          subterms (map #(anti-unify-sub % (inc n)) termlists)]
      (cons common-root subterms))

    :else
    (new variable (vec terms))))

(defn anti-unify
  "anti-unify terms, returning the least general generalisation"
  ([& terms]
   (let [term (anti-unify-sub terms 0)
         var-map (into {}
                       (for [avar (set (filter (partial instance? variable)
                                               (flatten [term])))]
                         [avar (fresh-var)]))
         res (clojure.walk/prewalk-replace var-map term)]
     res)))
