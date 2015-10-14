(ns de.blubberquark.mini-igor-test
  (:require [clojure.test :refer :all]
            [de.blubberquark.mini-igor :refer :all]))

(deftest last-test
  (testing "last element of a list"
    (is (= ((mini-igor [[?x] ?x] [[a ?x] ?x]
                       [[b a ?x] ?x] [[c b a ?x] ?x])
            [1 2 3 4 5 6 :foo]) :foo))))

(deftest length-test
  (testing "length of a list"
    (is (= ((mini-igor [[] 0] [[a] 1] [[b a] 2] 
                       [[c b a] 3]) 
            [1 2 4 5 6 7]) 6))))

(deftest map-test
  (testing "map f over list"
    (is (= ((mini-igor [?f [] []] [?f [a] [(?f a)]] 
                       [?f [b a] [(?f b) (?f a)]]
                       [?f [c b a] [(?f c) (?f b) (?f a)]])
            inc [1.3 2.5 6.4]) [2.3 3.5 7.4]))))

(deftest dec-test
  (testing "decrement int"
    (is (= ((mini-igor [1 0] [2 1] [3 2] [4 3]) 6) 5))))

(deftest inc-test
  (testing "increment int"
    (is (= ((mini-igor [0 1] [1 2] [2 3] [3 4]) 6) 7))))

(deftest half-test
  (testing "halve int"
    (is (= ((mini-igor [0 0] [2 1] [4 2] [6 3]) 12) 6))))

(deftest dbl-test
  (testing "double int"
    (is (= ((mini-igor [0 0] [1 2] [2 4] [3 6]) 6) 12))))
