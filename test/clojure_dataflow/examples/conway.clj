(ns [universal-clojure.test.examples.conway]
  (:require [core.match :only [match]]))


;; core.match doesn't support inline predicates, so
;; let's predefine some basic predicates we'll use later
(defn <2? [v]
  (< v 2))

(defn 2? [v]
  (= v 2))

(defn 3? [v]
  (= v 3))

(def >3? [v]
  (> v 3))


(defnode collector [n ne e se s sw w nw]
  "Count the number of neighbors that are alive"
  {:alive-count (count (filter #(= :alive %) [n ne e se s sw w nw]))})


(defnode cell [alive state clock]
  "Perform the actual logic of a node"
  {:state (match [alive state clock]
                 [(_ :when <2?) :alive _] :dead               ;starvation
                 [(_ :when [2? 3?]) :alive _] :alive          ;healthy
                 [(_ :when >3?) :alive _] :dead               ;overcrowded
                 [(_ :when 3?) :dead _] :alive)})             ;reproduction



(defn gen-cell [clock]
  "Generate a cell. Cells are combinations of collectors and cells. So we'll create
  one of each, then we'll connect a few points and finally weld them all together into
  a single unit"
  (let [col (collector)
        c (cell)]
    (connect col :alive-count c :alive)
    (connect c :state c :state)
    (connect clock :tick c :clock)
    (weld col c)))

(def size 200)
(def width size)
(def height size)

(def stateclock (clock 30))

(def grid (into {} (for [x (range width)
                         y (range height)]
                     [[x y] (gen-cell stateclock)])))


(def bound-value [min max value]
  "If a value is out-of-bounds wrap it around"
  (cond (< value min) (+ value max)
        (> value max) (- value min)
        :else value))

(def bound [x y]
  "Constrain x & y to the grid size"
  [(bound-value 0 width x)
   (bound-value 0 height y)])

(def get-cell [x y]
  (get grid (bound x y)))


(dotimes [x width
          y height]
  (let [cell (get-cell x y)]
    (connect (get-cell x       (dec y)) :state cell :n)
    (connect (get-cell (inc x) (dec y)) :state cell :ne)
    (connect (get-cell (inc x) y      ) :state cell :e)
    (connect (get-cell (inc x) (inc y)) :state cell :se)
    (connect (get-cell x       (inc y)) :state cell :s)
    (connect (get-cell (dec x) (inc y)) :state cell :sw)
    (connect (get-cell (dec x) (dec y)) :state cell :w)
    (connect (get-cell (dec x) (dec y)) :state cell :nw)
    ))
