(ns lab1.core
  (:gen-class)
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
  (import java.lang.Math)
)

;(use 'clojure.java.io)
;(use 'clojure.string)

(def radius-alpha 2)
(def radius-beta (* radius-alpha 1.5))
(def upper-threshold 0.5)
(def lower-threshold 0.15)


(defn alpha[]
  (/ 4 (* radius-alpha radius-alpha))
)

(defn beta[]
  (/ 4 (* radius-beta radius-beta))
)

(defn distance-between-points[x1 x2]
  (* (- x1, x2) (- x1, x2))
)


(defn e-distance[coordinate1 coordinate2]
  (Math/sqrt (reduce +
      (map distance-between-points coordinate1 coordinate2)
    )
  )
)

(defn delete-from-collection[elem collection]
  (let [collection-length (count collection)]
    (loop [iteration 0 new-collection ()]
      (if (< iteration collection-length)
        (if (not (= elem (nth collection iteration)))
          (recur (inc iteration) (conj new-collection (nth collection iteration)))
          (recur (inc iteration) new-collection)
        )
        new-collection
      )
    )
  )
)


(defn calc-potentinal[distance coef]
  (Math/exp(-(* coef distance)))
)

(defn parse-line[line]
  (map #(Double/parseDouble %1) (drop-last (string/split line #",")))
)

(defn parse-file [file-path]
  (into []
    (with-open [rdr (io/reader file-path)]
      (doall (map parse-line(line-seq rdr)))
    )
  )
)

(defn get-new-potentials [potentials max-potential coordinate-with-max-potential coordinates]
  (map (fn[potential coordinate] (- potential (* max-potential (calc-potentinal (e-distance coordinate coordinate-with-max-potential) (beta))))) potentials coordinates)
)

(defn get-potential[coordinate coordinates]
   (reduce +
      (map #(calc-potentinal (e-distance coordinate %) (alpha)) coordinates)
   )
)

(defn get-potentials[coordinates]
  (map #(get-potential %1 coordinates) coordinates)
)

(defn get-max-potential[potentials]
  (apply max potentials)
)

(defn get-max-potential-index[max-potential potentials coordinates]
  (loop [iteration 0]
    (if (= max-potential (nth potentials iteration))
      iteration
      (recur (inc iteration))
    )
  )
)

(defn get-coordinate-with-max-potential[max-potential-index coordinates]
  (loop [iteration 0]
    (if (= max-potential-index iteration)
      (nth coordinates iteration)
      (recur (inc iteration))
    )
  )
)

(defn get-original-coordinate-index[coordinate original-coordinates]
  (loop [iteration 0]
    (if (= coordinate (nth original-coordinates iteration))
      iteration
      (recur (inc iteration))
    )
  )
)

(defn is-center [first-max-potential potential coordinate cores]
  (if (> potential (* upper-threshold first-max-potential))
    1
    (if (< potential (* lower-threshold first-max-potential))
      0
      (let [min-distance (apply min (map (fn[core] (e-distance coordinate core)) cores))]
        (if (< 1 (+ (/ min-distance radius-alpha) (/ potential first-max-potential)))
          1
          2
        )
      )
    )
  )
)

(defn final-fockin-loop[file-path]
  (let [original-coordinates-collection (parse-file file-path)
        coordinates-collection (parse-file file-path)
        potentials (get-potentials coordinates-collection)
        first-max-potential (get-max-potential potentials)]

    (loop [coordinates-collection (parse-file file-path) potentials (get-potentials coordinates-collection) original-coordinates original-coordinates-collection cores ()]
      (let [max-potential (get-max-potential potentials)
            max-potential-index (get-max-potential-index max-potential potentials coordinates-collection)
            coordinate-with-max-potential (get-coordinate-with-max-potential max-potential-index coordinates-collection)
            original-coordinate-index (get-original-coordinate-index coordinate-with-max-potential original-coordinates)
            core {:index original-coordinate-index :coordinate coordinate-with-max-potential}]

        (case (is-center first-max-potential max-potential coordinate-with-max-potential cores)
          0 (println "the end")
          1 (let [new-coordinates (delete-from-collection coordinate-with-max-potential coordinates-collection)
                new-potentials (get-new-potentials potentials max-potential coordinate-with-max-potential new-coordinates)]
            (println core)
            (recur new-coordinates new-potentials original-coordinates (conj cores coordinate-with-max-potential))
          )
          2 (let [new-coordinates (delete-from-collection coordinate-with-max-potential coordinates-collection)
                new-potentials (delete-from-collection max-potential potentials)]
            (recur new-coordinates new-potentials original-coordinates cores)
          )
          nil
        )
      )
    )
  )
)


(defn -main
  "I don't do a whole lot ... yet."
  [file-path]

  (final-fockin-loop file-path)
)
