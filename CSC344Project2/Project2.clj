(ns csc344project2.core)

(require '[clojure.set :refer [union]])

(defn not-elimination
  [not-prop]
  (if (= (count not-prop) 2)
    (if (= (first not-prop) (symbol "not"))
      (if (seq? (second not-prop))
        (if (= (count (second not-prop)) 2)
          (if (= (first (second not-prop)) (symbol "not"))
            (do
              (println "Because:"not-prop)
              (println "I derived:"(second (second not-prop)))
              (println "by not-elimination")
              (set [(second (second not-prop))])
              ))
          (set nil))
        (set nil))
      (set nil))
    (set nil))
  )

(defn and-elimination
  [and-prop]
  (if (= (first and-prop) (symbol "and"))
    (do
      (println "Because:"and-prop)
      (println "I derived:"(nth and-prop 1))
      (println "and:"(nth and-prop 2))
      (println "by and-elimination")
      (union (set (rest and-prop)) (set [(last and-prop)]))
      )
    (set nil))
  )

(defn modus-ponens
  [if-prop kb]
  (if (contains? kb (second if-prop))
    (do
      (println "Because:"if-prop)
      (println "and:"(second if-prop))
      (println "I derived:"(last if-prop))
      (println "By modus-ponens")
      (set [(nth if-prop 2)])
      )
    (set nil))
  )

(defn modus-tollens
  [if-prop kb]
  (if (contains? kb (concat '(not) [(last if-prop)]))
    (do
      (println "Because:"if-prop)
      (println "and:"(concat '(not) [(last if-prop)]))
      (println "I derived:"(concat '(not) [(second if-prop)]))
      (println "By modus-tollens")
      (set [(concat '(not) [(second if-prop)])])
      )
    (set nil))
  )

(defn elim-step
  [prop kb]
  (if (symbol? prop)
    (do
      (if (= (count (str prop)) 1)
        (if (seq? (nth (filter seq? kb) 0))
          (if (= (nth (nth (filter seq? kb) 0) 1) prop)
            (do
              (println "Because:"(nth (filter seq? kb) 0))
              (println "and:"prop)
              (println "I derived:"(last (nth (filter seq? kb) 0)))
              (println "By modus-ponens")
              (set [(last (nth (filter seq? kb) 0))]))
            (if (seq? (nth (filter seq? kb) 1))
              (if (= (nth (nth (filter seq? kb) 1) 1) prop)
                (do
                  (println "Because:"(nth (filter seq? kb) 1))
                  (println "and:"prop)
                  (println "I derived:"(last (nth (filter seq? kb) 1)))
                  (println "By modus-ponens")
                  (set [(last (nth (filter seq? kb) 1))]))
                (set nil)))
            )
          (set nil))
        (set nil))
      )
    (case (str (first prop))
      "not" (not-elimination prop)
      "and" (and-elimination prop)
      "if"  (if (contains? kb (second prop))
              (modus-ponens prop kb)
              (modus-tollens prop kb))
      (set [prop]))
  ))

(defn fwd-infer
  [prop kb]
  (loop [cur-props #{prop} cur-kb kb]
    (if (not (empty? cur-props))
      (recur (union (elim-step (first cur-props) cur-kb) (rest cur-props))
             (conj cur-kb (first cur-props)))
      cur-kb)))