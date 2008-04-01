(load-file "spec-check.clj")
(clojure/refer 'spec)

;; Some small examples (some of them should fail!)
(defn s []
	(spec "test"
		(let [a 1 b 2]
			(is not= a b)
			(is == a (- b 1)))
		(is true? (throw (new java.lang.Exception)))))

(check
  (is == 1 1)
  (is == 1 2)
  (is not= 1 2)
  (is not= 1 1)
  (is true? (every? (fn [v] (== 0 (rem v 2))) [2 4 6]))
  (is true? (every? (fn [v] (== 0 (rem v 2))) [2 4 7]))
  (s)
  (all-are true?
    (== 1 1)
    (not= 2 3)
    (false? true) ; should fail 
  )
)