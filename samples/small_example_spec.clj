;; Some small examples (some of them should fail!)
(defn s []
	(spec "test"
		(let [a 1 b 2]
			(is not= a b)
			(is == a (- b 1)))
		(is true? (throw (new java.lang.Exception)))))

(spec "small examples, some should fail"
  (is == 1 1)
  (is == 1 2)
  (is not= 1 2)
  (is not= 1 1)
  (is true? (every? (fn [v] (== 0 (rem v 2))) [2 4 6]))
  (is true? (every? (fn [v] (== 0 (rem v 2))) [2 4 7]))
  (s)
)