; Implement factorial with plain old recursion
(defn fac-with-recursion [n]
	(if (< n 2)
		1
		(* n (fac-with-recursion (- n 1)))))

; Implement factorial with reduce
(defn fac-with-reduce [n] (reduce * 1 (range 1 (+ n 1))))

(load-file "spec-check.clj")
(clojure/refer 'spec)

(defn spec-factorial [fac]
  (fspec fac "small, positive integer arguments"
    (is = 1       (fac 1))
    (is = 2       (fac 2))
    (is = 6       (fac 3))
    (is = 24      (fac 4))
    (is = 120     (fac 5))
    (is = 720     (fac 6))
    (is = 3628800 (fac 10))))

(check
  (spec-factorial fac-with-recursion)
  (spec-factorial fac-with-reduce))