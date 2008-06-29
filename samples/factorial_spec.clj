; Implement factorial with plain old recursion
(defn fac-with-recursion [n]
  (if (< n 2)
      1
      (* n (fac-with-recursion (- n 1)))))

; Implement factorial with reduce
(defn fac-with-reduce [n] (reduce * 1 (range 1 (+ n 1))))

(defn spec-factorial [fac]
  (spec "for small, positive integer arguments"
    (is = 1       (fac 1))
    (is = 2       (fac 2))
    (is = 6       (fac 3))
    (is = 24      (fac 4))
    (is = 120     (fac 5))
    (is = 720     (fac 6))
    (is = 3628800 (fac 10))))

(spec "Different factorial implementations"
  (spec "fac-with-recursion"
    (spec-factorial fac-with-recursion))
  (spec "fac-with-reduce"
    (spec-factorial fac-with-reduce))
;  (spec "always give the same value"
;    (for-all [n a-small-positive-integer]
;      (is = (fac-with-recursion n) (fac-with-reduce n))))
)
