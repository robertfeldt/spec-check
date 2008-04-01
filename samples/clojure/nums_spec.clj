; Specs for Clojure Nums
;
(load-file "spec-check.clj")
(clojure/refer 'spec)

(def *max-fixnum* 2147483647)           ; 2^31 - 1
(def *min-fixnum* -2147483648)          ; - 2^31
(def *min-positive-bignum* 2147483648)  ; 2^32
(def *max-negative-bignum* -2147483649) ; - 2^31 - 1

; Helper test functions
(defn fixnum? [n] (= (class 1) (class n)))
(defn bignum? [n] (= (class *min-positive-bignum*) (class n)))

(defn spec-clojure-fixnum-bignum-limits []
  (spec "Limits between Fixnums and Bignums"
    (is fixnum? *max-fixnum*)
    (is fixnum? *min-fixnum*)
    (is bignum? *min-positive-bignum*)
    (is bignum? *max-negative-bignum*)
  )
)

(defn spec-clojure-fixnum-addition []
  (spec "+ with no arguments returns zero"
    (is == 0 (+))
  )
  (spec "Addition with zero changes nothing"
    (is == 0 (+ 0 0))
    (is == 1 (+ 0 1))
    (is == 99 (+ 99 0))
  )
  (spec "Addition of small, positive fixnums should give small, positive fixnums"
    (is == 2 (+ 1 1))
    (is == 3 (+ 2 1))
    (is == 3 (+ 1 2))
    (is == 7 (+ 3 4))
  )
  (spec "Addition of small fixnums (both positive and negative)"
    (is == -1 (+ 0 -1))
    (is == 0  (+ 1 -1))
    (is == -2 (+ -1 -1))
    (is == -3 (+ -2 -1))
    (is == -3 (+ -1 -2))
  )
  (spec "Addition on the limits for fixnums"
    (is == -1 (+ *max-fixnum* *min-fixnum*))
    (is fixnum? (+ *max-fixnum* -1))
    (is bignum? (+ *max-fixnum* 1))
    (is fixnum? (+ *min-fixnum* 1))
    (is bignum? (+ *min-fixnum* -1))
  )
)

(defn spec-suite-clojure-fixnum []
  (spec "Fixnum"
    (spec-clojure-fixnum-addition)
  )
)

(defn spec-clojure-nums []
  (spec "Nums"
    (spec-clojure-fixnum-bignum-limits)
    (spec-suite-clojure-fixnum)
  )
)

(check (spec-clojure-nums))