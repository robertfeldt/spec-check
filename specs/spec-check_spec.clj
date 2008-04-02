(load-file "spec-check.clj")
(clojure/refer 'spec)

(in-ns 'spec) ; we need to be able to access the defn- funcs...

(defn spec-join []
  (fspec join "Return empty string if no args given"
    (is = "" (join []))
    (is = "" (join [] " "))
    (is = "" (join [] " " "start")))

  (fspec join "One string argument"
    (is = "a" (join ["a"]))
    (is = "a" (join ["a"] " "))
    (is = "start:a" (join ["a"] " " "start:")))

  (fspec join "Multiple string arguments"
    (is = "a b" (join ["a" "b"]))
    (is = "ab" (join ["a" "b"] ""))
    (is = "start:a b c" (join ["a" "b" "c"] " " "start:")))

  (fspec join "Multiple arguments"
    (is = "1 2 3" (join [1 2 3]))
    (is = "1a" (join [1 "a"] ""))
    (is = "s:a b" (join ["a" 'b] " " "s:")))

)

(def *e* {:outcome 'exception :exception (new java.lang.Exception)})

(defn spec-trial-outcome-description []
  (fspec trial-outcome-description ""
    (is = \. (trial-outcome-description true))
    (is = \F (trial-outcome-description false))
    (is = \E (trial-outcome-description *e*)))
)

(defn spec-exception? []
  (fspec exception? "it's an exception as long as its a map"
    (is exception? *e*)
    (is exception? {:outcome 'blablabla :exception (new java.lang.Exception)}))

  (fspec exception= "we don't care what type the exception object is"
    (is exception? {:outcome 'exception :exception 1}))
)

(defn spec-failure-or-exception? []
  (fspec failure-or-exception? ""
    (is failure-or-exception? false)
    (is failure-or-exception? *e*)
    (isnt failure-or-exception? true)
    (isnt failure-or-exception? 1))
)

(defn spec-show-failing-trial []
  (fspec show-failing-trial "with exception map as argument"
    (is = "EXCEPTION\n  expectation was: !, but\n  it raised java.lang.Exception" 
	      (show-failing-trial "!" *e* #('dummy) #('dummy) [])))

  (fspec show-failing-trial "with failure as argument"
    (is = "FAILURE\n  expectation was: !, but\n  arguments was: false"
          (show-failing-trial "!" false (fn [] true?) (fn [] [false]) [])))
)

(def *rand-repeats* 100)

(defn spec-rand-int-in []
  (fspec rand-int-in 

	(spec "only generates within range"
      (dotimes [i *rand-repeats*]
        (let [r (rand-int-in -12 27)]
          (is >= r -12)
          (is <= r 27))))

	(spec "only generates within range"
      (for [i (range *rand-repeats*)]
        (let [r (rand-int-in  27)]
          (is >= r -12)
          (is <= r 27))))

  )
)

(defn spec-random-fixnum-seqs

  (fspec random-positive-fixnum

    (spec "Can take a large set of numbers from the infinite collection"
      (is = 2345 (count (take 2345 random-positive-fixnum))))

    (spec "Can take a large set of numbers from the infinite collection"
      (is = 2345 (count (take 2345 random-positive-fixnum))))

    (spec "Always larger than 0 and less than minimum positive Bignum"
      (for-all [n random-positive-fixnum]
        (is > n 0)
        (is <= n *max-fixnum*)))

  )

)

(check
  (spec-join)
  (spec-trial-outcome-description)
  (spec-exception?)
  (spec-failure-or-exception?)
  (spec-show-failing-trial)
  (spec-rand-int-in)
;  (spec-random-fixnum-seqs)
)