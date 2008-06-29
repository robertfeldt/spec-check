(load-file "spec-check.clj")
(clojure/refer 'spec)

(in-ns 'spec) ; we need to be able to access the defn- functions to test them

(defn spec-join []
  {:specifies join}
  (spec "Return empty string if no values given"
    (is = "" (join []))
    (is = "" (join [] " "))
    (is = "" (join [] " " "start")))

  (spec "One string argument"
    (is = "a" (join ["a"]))
    (is = "a" (join ["a"] " "))
    (is = "start:a" (join ["a"] " " "start:")))

  (spec "Multiple string arguments"
    (is = "a b" (join ["a" "b"]))
    (is = "ab" (join ["a" "b"] ""))
    (is = "start:a b c" (join ["a" "b" "c"] " " "start:")))

  (spec "Multiple arguments"
    (is = "1 2 3" (join [1 2 3]))
    (is = "1a" (join [1 "a"] ""))
    (is = "s:a b" (join ["a" 'b] " " "s:")))
)

(def *e* {:outcome 'exception :exception (new java.lang.Exception)})

(defn spec-trial-outcome-description []
  {:specifies trial-outcome-description}
  (spec
    (is = \. (trial-outcome-description true))
    (is = \F (trial-outcome-description false))
    (is = \E (trial-outcome-description *e*)))
)

(defn spec-exception? []
  {:specifies exception?}
  (spec "it's an exception as long as its a map"
    (is exception? *e*)
    (is exception? {:outcome 'blablabla :exception (new java.lang.Exception)}))

  (spec "we don't care what type the exception object is"
    (is exception? {:outcome 'exception :exception 1}))
)

(defn spec-failure-or-exception? []
  {:specifies join}
  (fspec failure-or-exception? ""
    (is failure-or-exception? false)
    (is failure-or-exception? *e*)
    (isnt failure-or-exception? true)
    (isnt failure-or-exception? 1))
)

(defspec spec-show-failing-trial []
  (spec "with exception map as argument"
    (is = "EXCEPTION\n  expectation was: !, but\n  it raised java.lang.Exception" 
	      (show-failing-trial "!" *e* #('dummy) #('dummy) [])))

  (spec "with failure as argument"
    (is = "FAILURE\n  expectation was: !, but\n  arguments was: false"
          (show-failing-trial "!" false (fn [] true?) (fn [] [false]) [])))
)

(defn spec-randint []
  (spec "Like rand-int but can also return a random number in a specified range from min (inclusive) to max (exclusive)."
    (spec "work like Clojure's rand-int when given a single argument"
        (let [r (randint 42)]
          (is >= r 0)
          (is < r 42)))
    (spec "work like Clojure's rand-int when given a single argument"
        (let [r (randint -12 27)]
          (is >= r -12)
          (is < r 27)))))

(defn spec-integer-generator [gen min max]
  (spec "Generates values in the right range inclusive"
      (let [v (gen)]
        (is >= v min)
        (is <= v max)))
  (spec "Generated values are not the same all the time"
    (let [genlist (list-of gen 100)]
	  (isnt = 1 (count (distinct genlist))))))
  
(defn spec-an-int []
  (fspec an-int ""
    (spec-integer-generator an-int -1000 1000)))

(defn spec-a-pos-int []
  (fspec a-pos-int ""
    (spec-integer-generator a-pos-int 1 1000)))

(defn spec-vector-of []
  (fspec vector-of ""
    (spec "Generates a vector"
        (is = (class []) (class (vector-of an-int))))
    (spec "Generates vectors of the right length"
      (let [len (randint 142)]
        (is = len (count (vector-of an-int len)))))))

(defn spec-list-of []
  (fspec list-of ""
    (spec "Generates a list"
        (is = (class (list 'a)) (class (list-of an-int (randint 1 142)))))
    (spec "Generates vectors of the right length"
      (let [len (randint 142)]
        (is = len (count (list-of an-int len)))))))

(def *repeats* 50)

(check
  (spec-join)
  (spec-trial-outcome-description)
  (spec-exception?)
  (spec-failure-or-exception?)
  (spec-show-failing-trial)
  (dotimes i *repeats* 
    (spec-randint)
    (spec-an-int)
    (spec-a-pos-int)
    (spec-vector-of)
    (spec-list-of))
)