;; spec-check - 
;;   Specification & checking of software behavior
;;
;; Copyright (c) Robert Feldt. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;; which can be found in the file CPL.TXT at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;;     the terms of this license.
;; You must not remove this notice, or any other, from this software.
;; 
;; version 0.2, 20080617
;;
(clojure/in-ns 'spec)
(clojure/refer 'clojure)

(defn- join
  "Join values to a string by inserting separator between them 
   and prefixing with start. Returns an empty string when values is
   empty. Uses a single space as the default separator and an empty
   string as the default start."
  ([values]           (join values " "))
  ([values separator] (join values separator ""))
  ([values separator start]
    (if (= 0 (count values))
        ""
        (str start (reduce (fn [acc e] (str acc separator (print-str e))) values)))))

(defn- exception? [outcome] (= (class outcome) (class {})))
(defn- failure-or-exception? [outcome] (or (not outcome) (exception? outcome)))

(defn- trial-outcome-description [outcome]
  (cond
    (= true  outcome)    \.
    (= false outcome)    \F
    (exception? outcome) \E
    :else                \U)) ; U for debugging purposes

(defn- print-progress [outcome]
  (print (trial-outcome-description outcome))
  (. *out* (flush)))

(def *failing-trials* [])
(def *checking-cases* false)
(def *num-trials* 0)
(def *num-repetitions* 100)

(def *inverted-reporting-fns* {
  =    (fn [f args] (str "(not= " (join args) ")"))
  ==   (fn [f args] (str "arguments are *NOT* the same: " (join args)))
  not= (fn [f args] (str "arguments *ARE* the same: " (join args)))
  <=   (fn [f args] (str "(> " (join args) ")"))
  >=   (fn [f args] (str "(< " (join args) ")"))
})

(defn default-reporting-fn [f args]
  (str "arguments was: " (pr-str (first args))))

(defn- trial-problem-message [type descstack & messagestrings]
  (if (> (count descstack) 0)
    (str type " in " (join descstack " | ") (join messagestrings ""))
    (str type (join messagestrings ""))))

(defn- show-failing-trial [code outcome fnfn argsfn descstack]
  (cond
    (exception? outcome)
	  (trial-problem-message "EXCEPTION" descstack 
		"\n  expectation was: " code ", but\n  it raised " (:exception outcome))
	(= false outcome)
      (let [func (fnfn) 
		    report-fn (or (get *inverted-reporting-fns* func) default-reporting-fn)]
		 (trial-problem-message "FAILURE" descstack 
			"\n  expectation was: " code ", but\n  " (report-fn func (argsfn))))))

(defn- log-trial [code outcome fnfn argsfn descstack]
	(set! *num-trials* (+ 1 *num-trials*))
	(if (failure-or-exception? outcome)
		(set! *failing-trials* (conj *failing-trials* (show-failing-trial code outcome fnfn argsfn descstack)))))

(defn- report-all-trials [seconds-taken]
  (println (join *failing-trials* "\n" "\n")) (newline)
  (println "Finished in" seconds-taken "seconds") (newline)
  (let [nfailures (count (filter (fn [s] (= \F (nth s 0))) *failing-trials*))
        nexceptions (- (count *failing-trials*) nfailures)]
    (println *num-trials* "expectations checked," nfailures "failed," nexceptions "raised exceptions")))

(def *report-progress* print-progress)
(def *log-trial* log-trial)
(def *report-all-trials* report-all-trials)
(def *spec-stack* [])

(defmacro run-expectation [code & fn-and-args]
  "An expectation that FN applied to ARGS should return true that uses
   CODE to document what was checked."
  `(let [outcome# (try (~@fn-and-args) (catch java.lang.Exception e# {:outcome 'exception :exception e#}))]
	(*report-progress* outcome#)
	(*log-trial* ~code outcome# (fn [] ~(first fn-and-args)) (fn [] [~@(rest fn-and-args)])
	             *spec-stack*)))

(defmacro codestr [exp & parts]
  `(str "(" ~exp ~@(for [p# parts] (str " " (print-str p#))) ")"))

(defmacro just-time
  "Evaluates expr and returns the number of seconds it took."
  [expr]
  `(let [start# (.nanoTime System)]
     ~expr
     (/ (- (.nanoTime System) start#) 1000000000.0)))

(defmacro check [& body]
  "Check specs and expectations in BODY"
  `(if *checking-cases*
	 (do ~@body) ; This is not the top-level check so just run the spec
	 (binding [*checking-cases* true ; This is the top-level check so setup for check, run spec and then report
	           *num-trials* 0 *failing-trials* [] *spec-stack* []
	           *num-repetitions* 100]
	   (*report-all-trials* (just-time (do ~@body))))))

;; The externally visible interface is the functions:
;;   spec
;;   fspec
;;   is
;;   isnt

(defmacro spec [description & body]
  "A spec described by description and defined by expectations and 
   nested specs in body."
  `(fspec nil ~description ~@body))

(defmacro fspec [func description & body]
  "A function spec (fspec) is a spec associated with the function func. 
   It is is described by description and defined by expectations and 
   specs in body."
	`(binding [*spec-stack* (conj *spec-stack* (str (if (= 'nil '~func) "" (str '~func ": ")) ~description))]
	     (do ~@body)))

;; Utility functions and macros that builds on the core
(defmacro is [& fn-and-args]
  "Run the expectation that fn applied to args should return true."
  `(run-expectation (codestr "is" ~@fn-and-args) ~(first fn-and-args) ~@(rest fn-and-args)))

(defmacro isnt [& fn-and-args]
  "Run the expectation that FN applied to ARGS should return false."
  `(run-expectation (codestr "isnt" ~@fn-and-args) (complement ~(first fn-and-args)) ~@(rest fn-and-args)))

(defmacro are-allways? [& fn-and-args]
  "Repeatedly run the expectation in fn-and-args."
  ;; We must loop over the is since we want it to be counted as *num-repetitions*
  ;; assertion checks.
  `(doall (for [i# (range 0 *num-repetitions*)] 
            (is ~@fn-and-args))))

(defmacro for-all [generator-exprs & body]
  "Take *num-rand-trials* values from the seqs in SEQ-EXPRS, assign them to the vars
   and execute the expectations in BODY."
  (let [generators (take-nth 2 (drop 1 generator-exprs))
	    variables (take-nth 2 generator-exprs)
	    bounded-seqs (map (fn [seq] `(~seq)) generators)
	    bounded-seq-exprs (interleave variables bounded-seqs)]
    `(doall (for [~@bounded-seq-exprs] (do ~@body)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Value generation framework
;;

;; A generator is a map with :type returning the generator type
;; that is used for dispatch.
(defmulti setup-generation :type)
(defmulti gen :type)
(defmulti gensize (fn [generator size] (:type generator)))
(defmulti generate (fn [generator size index] (:type generator)))

;; Default is that there is no setup, i.e. the generator has no state.
(defmethod setup-generation :default [gen] nil)

(def *max-fixnum* 2147483647)           ; 2^31 - 1

(defn make-random-gen
  "A RandomGenerator is just a generator function that given
   a size and an index returns a random value of that size.
   We can specify the default maxsize to use when a value of
   unspecified size is to be generated, if not a default maxsize
   of 42 is used."
  ([genfn]         (make-random-gen 42 genfn))
  ([maxsize genfn] {:type :RandomGenerator, 
                    :generatorfn genfn, :maxsize maxsize}))

;;Default gen function is to use a random size (within maxsize).
(defmethod gen :RandomGenerator
  [gen] (gensize gen (rand-int (:maxsize gen))))

(defmethod gensize :RandomGenerator
  [gen size] (generate gen size (rand-int *max-fixnum*)))

(defmethod generate :RandomGenerator
  [gen size index] ((:generatorfn gen) size index))

(def a-positive-fixnum
  (make-random-gen (fn [size index] (rem index size))))

(def a-char
  (make-random-gen 256 (fn [size index] 
    (char (generate a-positive-fixnum (rem size 256) index)))))

;;(defn vector-of
;;  ([gen]      (vector-of gen (randint *collection-size*)))
;;  ([gen size] (loop [count size res []] (if (= 0 count) res (recur (- count 1) (conj res (gen)))))))

;;(defn list-of
;;  ([gen]      (list-of gen (randint *collection-size*)))
;;  ([gen size] (apply list (vector-of gen size))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check spec files given in glob patterns
;;

;; Check files matching the given glob patterns
(defn check-files [globpatterns]
  (check
    (doall (for [globp globpatterns] 
      (do (println globp)
          (load-file globp)
          (newline))))))

;; Run clj spec files specified on the command line
(if (> (count *command-line-args*) 0) 
    (check-files *command-line-args*))