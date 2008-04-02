;; spec-check - 
;;   Specification & checking of software behavior expectations
;;
;; Copyright (c) Robert Feldt. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Common Public License 1.0 (http://opensource.org/licenses/cpl.php)
;; which can be found in the file CPL.TXT at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;;     the terms of this license.
;; You must not remove this notice, or any other, from this software.
;; 
;; version 0.1, 20080401
;;
(in-ns 'spec)
(clojure/refer 'clojure)

(defn- join
	"Join values to a string by inserting SERPARATOR between them 
	and prefixing with START."
	([values]           (join values " "))
	([values separator] (join values separator ""))
	([values separator start]
	  (if (= 0 (count values))
	    ""
	    (str start (reduce (fn [acc e] (str acc separator (print-str e))) values)))))

(defn- exception? [outcome] (= (class outcome) (class {})))
(defn- failure-or-exception? [outcome] (or (not outcome) (exception? outcome)))

(defn trial-outcome-description [outcome]
  (cond
    (= true  outcome)    \.
	(= false outcome)    \F
	(exception? outcome) \E
	:else                \U)) ; U just for debugging purposes

(defn- print-progress [outcome]
	(print (trial-outcome-description outcome))
	(. *out* (flush)))

(def *failing-trials*)
(def *checking-cases* false)
(def *num-trials*)

(def *inverted-reporting-fns* {
	=    (fn [f as] (str "(not= " (join as) ")"))
	==   (fn [f as] (str "arguments are *NOT* the same: " (join as)))
	not= (fn [f as] (str "arguments *ARE* the same: " (join as)))
})

(defn- trial-problem-message [type descstack & messagestrings]
  (if (> (count descstack) 0)
    (str type " in " (join descstack " | ") (join messagestrings ""))
    (str type (join messagestrings ""))))

(defn show-failing-trial [code outcome fnfn argsfn descstack]
  (cond
    (exception? outcome)
	  (trial-problem-message "EXCEPTION" descstack 
		"\n  expectation was: " code ", but\n  it raised " (:exception outcome))
	(= false outcome)
      (let [func (fnfn) 
		    report-fn (or (get *inverted-reporting-fns* func) (fn [f as] (str "arguments was: " (pr-str (first as)))))]
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
(def *spec-stack*)

(defmacro expectation [code & fn-and-args]
  "An expectation that FN applied to ARGS should return true that uses
   CODE to document what was checked."
  `(let [outcome# (try (~@fn-and-args) (catch java.lang.Exception e# {:outcome 'exception :exception e#}))]
	(*report-progress* outcome#)
	(*log-trial* ~code outcome# (fn [] ~(first fn-and-args)) (fn [] [~@(rest fn-and-args)])
	             *spec-stack*)))

(defmacro codestr [exp & parts]
  `(str "(" ~exp ~@(for [p# parts] (str " " (print-str p#))) ")"))

(defmacro spec [desc & body]
  "A spec is described by DESC and defined by expectations and specs in
   BODY."
  `(fspec nil ~desc ~@body))

(defmacro fspec [func desc & body]
  "A function spec (fspec) is a spec associated with the function FUNC. 
   It is is described by DESC and defined by expectations and specs in
   BODY."
	`(binding [*spec-stack* (conj *spec-stack* (str (if (= 'nil '~func) "" (str '~func "/")) ~desc))]
		;(*cache-spec* ~func ~desc ~forms)
	     (do ~@body)))

(defmacro just-time
  "Evaluates expr and returns the number of seconds it took."
  [expr]
  `(let [start# (. System (nanoTime))]
     ~expr
     (/ (- (. System (nanoTime)) start#) 1000000000.0)))

(defmacro check [& body]
  "Check specs and expectations in BODY"
  `(if *checking-cases*
	 (do ~@body) ; This is not the top-level check so just run the spec
	 (binding [*checking-cases* true ; This is the top-level check so setup for check, run spec and then report
	           *num-trials* 0 *failing-trials* [] *spec-stack* []]
	   (let [timetaken# (just-time (do ~@body))]
	     (*report-all-trials* timetaken#)))))

(defmacro for-all [seq-exprs & body]
  "Take *num-rand-trials* values from the seqs in SEQ-EXPRS, assign them to the vars
   and execute the expectations in BODY."
  (let [seqs (take-nth 2 (drop 1 seq-exprs))
	    vars (take-nth 2 seq-exprs)
	    bounded-seqs (map (fn [seq] `(take *num-rand-trials* ~seq)) seqs)
	    bounded-seq-exprs (interleave vars bounded-seqs)]
    `(doall (for [~@bounded-seq-exprs] (do ~@body)))))

;; Utility functions and macros that builds on the core
(defmacro is [& fn-and-args]
  "An expectation that FN applied to ARGS should return true."
  `(expectation (codestr "is" ~@fn-and-args) ~(first fn-and-args) ~@(rest fn-and-args)))

(defmacro isnt [& fn-and-args]
  "An expectation that FN applied to ARGS should return true."
  `(expectation (codestr "isnt" ~@fn-and-args) (complement ~(first fn-and-args)) ~@(rest fn-and-args)))

;; Value generation framework

(def *max-fixnum* 2147483647)           ; 2^31 - 1
(def *min-fixnum* -2147483648)          ; - 2^31
(def *min-positive-bignum* 2147483648)  ; 2^32
(def *max-negative-bignum* -2147483649) ; - 2^31 - 1

(def *num-rand-trials* 100) ; number of generations of random data in for-all expectations

;; Random in in range [min, max] inclusive
(defn rr [min, max] (+ min (rand-int (inc (- max min)))))

(defn seq-from-generator [generator-func]
  (lazy-cons (generator-func) (seq-from-generator generator-func)))

(defn rand-range-seq [min max] (seq-from-generator #(rr min max)))

(def a-small-random-int (rand-range-seq -9 9))
(def a-medium-random-int (rand-range-seq -99 99))
(def a-large-random-int (rand-range-seq -1000 1000))
;(def an-int (weighted-choice small-random-fixnum 50 medium-random-fixnum 45 large-random-fixnum 1))
(def an-int a-medium-random-int)

(def a-random-positive-fixnum (rand-range-seq 1 *max-fixnum*))
(def a-small-random-positive-int (rand-range-seq 1 9))
(def a-random-negative-fixnum (rand-range-seq *min-fixnum* -1))

(def *max-coll-size* 12)

(defn list-of [elementseq] (seq-from-generator #(apply list (take (rand-int *max-coll-size*) elementseq))))