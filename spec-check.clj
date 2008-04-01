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

(defn- trial-outcome-description [outcome]
  (cond
    (= true  outcome) \.
	(= false outcome) \F
	:else             \E))

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

(defn- exception? [outcome]
  (and (= (class outcome) (class {})) 
       (= (:outcome outcome) 'exception)
       (= (class (:exception outcome)) java.lang.Exception)))
(defn- failure-or-exception? [outcome] (or (not outcome) (exception? outcome)))

(defn- show-failing-trial [code outcome fnfn argsfn]
	(if (exception? outcome)
	  (str "EXCEPTION:\n  expectation was: " code ", but\n  it raised " (:exception outcome))
	  (let [func (fnfn) 
		    report-fn (or (get *inverted-reporting-fns* func) (fn [f as] (str "arguments was: " (print-str (first as)))))]
		 (str "FAILURE:\n  expectation was: " code ", but\n  " (report-fn func (argsfn))))))

(defn- log-trial [code outcome fnfn argsfn]
	(set! *num-trials* (+ 1 *num-trials*))
	(if (failure-or-exception? outcome)
		(set! *failing-trials* (conj *failing-trials* (show-failing-trial code outcome fnfn argsfn)))))

(defn- report-all-trials [seconds-taken]
  (newline)
  (println (join *failing-trials* "\n"))
  (newline)
  (println "Finished in" seconds-taken "seconds")
  (newline)
  (println *num-trials* "expectations checked," (count *failing-trials*) "failed"))

(def *report-progress* print-progress)
(def *log-trial* log-trial)
(def *report-all-trials* report-all-trials)
(def *spec-stack*)

(defmacro expectation [code & fn-and-args]
  "An expectation that FN applied to ARGS should return true that uses
   CODE to document what was checked."
  `(let [outcome# (try (~@fn-and-args) (catch java.lang.Exception e# {:outcome 'exception :exception e#}))]
	(*report-progress* outcome#)
	(*log-trial* ~code outcome# (fn [] ~(first fn-and-args)) (fn [] [~@(rest fn-and-args)]))))

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
	`(do ;(*cache-spec* ~func ~desc ~forms)
	     ~@body))

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
	           *num-trials* 0
	           *failing-trials* []
	           *spec-stack* []]
	   (let [timetaken# (just-time (do ~@body))]
	     (*report-all-trials* timetaken#)))))

;; Utility functions and macros that builds on the core
(defmacro is [& fn-and-args]
  "An expectation that FN applied to ARGS should return true."
  `(expectation (codestr "is" ~@fn-and-args) ~(first fn-and-args) ~@(rest fn-and-args)))

(defmacro isnt [& fn-and-args]
  "An expectation that FN applied to ARGS should return true."
  `(expectation (codestr "isnt" ~@fn-and-args) (complement ~(first fn-and-args)) ~@(rest fn-and-args)))

(defmacro all-are [checkfn & forms]
  "State multiple expectations in one go, namely that 
   CHECKFN applied to each form in FORMS should return true."
  `(for [f# [~@forms]] `(is ~~checkfn ~f#)))