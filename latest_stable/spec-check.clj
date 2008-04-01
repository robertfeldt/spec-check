;; spec-check - 
;;   specification and autochecking of software behavior expectations
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

(defn join
	"Join values to a string by inserting SERPARATOR between them 
	and prefixing with START."
	([values]           (join values " "))
	([values separator] (join values separator ""))
	([values separator start]
	  (if (= 0 (count values))
	    ""
	    (str start (reduce (fn [acc e] (str acc separator (print-str e))) values)))))

(defn trial-outcome-description [outcome]
  (cond
    (= true  outcome) \.
	(= false outcome) \F
	:else             \E))

(defn print-progress [outcome]
	(print (trial-outcome-description outcome))
	(. *out* (flush)))	

(def *failing-trials*)
(def *checking-cases* false)
(def *num-trials*)

(def *inverted-reporting-fns* {
	=    (fn [f as] (str "(not= " (join as) ")"))
	==   (fn [f as] (str "arguments are not the same: " (join as)))
	not= (fn [f as] (str "arguments are the same: " (join as)))
})

(defn exception? [outcome] (and (= (class outcome) (class {})) (= (:outcome outcome) 'exception)))
(defn failure-or-exception? [outcome] (or (not outcome) (exception? outcome)))

(defn show-failing-trial [code outcome fnfn argsfn]
	(if (exception? outcome)
	  (str "EXCEPTION:\n  the code was: " code ", but\n  it raised " (:exception outcome))
	  (let [func (fnfn) 
		    report-fn (or (get *inverted-reporting-fns* func) (fn [f as] (str "arguments was: " (print-str (first as)))))]
		 (str "FAILURE:\n  the code was: " code ", but\n  " (report-fn func (argsfn))))))

(defn log-trial [code outcome fnfn argsfn]
	(set! *num-trials* (+ 1 *num-trials*))
	(if (failure-or-exception? outcome)
		(set! *failing-trials* (conj *failing-trials* (show-failing-trial code outcome fnfn argsfn)))))

(defn report-all-trials [seconds-taken]
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

(defmacro is [& fn-and-args]
  `(let [outcome# (try (~@fn-and-args) (catch java.lang.Exception e# {:outcome 'exception :exception e#}))]
	(*report-progress* outcome#)
	(*log-trial* (print-str '(~@fn-and-args)) outcome# 
	                        (fn [] ~(first fn-and-args)) (fn [] [~@(rest fn-and-args)]))))

(defmacro spec [desc & forms]
  `(fspec nil ~desc ~@forms))

(defmacro fspec [func desc & forms]
	`(do ;(*cache-spec* ~func ~desc ~forms)
	     ~@forms))

(defmacro just-time
  "Evaluates expr and returns the number of seconds it took."
  [expr]
  `(let [start# (. System (nanoTime))]
     ~expr
     (/ (- (. System (nanoTime)) start#) 1000000000.0)))

(defmacro check [& body]
  `(if *checking-cases*
	 (do ~@body) ; This is not the top-level check so just run the spec
	 (binding [*checking-cases* true ; This is the top-level check so setup for check, run spec and then report
	           *num-trials* 0
	           *failing-trials* []
	           *spec-stack* []]
	   (let [timetaken# (just-time (do ~@body))]
	     (*report-all-trials* timetaken#)))))
