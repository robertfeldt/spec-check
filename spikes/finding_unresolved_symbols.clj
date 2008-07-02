(defmacro exception-when-evaluating 
  "Return exceptions thrown when evaluating code in arg instead
   of throwing them."
  [arg]
  `(try (eval '~arg) (catch java.lang.Exception e# e#)))

(def unresolved-symbol-error-message-re #"Unable to resolve symbol: (\\w+) in this context")

(defmacro unresolved-symbol
  "Return the name (as a string) of the first unresolved symbol
   in arg (if any)."
  [arg]
  `(let [e# (exception-when-evaluating ~arg)
         m# (re-seq unresolved-symbol-error-message-re (str e#))]
    (second (first m#))))

;; It is not clear how we can get the rest of the unresolved symbols,
;; if there are several, like for example in (do (seq? xs) (seq? ys)).
;; Maybe go through with read instead and see if the symbols we get 
;; from the reader can be resolved in the current closure?