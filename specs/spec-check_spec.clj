(load-file "spec-check.clj")
(clojure/refer 'spec)

(in-ns 'spec) ; we need to be able to access the defn- funcs...
	
(def *e* {:outcome 'exception :exception (new java.lang.Exception)})

(defn spec-exception? []
  (spec "exception?"
    (is exception? *e*)
    (isnt exception? {:outcome 'blablabla :exception (new java.lang.Exception)})
    (isnt exception? {:outcome 'exception :exception 1})
  )
)

(check
  (spec-exception?)
)