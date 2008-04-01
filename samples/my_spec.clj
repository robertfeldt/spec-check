(load-file "spec-check.clj")
(clojure/refer 'spec)

; Collect specifications in normal Clojure functions
(defn spec-myspec []
  (spec "My first spec"
    (is == 1 1)
    (isnt == 1 2)
    (is not= 4 (+ 3 1)) ; should fail!
    (is true? (= 'a 'a))
    (isnt true? (== 1 (count [1]))) ; should fail!
))

; and check it
(check (spec-myspec))
