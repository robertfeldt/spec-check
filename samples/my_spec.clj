(spec "My first spec"
  (is == 1 1)
  (isnt == 1 2)
  (is not= 4 (+ 3 1)) ; should fail!
  (is true? (= 'a 'a))
  (isnt true? (== 1 (count [1]))) ; should fail!
)
