(spec "SimonThompson - Example 1 from Quickcheck's ExSimple.hs"
  (for-all [x an-int y an-int z an-int]
    (is = 
      (and (== x y) (== y z))
      (and (* 2 x) (+ y z)))))

(spec "ReverseReverse - Example 2 from Quickcheck's ExSimple.hs"
  (for-all [xs (list-of an-integer)]
    (is = xs (reverse (reverse xs)))))