(refer 'spec)

(defn in-int-range? [min max value]
  (and (>= value min) (<= value max)))

(spec "Values are generated with uniform distribution"
  (spec "in range 0...10"
    (are-allways? in-int-range? 0 10 (gensize a-positive-fixnum 10)))
  (spec "in range 0...100"
    (are-allways? in-int-range? 0 99 (gensize a-positive-fixnum 100)))
)
