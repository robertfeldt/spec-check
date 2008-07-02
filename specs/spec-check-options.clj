(refer 'spec)

(fspec readstr
  (spec "reads literals"
    (is = 10 (readstr "10"))
    (is = 2.1 (readstr "2.1"))
    (is = \a (readstr "\\a"))
  )
  (spec "reads compound expressions"
    (is = '(+ 1 1) (readstr "(+ 1 1)"))
  )
)

(fspec options-and-specfiles
  (spec "returns no options and no files if no arguments"
    (is = [{} []] (options-and-specfiles [])))
  (spec "returns file name if only one file name"
    (is = [{} ["a"]] (options-and-specfiles ["a"])))
  (spec "returns file names if more than one file names"
    (is = [{} ["a" "b"]] (options-and-specfiles ["a" "b"]))
    (is = [{} ["a" "b" "c"]] (options-and-specfiles ["a" "b" "c"])))
  (spec "sets the options map when an option is specified and no file names"
    (is = [{:NumRepetitions 10} []] (options-and-specfiles ["-n" "10"]))
    (is = [{:NumRepetitions 42} []] (options-and-specfiles ["-N" "42"]))
    (is = [{:NumRepetitions 135} []] (options-and-specfiles ["--numreps" "135"]))
    (is = [{:NumRepetitions 2227} []] (options-and-specfiles ["--num-repetitions" "2227"]))
  )
  (spec "later options overrides earlier"
    (is = [{:NumRepetitions 5} []] (options-and-specfiles ["-n" "10" "-N" "5"]))
  )
  (spec "options first and then file names"
    (is = [{:NumRepetitions 5} ["a" "b"]] (options-and-specfiles ["-N" "5" "a" "b"]))
  )
)