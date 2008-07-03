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

(fspec parse-command-line-args
  (spec "returns no options and no files if no arguments"
    (is = [{} []] (parse-command-line-args [])))
  (spec "returns file name if only one file name"
    (is = [{} ["a"]] (parse-command-line-args ["a"])))
  (spec "returns file names if more than one file names"
    (is = [{} ["a" "b"]] (parse-command-line-args ["a" "b"]))
    (is = [{} ["a" "b" "c"]] (parse-command-line-args ["a" "b" "c"])))
  (spec "sets the options map when an option is specified and no file names"
    (is = [{:NumRepetitions 10} []] (parse-command-line-args ["-n" "10"]))
    (is = [{:NumRepetitions 42} []] (parse-command-line-args ["-N" "42"]))
    (is = [{:NumRepetitions 135} []] (parse-command-line-args ["--numreps" "135"]))
    (is = [{:NumRepetitions 2227} []] (parse-command-line-args ["--num-repetitions" "2227"]))
    (is = [{:Verbose true} []] (parse-command-line-args ["-v"]))
    (is = [{:Verbose true} []] (parse-command-line-args ["-V"]))
    (is = [{:Verbose true} []] (parse-command-line-args ["--verbose"]))
  )
  (spec "later options overrides earlier"
    (is = [{:NumRepetitions 5} []] (parse-command-line-args ["-n" "10" "-N" "5"]))
  )
  (spec "option first and then file names"
    (is = [{:NumRepetitions 5} ["a" "b"]] (parse-command-line-args ["-N" "5" "a" "b"]))
    (is = [{:Verbose true} ["1" "3"]] (parse-command-line-args ["-v" "1" "3"]))
  )
  (spec "option in between file names"
    (is = [{:NumRepetitions 2} ["a" "b" "c"]] (parse-command-line-args ["a" "-N" "2" "b" "c"]))
    (is = [{:NumRepetitions 6} ["a" "b" "c"]] (parse-command-line-args ["a" "b"  "-N" "6" "c"]))
    (is = [{:NumRepetitions 6} ["a" "b" "c"]] (parse-command-line-args ["a" "b"  "-N" "6" "c"]))
    (is = [{:Verbose true} ["a" "b" "c"]] (parse-command-line-args ["a" "-V" "b" "c"]))
    (is = [{:Verbose true} ["a" "b" "c"]] (parse-command-line-args ["a" "b" "-v" "c"]))
  )
  (spec "several options before file names"
    (is = [{:Verbose true :NumRepetitions 3} ["a" "b"]] 
          (parse-command-line-args ["-v" "-n" "3" "a" "b"]))
  )
  (spec "several options mixed in with file names"
    (is = [{:Verbose true :NumRepetitions 13} ["a" "b" "c" "d"]] 
          (parse-command-line-args ["a" "-n" "13" "b" "c" "-V" "d"]))
  )
)