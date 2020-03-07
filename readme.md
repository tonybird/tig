# Idiot


## Running tests

To compile testing suite
```
mkdir classes
clojure -Rtest \
        -e "(dorun (map compile '(speclj.platform.SpecFailure
                                  speclj.platform.SpecPending)))"
```

to run tests use `clojure -A:test --color [--autotest]`