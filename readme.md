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


## Running clojure format

```
clojure -Sdeps '{:deps {cljfmt {:mvn/version "0.6.4" :paths "src/"}}}' \
  -m cljfmt.main [check|fix]
```