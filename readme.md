# Tig

Built by Tony Bird and Chris Burgess in Spring 2020, for a Software Architecture pilot course (COMP 590) at UNC Chapel Hill.

## Instructions
First, [install Clojure](https://clojure.org/guides/getting_started) and clone this repository.

Usage: `clojure -m tig [<top-args>] <command> [<args>]`

Top-level arguments:
- `-r <dir>`   run from the given directory instead of the current one
- `-d <dir>`   store the database in <dir> (default: .tig)

Commands:
   - `help`
   - `init`
   - `hash-object [-w] <file>`
   - `cat-file {-p|-t} <address>`
   - `write-wtree`
   - `commit-tree <tree> -m "<message>" [(-p <parent>)...]`

## Development

### Running tests

To compile testing suite
```
mkdir classes
clojure -Rtest \
        -e "(dorun (map compile '(speclj.platform.SpecFailure
                                  speclj.platform.SpecPending)))"
```

to run tests use `clojure -A:test --color [--autotest]`


### Running clojure format

```
clojure -Sdeps '{:deps {cljfmt {:mvn/version "0.6.4" :paths "src/"}}}' \
  -m cljfmt.main [check|fix]
```