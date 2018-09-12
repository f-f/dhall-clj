# dhall-clj

[![Build Status](https://travis-ci.org/f-f/dhall-clj.svg?branch=master)](https://travis-ci.org/f-f/dhall-clj)
[![Coverage Status](https://coveralls.io/repos/github/f-f/dhall-clj/badge.svg?branch=master)](https://coveralls.io/github/f-f/dhall-clj?branch=master)
[![Clojars Project](https://img.shields.io/clojars/v/dhall-clj/dhall-clj.svg)](https://clojars.org/dhall-clj/dhall-clj)
[![cljdoc badge](https://cljdoc.xyz/badge/dhall-clj/dhall-clj)](https://cljdoc.xyz/d/dhall-clj/dhall-clj/CURRENT)

Compiler from Dhall to Clojure.

## Dhall?

[Dhall][dhall] is a functional programming language that is not Turing complete.

*You can think of Dhall as: JSON + functions + types + imports.*

For a Dhall Tutorial, see the [README of the project][dhall], or the [full tutorial][dhall-tutorial].

The purpose of this library is to consume Dhall expressions from Clojure.

Example use cases for Dhall are:
- typed configuration files
- templating
- safe exchange format (since the Language Standard defines an efficient binary encoding)

For more inspiration about possible use cases, see the wiki page
[Using Dhall in Production][dhall-production].

## Dhall version

*Note: this is quite alpha. Things might be broken, so don't hesitate to [open an issue][issues].*

We basically support all of Dhall `v2.0.0`, except the following:
- [#7](../../issues/7): Http imports
- [#4](../../issues/4): Binary serialization/deserialization
- [#5](../../issues/5): Imports semantic integrity checks (hashing)
- [#8](../../issues/8): Caching of hashed imports
- [#12](../../issues/12): The import alternative operator `?`

In addition to this, there's known bugs:
- `emit` really supports a subset of the language, e.g. no `Natural/build`
- some normalization tests are not passing, so some normalization might do funky things

## HOWTO

```clojure
(require [dhall-clojure.core :refer [input input-ast]])

;; We can run Dhall expression with the `input` function

(input "True && False")

;; => false


;; We can even import functions from Dhall, and execute them:

(def build-info (input "λ(major : Natural) → { version = \"${Natural/show major}.0\" }"))
(build-info 1)

;; => {"version" "1.0"}


;; Note that the `input` function calls `eval` on the imported form.
;; This is fine from the security point of view, as Dhall is not Turing Complete.
;; If you have any doubts, please read this document about Dhall's safety guarantees:
;; https://github.com/dhall-lang/dhall-lang/wiki/Safety-guarantees

;; However, if you would not like to emit & eval Clojure forms directly,
;; the function `input-ast` returns the AST constructed of the normalized expression:

(input-ast "1 + 1")

;; => #dhall_clj.ast.NaturalLit {:n 2}
```

## Catching exceptions

Sometimes the evaluation will fail, and we'd like to catch the error condition to handle that.

This library throws only [`ex-info`][ex-info], and the `ex-data` always includes a `:type` key.

This is so we can define a hierarchy of exceptions (which is defined in the [`dhall-clj.fail`][fail]
namespace), useful for catching groups of exceptions of a certain class, using the excellent [ex][ex]
library.

Example:

```clojure
(require '[qbits.ex :as ex])

(ex/try+

  ;; The following will fail to typecheck
  (input "1 + 3.0")

  ;; This will catch the exact exception thrown by the above
  (catch-data :dhall-clj.fail/cant-add data
    (prn :cant-add data))

  ;; But if we didn't specify the exact match, we could still catch this
  ;; with the following, as `:dhall-clj.fail/cant-add` is a descendant of `:dhall-clj.fail/typecheck`
  (catch-data :dhall-clj.fail/typecheck data
    (prn :typecheck data))

  ;; And again if we did not catch the above, we could still catch the "library-wide"
  ;; exception type, as `:dhall-clj.fail/typecheck` is a descendant of `:dhall-clj.fail/dhall-clj`
  (catch-data :dhall-clj.fail/dhall-clj data
    (prn :dhall-clj data))

  ;; This would catch any ex-info not thrown by this library
  (catch ExceptionInfo e
    (prn :this-is-not-us e))

  ;; Catchall
  (catch Exception e))
```

## License

Copyright © 2018 [Fabrizio Ferrai](http://twitter.com/fabferrai)

Distributed under the
[Eclipse Public License](http://www.eclipse.org/legal/epl-v10.html),
the same as Clojure.

[dhall]: https://github.com/dhall-lang/dhall-lang
[dhall-tutorial]: http://hackage.haskell.org/package/dhall-1.17.0/docs/Dhall-Tutorial.html
[dhall-production]: https://github.com/dhall-lang/dhall-lang/wiki/Dhall-in-production
[issues]: https://github.com/f-f/dhall-clj/issues
[ex]: https://github.com/mpenet/ex
[ex-info]: https://clojuredocs.org/clojure.core/ex-info
[fail]: ./src/dhall_clj/fail.clj
