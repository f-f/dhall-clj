# dhall-clj

[![Build Status](https://travis-ci.org/f-f/dhall-clj.svg?branch=master)](https://travis-ci.org/f-f/dhall-clj)
[![Build status](https://ci.appveyor.com/api/projects/status/l7ntvofkhy9iwh28/branch/master?svg=true)](https://ci.appveyor.com/project/f-f/dhall-clj/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/f-f/dhall-clj/badge.svg?branch=master)](https://coveralls.io/github/f-f/dhall-clj?branch=master)
[![Clojars Project](https://img.shields.io/clojars/v/dhall-clj/dhall-clj.svg)](https://clojars.org/dhall-clj/dhall-clj)
[![cljdoc badge](https://cljdoc.xyz/badge/dhall-clj/dhall-clj)](https://cljdoc.xyz/d/dhall-clj/dhall-clj/CURRENT)

## Dhall + Clojure = 😍

This package allows you to compile [Dhall][dhall] expressions to Clojure expressions.

And this is a very useful thing! Why is it so?

Some use cases for Dhall are:
- **typed configurations**: you need your configuration/data to have a precise type
- **templating**: template things in a sane way (you definitely don't want a Turing complete
  templating language..)
- send code on the wire in **safe** and **efficient** way: it's safe because the language is not
  Turing complete, and it's efficient because Dhall defines a binary encoding to serialize and 
  deserialize expressions.

If this sounds useful to you too, then read on! For more inspiration about possible use cases, you can
take a look at the wiki page ["Using Dhall in Production"][dhall-production] tracking the usage of
Dhall in the wild.

### Sounds nice! But where can I read more about this Dhall?

First of all, a quick definition: [Dhall][dhall] is a functional programming language that is not
Turing complete, geared towards practical usage as a configuration language.

*You can think of Dhall as: JSON + functions + types + imports.*

For a more detailed pitch and a live demo, the [Dhall website][dhall] is the best place to start.  
If you'd like a Dhall tutorial instead, see the [README of the project][dhall-repo], 
or the [full tutorial][dhall-tutorial].

### Dhall Language Version and compliance to the Standard

*Note: this is quite alpha. Things might be broken, so don't hesitate to [open an issue][issues].*

Dhall has a versioned [language specification][dhall-repo], so it's useful to know which version we are using.

This library implements `v5.0.0` of Dhall, except for http imports: that is, you cannot import things via
http URLs yet.

Moreover, the translation to Clojure data structures is somewhat partial for now, so be extra careful when
checking that the data you get from Dhall is what you expected.

For more information on the aspects in which this implementation is not compliant with the Standard, see
the issues labeled with ["standard compliance"][standard-compliance-issues].

## HOWTO

```clojure
(require '[dhall-clj.core :refer [input input-ast]])

;; We can run compile and run Dhall expression in Clojure with the `input` function.
;; Note that the result of the evaluation is a Clojure value

(input "True && False")
;; => false


;; We can even import functions from Dhall..
;; (the following compiles a Dhall function into a Clojure function)

(def build-info (input "λ(major : Natural) → { version = \"${Natural/show major}.0\" }"))

;; ..and run them in Clojure!

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

## HOWTO - Advanced usage

There are cases in which you'd need to run single compiler phases, e.g. if you wish to
serialize expressions.

If we follow the lifetime of a Dhall expression being compiled, we'll see the following
phases happening:
- **parsing**: here we go from "text" to Abstract Syntax Tree (AST), that is going to be the
  representation we'll use for the Dhall expression in all the next phases.  
  That is, all the following operations are defined as manipulations on the AST.
- **import resolution**: a Dhall expression might contain local and remote imports (files,
  environment variables, http URLs), so we should try to resolve these imports and incorporate
  them into the expression before going forward (as we cannot verify the type of the expression
  if we don't know what the import will contain).  
  So this phase will transform the AST containing imports into a import-free AST
- **typechecking**: once we have the full expression, we have to verify that the types are correct,
  e.g. we'll check that if a function takes an `Integer` it's not being passed a `Text`, and so on.
- **execution/normalization**: this is the final phase of the compilation, and effectively "runs"
  the computation. We do this by "reducing to a normal form" (all Dhall expression have a "normal form",
  and this means that all expressions *always* terminate), where we basically apply all the functions
  we can apply.
- (optionally) **emit Clojure**: this phase is not strictly included in the compilation, but it will
  transform the Dhall AST from the normalization into Clojure datastructures. E.g. Dhall Lists will
  become Clojure vectors, Records will become Clojure maps, Dhall functions will become Clojure functions,
  etc.

Let's see how to use this knowledge to do interesting things (like serializing Dhall expressions to binary):
```clojure
;; There are different namespaces for every compilation phase:
;; - "parsing"
(require '[dhall-clj.parse :refer [parse expr]])

;; - "import resolution" (the `state` ns is for the in-memory cache for imports)
(require '[dhall-clj.import :refer [resolve-imports]])
(require '[dhall-clj.state :as s])

;; - "typechecking"
(require '[dhall-clj.typecheck :refer [typecheck]])

;; - "normalization"
(require '[dhall-clj.beta-normalize :refer [beta-normalize]])

;; - "emit Clojure"
(require '[dhall-clj.emit :refer [emit]])

;; See the implementation of `dhall-clj.core/input` to see how to compose them together properly
;; But we can say that a basic re-implementation of `input` would be:

(defn my-input [dhall-text]
   (let [parse-tree (parse dhall-code)
         ast        (expr parse-tree)
         ast        (resolve-imports ast (s/new))
         type       (typecheck ast {})
         ast        (beta-normalize ast)
         clj-sexp   (emit ast)]
     (eval clj-sexp)))


;; Let's say we now want to serialize a Dhall expression to binary data.
;; We'll take the `build-info` function we used in the previous section to demonstrate this

(def dhall-source "λ(major : Natural) → { version = \"${Natural/show major}.0\" }")

(require '[dhall-clj.binary :refer [encode decode]])

(def serialized (-> dhall-source input-ast encode))

;; Now `serialized` will contain the serialized expression in a ByteArray.
;; You can now send this around, and when you want to _deserialize_ an encoded
;; expression you can run `decode` on it:

(def build-info (-> serialized decode emit eval))

;; ...and you can now run this code as well!

(build-info 1)

;; => {"version" "1.0"}
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

[dhall]: https://dhall-lang.org
[dhall-repo]: https://github.com/dhall-lang/dhall-lang
[dhall-tutorial]: http://hackage.haskell.org/package/dhall-1.20.1/docs/Dhall-Tutorial.html
[dhall-production]: https://github.com/dhall-lang/dhall-lang/wiki/Dhall-in-production
[issues]: https://github.com/f-f/dhall-clj/issues
[ex]: https://github.com/mpenet/ex
[ex-info]: https://clojuredocs.org/clojure.core/ex-info
[fail]: ./src/dhall_clj/fail.clj
[standard-compliance-issues]: https://github.com/f-f/dhall-clj/issues?q=is%3Aissue+is%3Aopen+label%3A%22standard+compliance%22
