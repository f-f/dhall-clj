(ns dhall-clj.in.emit
  (:require [clojure.string :as string]
            [dhall-clj.ast :refer :all]))


(defprotocol IEmit
  "Interface for records that can emit Clojure forms"
  (emit [e] "Returns the generated Clojure form"))

;; TODO: call extend-protocol on all the forms defined in core
