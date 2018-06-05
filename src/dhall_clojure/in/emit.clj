(ns dhall-clojure.in.emit
  (:require [clojure.string :as string]
            [dhall-clojure.in.core :refer :all]))


(defprotocol IEmit
  "Interface for records that can emit Clojure forms"
  (emit [e] "Returns the generated Clojure form"))

;; TODO: call extend-protocol on all the forms defined in core
