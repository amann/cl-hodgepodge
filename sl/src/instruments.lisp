


(defclass financial-instrument () ())

(defclass option ()
  (underlying
   strike-price
   units
   option-type
   option-style
   expiry-date))