

(oam:define-project-package "EXPRESSIONS" "EXPR")
(in-package "EXPRESSIONS-SYSTEM")


(defclass expr::object () ())

(defclass multi-function () ((args) (result)))
(defclass expr::function () ((from) (to)))

(defclass observable () ())

(defgeneric dispatch-function ())




(shift date d :month) --> (date:add-months date d)
(shift rate d :%)       --> ((lambda (rate d) (* rate (1+ (/ d 100)))) rate d)