(ns sicp.deriv4)

(defn simplify [op a1 a2]
  (case op
    + (cond
       (and (number? a1) (number? a2)) (+ a1 a2)
       (and (number? a1) (= a1 0)) a2
       (and (number? a2) (= a2 0)) a1
       (= a1 a2) (list '* 2 a1)
       :else (list op a1 a2))
    * (cond
       (and (number? a1) (number? a2)) (* a1 a2)
       (and (number? a1) (= a1 0)) 0
       (and (number? a2) (= a2 0)) 0
       (and (number? a1) (= a1 1)) a2
       (and (number? a2) (= a2 1)) a1
       :else (list op a1 a2))))

(defn deriv [exp var]
  (if (list? exp)
    (let [[op a1 a2] exp
	  da1 (deriv a1 var)
	  da2 (deriv a2 var)]
      (case op
        + (simplify '+ da1 da2)
        * (simplify '+
                    (simplify '* a1 da2)
                    (simplify '* da1 a2))))
    (if (= exp var)
      1
      0)))

(comment
  (require '[sicp.deriv4 :as d4])
  (def foo				; a*x*x + b*x + c
       '(+ (* a (* x x))
	   (+ (* b x)
	      c)))
  (d4/deriv foo 'x)			; 2*a*x + b
  )



