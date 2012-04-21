(ns sicp.deriv3)

(defn simplify [exp]
  (if (list? exp)
    (let [[op a1 a2] exp]
      (case op
        + (cond
           (and (number? a1) (number? a2)) (+ a1 a2)
           (and (number? a1) (= a1 0)) a2
           (and (number? a2) (= a2 0)) a1
           (= a1 a2) (list '* 2 (simplify a2))
           :else (list '+ (simplify a1) (simplify a2)))
        * (cond
           (and (number? a1) (number? a2)) (* a1 a2)
           (and (number? a1) (= a1 0)) 0
           (and (number? a2) (= a2 0)) 0
           (and (number? a1) (= a1 1)) a2
           (and (number? a2) (= a2 1)) a1
           :else (list '* (simplify a1) (simplify a2)))))
    exp))

(defn deriv-xl [exp var]
  (if (list? exp)
    (let [[op a1 a2] exp]
      (case op
        + (list '+
                (deriv-xl a1 var)
                (deriv-xl a2 var))
        * (list '+
                (list '* a1 (deriv-xl a2 var))
                (list '* (deriv-xl a1 var) a2))))
    (if (= exp var)
      1
      0)))

(defn deriv [exp var]
  (loop [e1 (deriv-xl exp var)]
    (let [e2 (simplify e1)]
      (if (= e1 e2)
	e1
	(recur e2)))))

(comment
  (require '[sicp.deriv3 :as d3])
  (def foo				; a*x*x + b*x + c
       '(+ (* a (* x x))
	   (+ (* b x)
	      c)))
  (d3/deriv foo 'x)			; 2*a*x + b
  )



