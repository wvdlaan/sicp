(ns sicp.deriv2)

(defn constant? [exp var]
  (and (not (list? exp))
       (not (= exp var))))

(defn same-var? [exp var]
  (and (not (list? exp))
       (= exp var)))

(defn function? [exp f]
  (and (list? exp)
       (= (first exp) f)))

(declare simplify)

(defn simple-sum [a1 a2]
  (cond
   (and (number? a1) (number? a2)) (+ a1 a2)
   (and (number? a1) (= a1 0)) a2
   (and (number? a2) (= a2 0)) a1
   (= a1 a2) (list '* 2 (simplify a2))
   :else (list '+ (simplify a1) (simplify a2))))

(defn simple-product [m1 m2]
  (cond
   (and (number? m1) (number? m2)) (* m1 m2)
   (and (number? m1) (= m1 0)) 0
   (and (number? m2) (= m2 0)) 0
   (and (number? m1) (= m1 1)) m2
   (and (number? m2) (= m2 1)) m1
   :else (list '* (simplify m1) (simplify m2))))

(defn simplify [exp]
  (cond
   (function? exp '+) (simple-sum (nth exp 1) (nth exp 2))
   (function? exp '*) (simple-product  (nth exp 1) (nth exp 2))
   :else exp))

(defn deriv-xl [exp var]
  (cond
   (constant? exp var) 0
   (same-var? exp var) 1
   (function? exp '+) (list '+
	       (deriv-xl (nth exp 1) var)
	       (deriv-xl (nth exp 2) var))
   (function? exp '*) (list '+
		   (list '* (nth exp 1)
				 (deriv-xl (nth exp 2) var))
		   (list '* (deriv-xl (nth exp 1) var)
				 (nth exp 2)))))

(defn deriv [exp var]
  (loop [e1 (deriv-xl exp var)]
    (let [e2 (simplify e1)]
      (if (= e1 e2)
	e1
	(recur e2)))))

(comment
  (require '[sicp.deriv2 :as d2])
  (def foo				; a*x*x + b*x + c
       '(+ (* a (* x x))
	   (+ (* b x)
	      c)))
  (d2/deriv foo 'x)			; 2*a*x + b
  )

	       
