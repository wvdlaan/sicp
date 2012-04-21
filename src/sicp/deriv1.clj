(ns sicp.deriv1)

(defn constant? [exp var]
  (and (not (list? exp))
       (not (= exp var))))

(defn same-var? [exp var]
  (and (not (list? exp))
       (= exp var)))

(defn sum? [exp]
  (and (list? exp)
       (= (first exp) '+)))

(defn make-sum [a1 a2]
  (cond
   (and (number? a1) (number? a2)) (+ a1 a2)
   (and (number? a1) (= a1 0)) a2
   (and (number? a2) (= a1 0)) a1
   :else (list '+ a1 a2)))

(defn product? [exp]
  (and (list? exp)
       (= (first exp) '*)))

(defn make-product [m1 m2]
  (cond
   (and (number? m1) (number? m2)) (* m1 m2)
   (and (number? m1) (= m1 0)) 0
   (and (number? m2) (= m2 0)) 0
   (and (number? m1) (= m1 1)) m2
   (and (number? m2) (= m2 1)) m1
   :else (list '* m1 m2)))

(defn deriv [exp var]
  (cond
   (constant? exp var) 0
   (same-var? exp var) 1
   (sum? exp) (make-sum
	       (deriv (nth exp 1) var)
	       (deriv (nth exp 2) var))
   (product? exp) (make-sum
		   (make-product (nth exp 1)
				 (deriv (nth exp 2) var))
		   (make-product (deriv (nth exp 1) var)
				 (nth exp 2)))))

(comment
  (require '[sicp.deriv1 :as d1])
  (def foo				; a*x*x + b*x + c
       '(+ (* a (* x x))
	   (+ (* b x)
	      c)))
  (d1/deriv foo 'x)			; 2*a*x + b
  )

	       
