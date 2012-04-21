(ns sicp.deriv5)

(def deriv-rules
     '(
       ( (dd (?c c) (? v)) 0 )		; 5 to x => 0
       ( (dd (?v v) (? v)) 1 )		; x to x => 1
       ( (dd (?v u) (? v)) 0 )		; y to x => 0

       ;; e1+e2 to x => e1 to x + e2 to x
       ( (dd (+ (? x1) (? x2)) (? v))
	 (+ (dd (! x1) (! v))
	    (dd (! x2) (! v)))) 

       ;; e1*e2 to x => (e1 to x) * e2 + (e2 to x) * e1
       ( (dd (* (? x1) (? x2)) (? v))
	 (+ (* (dd (! x1) (! v)) (! x2))
	    (* (dd (! x2) (! v)) (! x1))))

       ;; x^3 to x => 3x^2
       ( (dd (** (? x) (?c n)) (? v))
	 (* (* (! n)
	       (** (! x) (- (! n) 1)))
	    (dd (! x) (! v))))
       ))

(def algebra-rules
     '(
       ( ((? op) (?c e1) (?c e2))
	 (eval ((! op) (! e1) (! e2)))) ; 1+1 => 2

       ( ((? op) (? e1) (?c e2))
	 ((! op) (! e2) (! e1)))	; x*2 => 2*x

       ( (+ 0 (? e)) (! e))		; 0+x => x
       ( (* 1 (? e)) (! e))		; 1x => x
       ( (* 0 (? e)) 0)			; 0x => 0

       ( (+ (? e1) (+ (?c e2) (? e3)))
	 (+ (! e2) (+ (! e1) (! e3))))  ; e1+(2+e3) => 2+(e1+e3)

       ( (+ ( + (? e1) (? e2)) (? e3))
	 (+ (! e1) (+ (! e2) (! e3))))	; (e1+e2)+e3 => e1+(e2+e3)

       ( (+ (* (?c c) (? a)) (* (?c d) (? a)))
	 (* (! (+ c d) (! a))))		; 2x + 3x => (2+3)x

       ( (* (? c) (+ (? d) (? e)))
	 (+ (* (! c) (! d)) (* (! c) (! e)))) ; c(d+e) => cd + ce

       ( (+ (? e) (? e))
	 (* 2 (! e)))			; x+x => 2x
       ))

(defn extend-dict [dict pat exp]
  (if-let [entry (dict pat)]
    (if (= entry exp)
      dict
      'failed)
    (assoc dict pat exp)))

(defn match [pat exp dict]
  (cond
   (= dict 'failed) 'failed
   (seq? pat) (case
                  (first pat)
                ?c (if (number? exp)
		     (extend-dict dict (second pat) exp)
		     'failed)
                ?v (if (symbol? exp)
		     (extend-dict dict (second pat) exp)
		     'failed)
                ? (extend-dict dict (second pat) exp)
                (if (seq? exp)
                  (match
                   (next pat)
                   (next exp)
                   (match
                    (first pat)
                    (first exp)
                    dict))
                  'failed))
   (= pat exp) dict
   :else 'failed))

(defn instantiate [skeleton dict]
  (if (seq? skeleton)
    (case (first skeleton)
      ! (dict (second skeleton))
      eval (eval (instantiate (second skeleton) dict))
      (cons (instantiate (first skeleton) dict)
            (instantiate (next skeleton) dict)))
    skeleton))

(defn apply-rule
  "Apply 1 rewrite rule to 1 sub-expression"
  [sub-exp rule]
  (let [pat (first rule)
	ske (second rule)
	dict (match pat sub-exp {})]
    (if (= dict 'failed)
      sub-exp
      (instantiate ske dict))))

(defn walk-exp
  "Apply 1 rewrite rule to all sub-expressions in a expression"
  [sub-exp rule]
  (apply-rule
   (if (seq? sub-exp)
     (apply list (map #(walk-exp % rule) sub-exp))
     sub-exp)
   rule))

(defn apply-rules
  "Continue to apply all rewrite rules to exp until it stops changing"
  [exp1 rules]
  (let [exp2 (reduce walk-exp exp1 rules)]
    (if (= exp1 exp2)
      exp1
      (recur exp2 rules))))

(defn deriv [exp]
  (apply-rules exp deriv-rules)) 

(defn algebra [exp]
  (apply-rules exp algebra-rules))

(defmacro dfn [params body]
  (let [v (first params)
        a (list 'dd body v)
        d (algebra (deriv a))]
    (list 'fn params d)))

(comment
  (use 'sicp.deriv5)
  (def a '(dd (+ (* x x) (* x 5)) x))	 ; derive function x^2+5x to x
  (reduce walk-exp a deriv-rules)	 ; do 1 deriv-rewrite
  (deriv a)				 ; do all deriv-rewrites
  (algebra (deriv a))                    ; do all deriv & algebra rewrites
  (def f' (dfn [x] (+ (* x x) (* x 5)))) ; define the derivative function
  (f' 2) ;=> 9
  )

