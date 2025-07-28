(define (make-exponentiation base expon) 
    (cond ((=number? base 1) 1)
          ((=number? base 0) 0)
          ((=number? expon 1) base)
          ((=number? expon 0) 1)
        ;   I haven't inputed the mechanism about calculating numerical exponentiation. 
          (else (list '** base expon))))
      
(define (exponentiation? x) 
    (and (pair? x)
         (eq? (car x) '**)))

(define (base x) (cadr x))

(define (exponent x) (caddr x))

; These can be found in SICP book (around 200 page). 
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-sum a1 a2) 
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (=number? a1) (=number? a2)) (+ a1 a2))
          (else (list '+ a1 a2))))
      
(define (make-product p1 p2) 
    (cond ((or (=number? p1 0) (=number? p2 0)) 0)
          ((=number? p1 1) p2)
          ((=number? p2 1) p1)
          ((and (=number? p1) (=number? p2)) (* p1 p2))
          (else (list '* p1 p2))))
      
(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend x) (cadr x))

(define (augend x) (caddr x))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

; It's deriv with exponentiation. 
(define (deriv exp var) 
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          ((sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
          ((product? exp) (make-sum (make-product (multiplier exp) 
                                                  (deriv (multiplicand exp) var))
                                    (make-product (deriv (multiplier exp) var)
                                                  (multiplicand exp))))
          ((and (exponentiation? exp) (same-variable? (base exp) var)) 
                            (make-product (exponent exp) 
                                          (make-product (make-exponentiation base (- (exponent exp) 1)) 
                                                        (deriv (base exp) var))))
          (else (error "unknown expression type: DERIV" exp))))
