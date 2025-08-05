;; Monad Example
;; Demonstrates monads with computational semantics

(use-modules (ice-9 match)
             (ice-9 format)
             (srfi srfi-9))

;; Monad record type
(define-record-type <monad>
  (make-monad unit bind)
  monad?
  (unit monad-unit)
  (bind monad-bind))

;; Maybe monad implementation
(define maybe-unit
  (lambda (x) `(Just ,x)))

(define maybe-bind
  (lambda (m f)
    (match m
      (`(Just ,x) (f x))
      ('Nothing 'Nothing))))

(define Maybe
  (make-monad maybe-unit maybe-bind))

;; Monadic helper functions
(define (return monad value)
  ((monad-unit monad) value))

(define (>>= monad m f)
  ((monad-bind monad) m f))

;; Safe division using Maybe monad
(define (safe-divide x y)
  (if (zero? y)
      'Nothing
      `(Just ,(/ x y))))

;; Chain of safe operations
(define (safe-calculation x)
  (>>= Maybe
    (safe-divide x 2)
    (lambda (a)
      (>>= Maybe
        (safe-divide a 3)
        (lambda (b)
          (return Maybe (* b 10)))))))

;; Verify monad laws
(define (verify-monad-laws monad)
  (format #t "Verifying monad laws:~%~%")
  
  ;; Left identity: return a >>= f ≡ f a
  (let* ((a 42)
         (f (lambda (x) `(Just ,(* x 2))))
         (left (>>= monad (return monad a) f))
         (right (f a)))
    (format #t "1. Left Identity Law:~%")
    (format #t "   return ~a >>= f should equal f(~a)~%" a a)
    (format #t "   Left side: ~a~%" left)
    (format #t "   Right side: ~a~%" right)
    (format #t "   Law satisfied: ~a~%~%" (equal? left right)))
  
  ;; Right identity: m >>= return ≡ m
  (let* ((m `(Just 42))
         (left (>>= monad m (lambda (x) (return monad x))))
         (right m))
    (format #t "2. Right Identity Law:~%")
    (format #t "   m >>= return should equal m~%")
    (format #t "   m = ~a~%" m)
    (format #t "   Left side: ~a~%" left)
    (format #t "   Right side: ~a~%" right)
    (format #t "   Law satisfied: ~a~%~%" (equal? left right)))
  
  ;; Associativity: (m >>= f) >>= g ≡ m >>= (λx. f x >>= g)
  (let* ((m `(Just 10))
         (f (lambda (x) `(Just ,(* x 2))))
         (g (lambda (x) `(Just ,(+ x 3))))
         (left (>>= monad (>>= monad m f) g))
         (right (>>= monad m (lambda (x) (>>= monad (f x) g)))))
    (format #t "3. Associativity Law:~%")
    (format #t "   (m >>= f) >>= g should equal m >>= (λx. f x >>= g)~%")
    (format #t "   m = ~a, f = double, g = add3~%" m)
    (format #t "   Left side: ~a~%" left)
    (format #t "   Right side: ~a~%" right)
    (format #t "   Law satisfied: ~a~%~%" (equal? left right))))

;; Demonstrate Maybe monad usage
(define (demonstrate-maybe-monad)
  (format #t "Maybe Monad Demonstration:~%")
  (format #t "=========================~%~%")
  
  ;; Successful computation
  (format #t "Safe calculation chain (starting with 60):~%")
  (format #t "  60 ÷ 2 ÷ 3 × 10 = ~a~%~%" (safe-calculation 60))
  
  ;; Failed computation (division by zero)
  (format #t "Safe division examples:~%")
  (format #t "  10 ÷ 2 = ~a~%" (safe-divide 10 2))
  (format #t "  10 ÷ 0 = ~a~%~%" (safe-divide 10 0))
  
  ;; Chain that fails
  (format #t "Calculation that encounters division by zero:~%")
  (let ((result (>>= Maybe
                  (safe-divide 10 2)
                  (lambda (a)
                    (>>= Maybe
                      (safe-divide a 0)  ; This will fail
                      (lambda (b)
                        (return Maybe (* b 10))))))))
    (format #t "  10 ÷ 2 ÷ 0 × 10 = ~a~%~%" result)))

;; do-notation style helper
(define-syntax do-maybe
  (syntax-rules (<-)
    ((do-maybe result)
     result)
    ((do-maybe (var <- expr) rest ...)
     (>>= Maybe expr (lambda (var) (do-maybe rest ...))))))

;; Example using do-notation style
(define (calculation-with-do x y z)
  (do-maybe
    (a <- (safe-divide x y))
    (b <- (safe-divide a z))
    (return Maybe (* b 100))))

(define (demonstrate-do-notation)
  (format #t "Do-notation style:~%")
  (format #t "==================~%")
  (format #t "100 ÷ 2 ÷ 5 × 100 = ~a~%" (calculation-with-do 100 2 5))
  (format #t "100 ÷ 0 ÷ 5 × 100 = ~a~%" (calculation-with-do 100 0 5))
  (format #t "100 ÷ 2 ÷ 0 × 100 = ~a~%~%" (calculation-with-do 100 2 0)))

;; Main demo
(define (main)
  (format #t "=== Monad Theory Demo ===~%~%")
  (demonstrate-maybe-monad)
  (demonstrate-do-notation)
  (verify-monad-laws Maybe)
  (format #t "=== Demo Complete ===~%"))

;; Run the demo
(main)