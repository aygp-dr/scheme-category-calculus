;; Basic Category Theory Example
;; Demonstrates symbolic category manipulation

(use-modules (ice-9 match)
             (ice-9 format)
             (srfi srfi-9)
             (srfi srfi-1))

;; Define a category record type
(define-record-type <category>
  (make-category-internal objects morphisms compose identity)
  category?
  (objects category-objects)
  (morphisms category-morphisms)
  (compose category-compose)
  (identity category-identity))

;; Category constructor with validation
(define (make-category objects morphisms compose identity)
  (make-category-internal objects morphisms compose identity))

;; Example: Category of Sets
(define (compose f g)
  "Function composition: (f ∘ g)(x) = f(g(x))"
  (lambda (x) (f (g x))))

(define (identity x) x)

;; Define the Set category
(define Set
  (make-category
    'all-sets           ; objects
    'all-functions      ; morphisms
    compose            ; composition
    identity))         ; identity

;; Helper to display category info
(define (display-category cat name)
  (format #t "Category ~a:~%" name)
  (format #t "  Objects: ~a~%" (category-objects cat))
  (format #t "  Morphisms: ~a~%" (category-morphisms cat))
  (format #t "  Has composition: ~a~%" (procedure? (category-compose cat)))
  (format #t "  Has identity: ~a~%~%" (procedure? (category-identity cat))))

;; Test composition
(define (test-composition)
  (let* ((f (lambda (x) (* x 2)))
         (g (lambda (x) (+ x 3)))
         (comp (category-compose Set))
         (h (comp f g)))
    (format #t "Testing composition in Set:~%")
    (format #t "  f(x) = x * 2~%")
    (format #t "  g(x) = x + 3~%")
    (format #t "  (f ∘ g)(5) = f(g(5)) = f(8) = ~a~%" (h 5))
    (format #t "  Expected: 16~%~%")))

;; Test identity law
(define (test-identity-law)
  (let* ((f (lambda (x) (* x 2)))
         (id (category-identity Set))
         (comp (category-compose Set))
         (left-id (comp f id))
         (right-id (comp id f)))
    (format #t "Testing identity laws:~%")
    (format #t "  f(x) = x * 2~%")
    (format #t "  (f ∘ id)(5) = ~a~%" (left-id 5))
    (format #t "  (id ∘ f)(5) = ~a~%" (right-id 5))
    (format #t "  f(5) = ~a~%" (f 5))
    (format #t "  Identity laws satisfied: ~a~%~%"
            (and (= (left-id 5) (f 5))
                 (= (right-id 5) (f 5))))))

;; Main demo
(define (main)
  (format #t "=== Basic Category Theory Demo ===~%~%")
  (display-category Set "Set")
  (test-composition)
  (test-identity-law)
  (format #t "=== Demo Complete ===~%"))

;; Run the demo
(main)