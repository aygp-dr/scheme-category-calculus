;; Natural Transformation Example
;; Demonstrates natural transformations between functors

(use-modules (ice-9 match)
             (ice-9 format)
             (srfi srfi-9)
             (srfi srfi-1))

;; Basic definitions
(define-record-type <functor>
  (make-functor name object-map morphism-map)
  functor?
  (name functor-name)
  (object-map functor-object-map)
  (morphism-map functor-morphism-map))

(define-record-type <natural-transformation>
  (make-natural-transformation name source target components)
  natural-transformation?
  (name nat-trans-name)
  (source nat-trans-source)
  (target nat-trans-target)
  (components nat-trans-components))

;; List functor
(define List
  (make-functor
    "List"
    (lambda (type) `(List ,type))
    (lambda (f) (lambda (lst) (map f lst)))))

;; Maybe functor
(define Maybe
  (make-functor
    "Maybe"
    (lambda (type) `(Maybe ,type))
    (lambda (f)
      (lambda (m)
        (match m
          (`(Just ,x) `(Just ,(f x)))
          ('Nothing 'Nothing))))))

;; Natural transformation: safeHead from List to Maybe
(define safeHead
  (make-natural-transformation
    "safeHead"
    List
    Maybe
    (lambda (type)  ; Component at each type
      (lambda (lst)
        (if (null? lst)
            'Nothing
            `(Just ,(car lst)))))))

;; Natural transformation: length from List to Const-Int
(define length-transform
  (make-natural-transformation
    "length"
    List
    (make-functor "Const-Int" 
                  (lambda (type) 'Int)
                  (lambda (f) identity))  ; Const functor ignores morphisms
    (lambda (type) length)))

;; Verify naturality condition
(define (verify-naturality nat-trans f test-value)
  (let* ((F (nat-trans-source nat-trans))
         (G (nat-trans-target nat-trans))
         (eta (nat-trans-components nat-trans))
         (F-f ((functor-morphism-map F) f))
         (G-f ((functor-morphism-map G) f))
         (eta-A (eta 'A))
         (eta-B (eta 'B)))
    
    ;; Naturality: G(f) ∘ η_A = η_B ∘ F(f)
    (let* ((left (lambda (x) (G-f (eta-A x))))
           (right (lambda (x) (eta-B (F-f x))))
           (left-result (left test-value))
           (right-result (right test-value)))
      
      (format #t "Verifying naturality for ~a:~%" (nat-trans-name nat-trans))
      (format #t "  Test value: ~a~%" test-value)
      (format #t "  G(f) ∘ η_A: ~a~%" left-result)
      (format #t "  η_B ∘ F(f): ~a~%" right-result)
      (format #t "  Naturality satisfied: ~a~%~%"
              (equal? left-result right-result))
      (equal? left-result right-result))))

;; Demonstrate safeHead natural transformation
(define (demonstrate-safeHead)
  (format #t "SafeHead Natural Transformation~%")
  (format #t "===============================~%")
  (format #t "Components map lists to their first element (wrapped in Maybe)~%~%")
  
  (let ((eta (nat-trans-components safeHead)))
    (let ((component (eta 'Int)))
      (format #t "Examples:~%")
      (format #t "  safeHead([1,2,3]) = ~a~%" (component '(1 2 3)))
      (format #t "  safeHead([]) = ~a~%" (component '()))
      (format #t "  safeHead([42]) = ~a~%~%" (component '(42)))))
  
  ;; Test naturality
  (let ((f (lambda (x) (* x 2))))
    (format #t "Testing naturality with f(x) = 2x:~%")
    (verify-naturality safeHead f '(5 10 15))
    (verify-naturality safeHead f '())))

;; Demonstrate length natural transformation
(define (demonstrate-length)
  (format #t "Length Natural Transformation~%")
  (format #t "=============================~%")
  (format #t "Maps lists to their length (constant functor target)~%~%")
  
  (let ((eta (nat-trans-components length-transform)))
    (let ((component (eta 'String)))
      (format #t "Examples:~%")
      (format #t "  length([a,b,c]) = ~a~%" (component '(a b c)))
      (format #t "  length([]) = ~a~%" (component '()))
      (format #t "  length([x,y,z,w]) = ~a~%~%" (component '(x y z w)))))
  
  ;; Test naturality (trivial for const functor)
  (let ((f (lambda (x) (string-append x "!"))))
    (format #t "Testing naturality with string concatenation:~%")
    (verify-naturality length-transform f '("hello" "world"))))

;; Composition of natural transformations
(define (vertical-composition eta theta)
  "Compose natural transformations η: F ⟹ G and θ: G ⟹ H"
  (make-natural-transformation
    (string-append (nat-trans-name theta) "∘" (nat-trans-name eta))
    (nat-trans-source eta)
    (nat-trans-target theta)
    (lambda (type)
      (let ((eta-component ((nat-trans-components eta) type))
            (theta-component ((nat-trans-components theta) type)))
        (lambda (x) (theta-component (eta-component x)))))))

;; Example: reverse as natural transformation
(define reverse-transform
  (make-natural-transformation
    "reverse"
    List
    List
    (lambda (type) reverse)))

(define (demonstrate-reverse)
  (format #t "Reverse Natural Transformation~%")
  (format #t "==============================~%")
  (format #t "Endofunctor natural transformation (List ⟹ List)~%~%")
  
  (let ((component ((nat-trans-components reverse-transform) 'Int)))
    (format #t "Examples:~%")
    (format #t "  reverse([1,2,3,4]) = ~a~%" (component '(1 2 3 4)))
    (format #t "  reverse([a]) = ~a~%" (component '(a)))
    (format #t "  reverse([]) = ~a~%~%" (component '())))
  
  ;; Test naturality
  (let ((f (lambda (x) (+ x 10))))
    (format #t "Testing naturality with f(x) = x+10:~%")
    (verify-naturality reverse-transform f '(1 2 3 4))))

;; Main demo
(define (main)
  (format #t "=== Natural Transformation Demo ===~%~%")
  
  (demonstrate-safeHead)
  (demonstrate-length)
  (demonstrate-reverse)
  
  ;; Demonstrate composition
  (format #t "Natural Transformation Composition~%")
  (format #t "==================================~%")
  (let* ((double-reverse (vertical-composition reverse-transform reverse-transform))
         (component ((nat-trans-components double-reverse) 'Symbol)))
    (format #t "Double reverse (should be identity):~%")
    (format #t "  reverse∘reverse([a,b,c,d]) = ~a~%"
            (component '(a b c d)))
    (format #t "  Identity satisfied: ~a~%"
            (equal? (component '(a b c d)) '(a b c d))))
  
  (format #t "~%=== Demo Complete ===~%"))

;; Run the demo
(main)