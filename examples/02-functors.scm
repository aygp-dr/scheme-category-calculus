;; Functor Example
;; Demonstrates functors between categories

(use-modules (ice-9 match)
             (ice-9 format)
             (srfi srfi-9)
             (srfi srfi-1))

;; Category definition (reused from previous example)
(define-record-type <category>
  (make-category objects morphisms compose identity)
  category?
  (objects category-objects)
  (morphisms category-morphisms)
  (compose category-compose)
  (identity category-identity))

;; Functor definition
(define-record-type <functor>
  (make-functor source target object-map morphism-map)
  functor?
  (source functor-source)
  (target functor-target)
  (object-map functor-object-map)
  (morphism-map functor-morphism-map))

;; Function composition
(define (compose f g)
  (lambda (x) (f (g x))))

(define (identity x) x)

;; The Set category
(define Set
  (make-category 'sets 'functions compose identity))

;; List functor implementation
(define (list-object-map type)
  "Maps a type to List of that type"
  `(List ,type))

(define (list-morphism-map f)
  "Maps a function to map over lists"
  (lambda (lst) (map f lst)))

;; Create the List functor
(define List-functor
  (make-functor
    Set                  ; source category
    Set                  ; target category
    list-object-map      ; object mapping
    list-morphism-map))  ; morphism mapping

;; Verify functor laws
(define (verify-functor-laws functor)
  (format #t "Verifying functor laws for ~a:~%~%" 
          (if (eq? functor List-functor) "List" "Unknown"))
  
  ;; Test 1: Functor preserves identity
  (let* ((id identity)
         (fmap (functor-morphism-map functor))
         (fmap-id (fmap id))
         (test-list '(1 2 3 4 5)))
    (format #t "1. Identity preservation:~%")
    (format #t "   F(id) should equal id~%")
    (format #t "   Original list: ~a~%" test-list)
    (format #t "   F(id) applied: ~a~%" (fmap-id test-list))
    (format #t "   Identity law satisfied: ~a~%~%"
            (equal? (fmap-id test-list) test-list)))
  
  ;; Test 2: Functor preserves composition
  (let* ((f (lambda (x) (* x 2)))
         (g (lambda (x) (+ x 3)))
         (comp (compose f g))
         (fmap (functor-morphism-map functor))
         (fmap-comp (fmap comp))
         (fmap-f-g (compose (fmap f) (fmap g)))
         (test-list '(1 2 3)))
    (format #t "2. Composition preservation:~%")
    (format #t "   F(f ∘ g) should equal F(f) ∘ F(g)~%")
    (format #t "   f(x) = x * 2, g(x) = x + 3~%")
    (format #t "   Original list: ~a~%" test-list)
    (format #t "   F(f ∘ g): ~a~%" (fmap-comp test-list))
    (format #t "   F(f) ∘ F(g): ~a~%" (fmap-f-g test-list))
    (format #t "   Composition law satisfied: ~a~%~%"
            (equal? (fmap-comp test-list) (fmap-f-g test-list)))))

;; Example usage of the List functor
(define (demonstrate-list-functor)
  (format #t "List Functor Demonstration:~%")
  (format #t "==========================~%~%")
  
  ;; Object mapping
  (let ((int-type 'Int))
    (format #t "Object mapping:~%")
    (format #t "  Int → ~a~%~%" 
            ((functor-object-map List-functor) int-type)))
  
  ;; Morphism mapping
  (let* ((double (lambda (x) (* x 2)))
         (fmap (functor-morphism-map List-functor))
         (numbers '(1 2 3 4 5)))
    (format #t "Morphism mapping:~%")
    (format #t "  Function: double(x) = x * 2~%")
    (format #t "  Original list: ~a~%" numbers)
    (format #t "  Mapped list: ~a~%~%"
            ((fmap double) numbers))))

;; Main demo
(define (main)
  (format #t "=== Functor Theory Demo ===~%~%")
  (demonstrate-list-functor)
  (verify-functor-laws List-functor)
  (format #t "=== Demo Complete ===~%"))

;; Run the demo
(main)