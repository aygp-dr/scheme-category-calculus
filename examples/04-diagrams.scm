;; Commutative Diagram Example
;; Demonstrates diagram calculation and commutation checking

(use-modules (ice-9 match)
             (ice-9 format)
             (srfi srfi-9)
             (srfi srfi-1))

;; Diagram record type
(define-record-type <diagram>
  (make-diagram objects morphisms equations)
  diagram?
  (objects diagram-objects)
  (morphisms diagram-morphisms)
  (equations diagram-equations))

;; Morphism record for tracking composition paths
(define-record-type <morphism>
  (make-morphism name source target function)
  morphism?
  (name morphism-name)
  (source morphism-source)
  (target morphism-target)
  (function morphism-function))

;; Compose morphisms
(define (compose-morphisms f g)
  "Compose morphisms g then f (standard mathematical order)"
  (if (equal? (morphism-target g) (morphism-source f))
      (make-morphism
        (string-append (morphism-name f) "∘" (morphism-name g))
        (morphism-source g)
        (morphism-target f)
        (lambda (x) ((morphism-function f) ((morphism-function g) x))))
      (error "Morphisms cannot be composed - incompatible types")))

;; Create a square diagram
(define (create-square-diagram)
  (let* ((f (make-morphism "f" 'A 'B (lambda (x) (* x 2))))
         (g (make-morphism "g" 'B 'D (lambda (x) (+ x 3))))
         (h (make-morphism "h" 'A 'C (lambda (x) (+ x 1))))
         (i (make-morphism "i" 'C 'D (lambda (x) (* x 2)))))
    (make-diagram
      '(A B C D)                    ; objects
      (list f g h i)                ; morphisms
      `((,(compose-morphisms g f)   ; equations (commutative paths)
         ,(compose-morphisms i h))))))

;; Check if diagram commutes
(define (check-commutation diagram test-values)
  (format #t "Checking diagram commutation:~%~%")
  
  (let ((equations (diagram-equations diagram)))
    (for-each
      (lambda (eq-pair)
        (let ((path1 (car eq-pair))
              (path2 (cadr eq-pair)))
          (format #t "Path 1: ~a~%" (morphism-name path1))
          (format #t "Path 2: ~a~%~%" (morphism-name path2))
          
          (let ((all-commute
                  (every
                    (lambda (val)
                      (let ((result1 ((morphism-function path1) val))
                            (result2 ((morphism-function path2) val)))
                        (format #t "  Test value ~a:~%" val)
                        (format #t "    Path 1 result: ~a~%" result1)
                        (format #t "    Path 2 result: ~a~%" result2)
                        (format #t "    Commutes: ~a~%~%" (equal? result1 result2))
                        (equal? result1 result2)))
                    test-values)))
            (format #t "Overall commutation: ~a~%~%"
                    (if all-commute "✓ Diagram commutes" "✗ Diagram does NOT commute")))))
      equations)))

;; Pullback diagram example
(define (create-pullback-diagram)
  (format #t "Creating pullback diagram:~%")
  (format #t "  P -----> B~%")
  (format #t "  |        |~%")
  (format #t "  |        |~%")
  (format #t "  v        v~%")
  (format #t "  A -----> C~%~%")
  
  ;; Functions for the diagram
  (let* ((f (make-morphism "f" 'A 'C (lambda (x) (modulo x 5))))
         (g (make-morphism "g" 'B 'C (lambda (x) (modulo x 5))))
         ;; Pullback object P consists of pairs (a,b) where f(a) = g(b)
         (p1 (make-morphism "π₁" 'P 'A (lambda (pair) (car pair))))
         (p2 (make-morphism "π₂" 'P 'B (lambda (pair) (cdr pair)))))
    (make-diagram
      '(P A B C)
      (list f g p1 p2)
      `((,(compose-morphisms f p1)
         ,(compose-morphisms g p2))))))

;; Universal property checker for pullback
(define (check-pullback-universal-property)
  (format #t "Checking pullback universal property:~%~%")
  
  ;; Given morphisms h: X -> A and k: X -> B with f∘h = g∘k
  ;; There should exist unique u: X -> P such that π₁∘u = h and π₂∘u = k
  
  (let* ((h (lambda (x) (* x 3)))      ; X -> A
         (k (lambda (x) (+ x 10)))      ; X -> B
         (f (lambda (x) (modulo x 5)))  ; A -> C
         (g (lambda (x) (modulo x 5)))  ; B -> C
         ;; Check if f∘h = g∘k for some test values
         (test-vals '(0 1 2 3 4 5)))
    
    (format #t "Testing morphisms h: x ↦ 3x and k: x ↦ x+10~%")
    (format #t "With f,g: x ↦ x mod 5~%~%")
    
    (let ((compatible
            (every
              (lambda (x)
                (let ((fh (f (h x)))
                      (gk (g (k x))))
                  (format #t "  x=~a: f(h(~a))=~a, g(k(~a))=~a ~a~%"
                          x x fh x gk
                          (if (= fh gk) "✓" "✗"))
                  (= fh gk)))
              test-vals)))
      (if compatible
          (format #t "~%✓ Morphisms satisfy the pullback condition~%")
          (format #t "~%✗ Morphisms do NOT satisfy the pullback condition~%")))))

;; Natural transformation square
(define (create-naturality-square)
  (format #t "~%Natural Transformation Square:~%")
  (format #t "  F(A) --F(f)--> F(B)~%")
  (format #t "   |              |~%")
  (format #t "  η_A            η_B~%")
  (format #t "   |              |~%")
  (format #t "   v              v~%")
  (format #t "  G(A) --G(f)--> G(B)~%~%")
  
  ;; Example: List to length natural transformation
  (let* ((f (make-morphism "reverse" 'ListInt 'ListInt reverse))
         (F-f (make-morphism "F(reverse)" 'F-ListInt 'F-ListInt reverse))
         (G-f (make-morphism "G(reverse)" 'G-ListInt 'G-ListInt reverse))
         (eta-A (make-morphism "length_A" 'F-ListInt 'G-ListInt length))
         (eta-B (make-morphism "length_B" 'F-ListInt 'G-ListInt length)))
    (make-diagram
      '(F-ListInt F-ListInt G-ListInt G-ListInt)
      (list F-f G-f eta-A eta-B)
      `((,(compose-morphisms eta-B F-f)
         ,(compose-morphisms G-f eta-A))))))

;; Main demo
(define (main)
  (format #t "=== Commutative Diagram Demo ===~%~%")
  
  ;; Test square diagram
  (format #t "1. Square Diagram Test~%")
  (format #t "======================~%")
  (let ((square (create-square-diagram)))
    (format #t "Square diagram:~%")
    (format #t "  A ---f---> B~%")
    (format #t "  |          |~%")
    (format #t "  h          g~%")
    (format #t "  |          |~%")
    (format #t "  v          v~%")
    (format #t "  C ---i---> D~%~%")
    (format #t "Where: f(x)=2x, g(x)=x+3, h(x)=x+1, i(x)=2x~%")
    (format #t "g∘f: x ↦ 2x+3, i∘h: x ↦ 2(x+1) = 2x+2~%")
    (format #t "Note: This diagram does NOT commute (intentionally, for demonstration)~%~%")
    (check-commutation square '(1 2 3 4 5)))
  
  ;; Test pullback diagram
  (format #t "2. Pullback Diagram Test~%")
  (format #t "========================~%")
  (let ((pullback (create-pullback-diagram)))
    (check-commutation pullback '((2 . 7) (3 . 8) (4 . 9))))
  
  ;; Check universal property
  (check-pullback-universal-property)
  
  (format #t "~%=== Demo Complete ===~%"))

;; Run the demo
(main)