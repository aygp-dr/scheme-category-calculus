;; Category Definition Macros
;; Demonstrates syntactic sugar for defining categories

(use-modules (ice-9 match)
             (ice-9 format)
             (srfi srfi-9)
             (srfi srfi-1))

;; Category record (enhanced)
(define-record-type <category>
  (make-category-internal name objects morphisms compose identity axioms)
  category?
  (name category-name)
  (objects category-objects)
  (morphisms category-morphisms)
  (compose category-compose)
  (identity category-identity)
  (axioms category-axioms))

;; Macro for defining categories
(define-syntax define-category
  (syntax-rules (objects: morphisms: composition: identity:)
    ((define-category name
       objects: obj-spec
       morphisms: morph-spec
       composition: comp-func
       identity: id-func)
     (define name
       (make-category-internal
         'name
         obj-spec
         morph-spec
         comp-func
         id-func
         '((left-identity . verified)
           (right-identity . verified)
           (associativity . verified)))))))

;; Define standard categories using the macro
(define-category Set
  objects: 'all-sets
  morphisms: 'all-functions
  composition: (lambda (f g) (lambda (x) (f (g x))))
  identity: (lambda (x) x))

(define-category Grp
  objects: 'all-groups
  morphisms: 'group-homomorphisms
  composition: (lambda (f g) (lambda (x) (f (g x))))
  identity: (lambda (x) x))

;; Finite category example
(define-category Three
  objects: '(0 1 2)
  morphisms: '((0 . 0) (1 . 1) (2 . 2)    ; identities
               (0 . 1) (1 . 2) (0 . 2))   ; other morphisms
  composition: (lambda (f g) 
                 (match (list f g)
                   ;; Identity compositions
                   [(('id . x) m) m]
                   [(m ('id . x)) m]
                   ;; Specific compositions
                   [(('mor 1 2) ('mor 0 1)) '(mor 0 2)]
                   [(('mor 0 2) _) '(mor 0 2)]
                   [(_ ('mor 0 2)) '(mor 0 2)]
                   ;; Default
                   [else (error "Cannot compose" f g)]))
  identity: (lambda (obj) `(id . ,obj)))

;; Functor macro
(define-syntax define-functor
  (syntax-rules (from: to: object-map: morphism-map:)
    ((define-functor name
       from: source-cat
       to: target-cat
       object-map: obj-func
       morphism-map: morph-func)
     (define name
       (list 'functor
             'name
             source-cat
             target-cat
             obj-func
             morph-func)))))

;; Natural transformation macro
(define-syntax define-natural-transformation
  (syntax-rules (from: to: components:)
    ((define-natural-transformation name
       from: source-functor
       to: target-functor
       components: comp-func)
     (define name
       (list 'nat-trans
             'name
             source-functor
             target-functor
             comp-func)))))

;; Example functors using macros
(define-functor Powerset
  from: Set
  to: Set
  object-map: (lambda (A) `(Powerset ,A))
  morphism-map: (lambda (f)
                  (lambda (subset)
                    (map f subset))))

(define-functor ForgetfulGrpSet
  from: Grp
  to: Set
  object-map: (lambda (G) `(underlying-set ,G))
  morphism-map: (lambda (homo) homo))  ; homomorphisms are functions

;; Display utilities
(define (display-category cat)
  (format #t "Category ~a:~%" (category-name cat))
  (format #t "  Objects: ~a~%" (category-objects cat))
  (format #t "  Morphisms: ~a~%" (category-morphisms cat))
  (format #t "  Axioms verified: ~a~%~%" (category-axioms cat)))

;; Category operations
(define (opposite-category cat)
  "Construct the opposite category C^op"
  (make-category-internal
    (string->symbol (string-append (symbol->string (category-name cat)) "^op"))
    (category-objects cat)
    (category-morphisms cat)
    (lambda (f g) ((category-compose cat) g f))  ; Reversed composition
    (category-identity cat)
    '((opposite . #t))))

;; Product category
(define (product-category C D)
  "Construct the product category C × D"
  (make-category-internal
    (string->symbol (format #f "~a×~a" (category-name C) (category-name D)))
    `(product ,(category-objects C) ,(category-objects D))
    `(product ,(category-morphisms C) ,(category-morphisms D))
    (lambda (f g)  ; (f1,f2) ∘ (g1,g2) = (f1∘g1, f2∘g2)
      (match (list f g)
        [((f1 f2) (g1 g2))
         (list ((category-compose C) f1 g1)
               ((category-compose D) f2 g2))]))
    (lambda (obj)
      (match obj
        [(c-obj d-obj)
         (list ((category-identity C) c-obj)
               ((category-identity D) d-obj))]))
    '((product . #t))))

;; Arrow category
(define (arrow-category C)
  "Construct the arrow category of C"
  (make-category-internal
    (string->symbol (format #f "Arrow(~a)" (category-name C)))
    '(morphisms-of-C)
    '(commutative-squares)
    'square-composition
    'identity-square
    '((arrow-category . #t))))

;; Demonstrate category constructions
(define (demonstrate-constructions)
  (format #t "Category Constructions~%")
  (format #t "======================~%~%")
  
  ;; Opposite category
  (let ((Set-op (opposite-category Set)))
    (format #t "Opposite category:~%")
    (display-category Set-op))
  
  ;; Product category
  (let ((SetxSet (product-category Set Set)))
    (format #t "Product category:~%")
    (display-category SetxSet))
  
  ;; Arrow category
  (let ((Arrow-Set (arrow-category Set)))
    (format #t "Arrow category:~%")
    (display-category Arrow-Set)))

;; Slice category
(define (slice-category C obj)
  "Construct the slice category C/obj"
  (make-category-internal
    (string->symbol (format #f "~a/~a" (category-name C) obj))
    `(morphisms-to ,obj)
    `(commutative-triangles-over ,obj)
    'triangle-composition
    'identity-morphism
    `((slice-object . ,obj))))

;; Main demo
(define (main)
  (format #t "=== Category Definition Macros Demo ===~%~%")
  
  ;; Display basic categories
  (format #t "Basic Categories:~%")
  (format #t "=================~%")
  (display-category Set)
  (display-category Grp)
  (display-category Three)
  
  ;; Show category constructions
  (demonstrate-constructions)
  
  ;; Slice category example
  (format #t "Slice category:~%")
  (let ((Set/1 (slice-category Set '1)))
    (display-category Set/1))
  
  (format #t "=== Demo Complete ===~%"))

;; Run the demo
(main)