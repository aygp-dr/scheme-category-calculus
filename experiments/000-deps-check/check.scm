;; Dependency Check for Guile Scheme Category Theory Calculus Engine
;; Confirms Guile 3.0+ and mathematical computation capabilities

(use-modules (ice-9 format)
             (ice-9 regex)
             (ice-9 pretty-print)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26)
             (srfi srfi-9)
             (system base compile))

(define (check-guile-version)
  "Check that we're running Guile 3.0 or later"
  (let ((version (version)))
    (format #t "✓ Guile version: ~a~%" version)
    (if (string-prefix? "3." version)
        (format #t "✓ Guile 3.x detected - compatible~%")
        (format #t "⚠ Warning: Expected Guile 3.x, got ~a~%" version))))

(define (check-module module-name)
  "Check if a module can be loaded"
  (catch #t
    (lambda ()
      (resolve-module module-name)
      (format #t "✓ Module ~a: available~%" module-name)
      #t)
    (lambda (key . args)
      (format #t "✗ Module ~a: missing (~a)~%" module-name key)
      #f)))

(define required-modules
  '((ice-9 match)
    (ice-9 format)
    (ice-9 pretty-print)
    (srfi srfi-1)
    (srfi srfi-9)
    (srfi srfi-26)
    (system base compile)))

(define mathematical-modules
  '((ice-9 q)
    (ice-9 hash-table)
    (srfi srfi-69)
    (ice-9 vlist)))

(define (test-basic-computation)
  "Test basic mathematical computation capabilities"
  (format #t "Mathematical computation tests:~%")
  (format #t "✓ Basic arithmetic: ~a~%" (+ 1 2 3))
  (format #t "✓ List operations: ~a~%" (fold + 0 '(1 2 3 4)))
  (format #t "✓ Pattern matching: ")
  (match '(category morphism)
    ((cat mor) (format #t "~a -> ~a works~%" cat mor))
    (_ (format #t "failed~%"))))

(define (main)
  (format #t "=== Guile Scheme Category Theory Calculus Engine - Dependency Check ===~%~%")
  
  ;; Check Guile version
  (check-guile-version)
  (newline)
  
  ;; Check required modules
  (format #t "Required modules:~%")
  (let ((missing (filter (lambda (mod) (not (check-module mod))) required-modules)))
    (if (null? missing)
        (format #t "✓ All required modules available~%")
        (format #t "✗ Missing required modules: ~a~%" missing)))
  
  (newline)
  
  ;; Check mathematical modules
  (format #t "Mathematical computation modules:~%")
  (for-each check-module mathematical-modules)
  
  (newline)
  
  ;; Test computations
  (test-basic-computation)
  
  (newline)
  (format #t "=== Dependency check complete ===~%"))

;; Run the check
(main)