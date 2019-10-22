#lang racket
(require 2htdp/image)
;;;
;;; Definitions for image iterators used in EECS-111
;;; Ian Horswill, 10/6/2015
;;;
;;; Please note that these definitions are a little fancier than those
;;; given in class:
;;;   - The version in class doesn't handle a count of 0 or 1
;;;   - This version gives better error messages
;;;

(provide iterated-overlay iterated-underlay iterated-beside iterated-above
         row column grid
         safe-color
         interpolate-colors)

(define iterate
  (λ (name combiner generator count)
    (cond [(not (procedure? generator))
           (raise-argument-error name "procedure" generator)]
          [(not (procedure-arity-includes? generator 1))
           (raise-argument-error name "procedure of one argument" generator)]
          [(not (integer? count))
           (raise-argument-error name "integer" count)]
          [(< count 0)
           (raise-argument-error name "non-negative-integer" count)]
          [(= count 0)
           empty-image]
          [(= count 1)
           (generator 0)]
          [else
           (define l (build-list count generator))
           (for-each
            (lambda (i)
              (unless (image? i)
                (error name "Expected generator to return an image, but instead it returned ~a"
                       i)))
            l)
           (apply combiner
                  l)])))

(define iterated-overlay
  (λ (generator count)
    (iterate 'iterated-overlay overlay generator count)))

(define iterated-underlay
  (λ (generator count)
    (iterate 'iterated-underlay underlay generator count)))

(define iterated-above
  (λ (generator count)
    (iterate 'iterated-above above generator count)))

(define iterated-beside
  (λ (generator count)
    (iterate 'iterated-beside beside generator count)))

(define row
  (λ (item count)
    (iterated-beside (λ (ignore) item)
                     count)))

(define column
  (λ (item count)
    (iterated-above (λ (ignore) item)
                     count)))

(define grid
  (λ (item columns rows)
    (column (row item columns)
            rows)))

(define safe-color
  (case-lambda
    [(r g b)
     (color (safe r) (safe g) (safe b))]
    [(r g b a)
     (color (safe r) (safe g) (safe b) (safe a))]))

(define safe
  (λ (value)
    (min 255 (max 0 (inexact->exact (round value))))))

(define interpolate
  (λ (a b weight)
    (+ a
       (* weight
          (- b a)))))

(define interpolate-colors
  (λ (c1 c2 weight)
    (safe-color (interpolate (color-red c1)
                             (color-red c2)
                             weight)
                (interpolate (color-green c1)
                             (color-green c2)
                             weight)
                (interpolate (color-blue c1)
                             (color-blue c2)
                             weight)
                (interpolate (color-alpha c1)
                             (color-alpha c2)
                             weight))))

