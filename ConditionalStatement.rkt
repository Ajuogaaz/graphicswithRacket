;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ConditionalStatement) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
 ;;(cond
 ;;[(positive? -5) (error "doesn't get here")]
 ;;[(zero? -5) (error "doesn't get here, either")]
 ;; [(positive? 5) (error "its here")])


;;(cond
;;   [(member? 2 '(1 2 3))])

(define a 35)
(define b 44)

(= a b)
(> a b)
(< a b)



;;Absolute Value: number -> number
;;Removes the sign from a number

(define absoluteValue
  (lambda (n)
    (cond
   [(positive? n) (error "doesn't get here")]
   [(zero? n) (error "doesn't get here, either")]
   [(not (positive? n)) 'here])))

(absoluteValue -3)

(or (number? a) (number? b) (= a b))

(define perfectSquare?
  (lambda (n)
    (integer? (sqrt n))
    ))

(perfectSquare? 4)

(define (pefectSquare? n)
  (integer? (sqrt n)))

(pefectSquare? 16)

  
