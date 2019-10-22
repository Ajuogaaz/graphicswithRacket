#lang racket
(require (for-syntax syntax/id-table syntax/parse racket/syntax)
         (prefix-in asl: (only-in lang/htdp-advanced lambda))
         )
(provide (rename-out [in:define-struct define-struct]))

(begin-for-syntax
  (define method-table (make-free-id-table))
  (define (get/make-method name)
    (define fold
      (if (free-id-table-ref method-table name #f)
          #'()
          (let ()
            (define-values (prop pred acc)
              (apply values
                     (syntax-local-lift-values-expression 3 #`(make-struct-type-property '#,name))))
            (free-id-table-set! method-table name prop)
            (with-syntax ([do (generate-temporary)])
              #`((define (do marks s . a)
                   (unless (#,pred s)
                     (raise
                      (make-exn:fail:contract
                       (format "method : Expected a structure that defines the method `~a` but was given ~s"
                               '#,name s)
                       marks)))
                   (apply (#,acc s) s a))

                 (define-syntax (#,name stx)
                   (syntax-parse stx
                     [a:id
                      (with-syntax ([ccm (syntax/loc #'a (current-continuation-marks))])
                        (syntax/loc stx (curry do ccm)))]
                     [(n args (... ...))
                      (with-syntax ([ccm (syntax/loc stx (current-continuation-marks))])
                        (quasisyntax/loc stx
                          (do ccm args (... ...))))])))))))
    (list (free-id-table-ref method-table name)
          fold)))

(define-syntax (in:define-struct stx)
  (define-syntax-class def
    #:attributes (name lmbda)
    (pattern (define (name . a) body ...)
             #:with lmbda (syntax/loc this-syntax (asl:lambda a body ...)))
    (pattern (define name body)
             #:with lmbda #'(let ([bdy body])
                              (unless (procedure? bdy)
                                (error 'method "Expected a procedure for method ~a, but was given ~a"
                                       'name bdy))
                              bdy)))
  (syntax-parse stx
    [(n names:expr
        fields:expr)
     #'(n names fields #:methods)]
    [(_ names:expr [fields:id ...] #:methods methods:def ...)
     (with-syntax* ([((prop (lifts ...)) ...)
                     (map get/make-method (syntax->list #'(methods.name ...)))]
                    [((props ...) ...)
                     (map (lambda (p l) (list '#:property p l))
                          (syntax->list #'(prop ...))
                          (syntax->list #'(methods.lmbda ...)))])
       #`(begin
           (define-struct names ([fields #:mutable] ... )
             #:constructor-name #,(format-id #'names "make-~a"
                                             (if (identifier? #'names) #'names (car (syntax-e #'names))))
             #:transparent
             props ... ...)
           lifts ... ...))]))

(module* test racket
  (require (submod "." "..") rackunit)

  (define-struct my-posn (x y)
    #:methods
    (define (add a b)
      (make-my-posn (+ (my-posn-x a)
                       (my-posn-x b))
                    (+ (my-posn-y a)
                       (my-posn-y b)))))

  (check-equal? (add (make-my-posn 1 1) (make-my-posn 2 2))
                (make-my-posn 3 3))

  (define-struct (posn3d my-posn) (z)
    #:methods
    (define (add a b)
      (make-posn3d (+ (my-posn-x a)
                      (my-posn-x b))
                   (+ (my-posn-y a)
                      (my-posn-y b))
                   (+ (posn3d-z a)
                      (posn3d-z b)))))

  (check-equal? (add (make-posn3d 1 1 1) (make-posn3d 2 2 2))
                (make-posn3d 3 3 3))

  (define-struct (posn2 my-posn) ())

  (check-equal? (add (make-posn2 1 1) (make-posn2 2 2))
                (make-my-posn 3 3))
  (check-exn
   exn:fail?
   (thunk (add "a" "b"))))
