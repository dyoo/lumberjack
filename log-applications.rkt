#lang racket/base

(require syntax/kerncase
         racket/contract
         (for-syntax racket/base))

(define code-insp (current-code-inspector))

(provide/contract [annotate-applications
                   (syntax? . -> . syntax?)])


;; annotate-applications: syntax -> syntax
;; Given a whole program, expands it, and annotates each
;; procedure application with a call to log-debug.
(define (annotate-applications stx)
  (define expanded (expand stx))
  
  ;; The case analysis is done based on the grammar of
  ;; fully expanded programs.
  ;; http://docs.racket-lang.org/reference/syntax-model.html#(part._fully-expanded)
  
  (kernel-syntax-case (syntax-disarm expanded code-insp) #f
    [(#%expression expr)
     (quasisyntax/loc stx
       (#%expression #,(on-expr #'expr)))]
    
    [(module id name-id (#%plain-module-begin module-level-form ...))
     (quasisyntax/loc stx
       (module id name-id (#%plain-module-begin 
                           #,@(map on-module-level
                                   (syntax->list #'(module-level-form ...))))))]
    
    [(begin top-level-form ...)
     (quasisyntax/loc stx
       (begin #,@(map on-toplevel 
                      (syntax->list #'(top-level-form ...)))))]
    [else
     (on-toplevel expanded)]))


;; on-module-level: module-level-form-syntax -> syntax
(define (on-module-level stx)
  (kernel-syntax-case (syntax-disarm stx code-insp) #f
    [(#%provide raw-provide-spec ...)
     stx]
    [else
     (on-toplevel stx)]))
    

;; on-toplevel: general-top-level-form: syntax -> syntax
(define (on-toplevel stx)
  (kernel-syntax-case (syntax-disarm stx code-insp) #f
    [(#%provide raw-provide-spec ...)
     stx]
    
    [(#%require raw-require-spec ...)
     stx]
    
    [(define-values ids expr)
     (quasisyntax/loc stx 
       (define-values ids #,(on-expr #'expr)))]
    
    [(define-syntaxes ids expr)
     (quasisyntax/loc stx 
       (define-syntaxes ids #,(on-expr #'expr)))]
    
    [(define-values-for-syntax ids expr)
     (quasisyntax/loc stx 
       (define-values-for-syntax ids #,(on-expr #'expr)))]
    
    [else
     (on-expr stx)]))


;; on-expr: expr-syntax -> syntax
(define (on-expr expr)
  (kernel-syntax-case (syntax-disarm expr code-insp) #f
    
    [(#%plain-lambda formals subexpr ...)
     (quasisyntax/loc expr
       (#%plain-lambda formals #,@(map on-expr (syntax->list #'(subexpr ...)))))]
    
    [(case-lambda case-lambda-clauses ...)
     (quasisyntax/loc expr
       (case-lambda #,@(map (lambda (a-clause)
                              (syntax-case (syntax-disarm a-clause code-insp) ()
                                [(formals subexpr ...)
                                 (quasisyntax/loc a-clause
                                   (formals #,@(map on-expr #'(subexpr ...))))]))
                            (syntax->list #'(case-lambda-clauses ...)))))]
    
    [(if test true-part false-part)
     (quasisyntax/loc expr
       (if #,(on-expr #'test)
           #,(on-expr #'true-part)
           #,(on-expr #'false-part)))]
    
    [(begin subexpr ...)
     (quasisyntax/loc expr
       (begin #,@(map on-expr (syntax->list #'(subexpr ...)))))]
    
    [(begin0 subexpr ...)
     (quasisyntax/loc expr
       (begin0 #,@(map on-expr (syntax->list #'(subexpr ...)))))]
    
    [(let-values bindingss body ...)
     (quasisyntax/loc expr
       (let-values #,(syntax-case (syntax-disarm #'bindingss code-insp) ()
                       [(binding ...)
                        (quasisyntax/loc #'bindings
                          (#,@(map (lambda (binding) 
                                     (syntax-case (syntax-disarm binding code-insp) ()
                                       [(ids expr)
                                        (quasisyntax/loc binding
                                          (ids #,(on-expr #'expr)))]))
                                   (syntax->list #'(binding ...)))))])
         #,@(map on-expr (syntax->list #'(body ...)))))]
    
    [(letrec-values bindingss body ...)
     (quasisyntax/loc expr
       (letrec-values #,(syntax-case (syntax-disarm #'bindingss code-insp) ()
                          [(binding ...)
                           (quasisyntax/loc #'bindings
                             (#,@(map (lambda (binding) 
                                        (syntax-case (syntax-disarm binding code-insp) ()
                                          [(ids expr)
                                           (quasisyntax/loc binding
                                             (ids #,(on-expr #'expr)))]))
                                      (syntax->list #'(binding ...)))))])
         #,@(map on-expr (syntax->list #'(body ...)))))]
    
    [(set! id subexpr)
     (quasisyntax/loc expr
       (set! id #,(on-expr #'subexpr)))]
    
    [(quote datum)
     expr]
    
    [(quote-syntax datum)
     expr]
    
    [(with-continuation-mark key value body)
     (quasisyntax/loc expr
       (with-continuation-mark #,(on-expr #'key) #,(on-expr #'value) 
         #,(on-expr #'body)))]
    
    
    ;; Here's where we do our work.
    [(#%plain-app)
     expr]
    
    [(#%plain-app proc-expr arg-expr ...)
     (with-syntax ([(transformed-proc-expr transformed-arg-expr ...)
                    (map on-expr (syntax->list #'(proc-expr arg-expr ...)))])
       (quasisyntax/loc expr
         (let ([proc-value transformed-proc-expr])
           (log-debug (format "Applying ~s" 
                              (object-name proc-value)))
           (#%plain-app proc-value transformed-arg-expr ...))))]

    
    [(#%top . id)
     expr]
    
    [(#%variable-reference (#%top . id))
     expr]
    
    [(#%variable-reference id)
     expr]
    
    [(#%variable-reference)
     expr]
    
    [else 
     expr]))
