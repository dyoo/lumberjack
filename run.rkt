#lang racket/base

(require "log-applications.rkt"
         racket/match
         racket/path)

(define handshake (make-channel))
(define t 
 (thread (lambda ()
           (define receiver (make-log-receiver (current-logger) 'debug))
           (define (loop)
             (displayln (sync receiver))
             (loop))
           (channel-get handshake)
           (loop))))

(channel-put handshake 'program-done)
(parameterize ([read-accept-reader #t]
               [current-namespace (make-base-namespace)])
  (match (current-command-line-arguments)
    [(vector)
     (void)]
    
    [(vector program)
     (define-values (base name dir?)
       (split-path (normalize-path program)))
     (parameterize ([current-directory base]
                    [current-load-relative-directory base])
       (define prog-stx 
         (call-with-input-file name 
           (lambda (ip)
             (read-syntax #f ip))))                  

       (eval (annotate-applications prog-stx))
       (dynamic-require ''foo #f))]))

(thread-wait t)
