#lang racket/base

(require "log-applications.rkt"
         racket/match
         racket/path)

(define cmd-channel (make-channel))

(define (request/response cmd)
  (channel-put cmd-channel cmd)
  (channel-get cmd-channel))

(define-struct print-cmd (v))

(define t 
  (thread (lambda ()
            (let/ec break
              (let loop ()
                (sync (handle-evt cmd-channel
                                  (lambda (cmd)
                                    (cond 
                                      [(print-cmd? cmd)
                                       (displayln (print-cmd-v cmd))
                                       (channel-put cmd-channel 'ok)]
                                      [(eq? cmd 'ready?)
                                       (channel-put cmd-channel 'ok)]
                                      [(eq? cmd 'quit!)
                                       (channel-put cmd-channel 'ok)
                                       (break)]))))
                (loop))))))
                                   

(void (thread (lambda ()
                (define receiver (make-log-receiver (current-logger) 'debug))
                (let loop ()
                  (request/response (make-print-cmd (sync receiver)))
                  (loop)))))


(parameterize ([read-accept-reader #t]
               [current-namespace (make-base-namespace)])
  (match (current-command-line-arguments)
    [(vector)
     (void)]
    
    [(vector program)
     (define-values (base name dir?)
       (split-path (normalize-path program)))
     (define module-name (string->symbol
                          (regexp-replace #px"\\.\\w+$" 
                                          (path->string name) 
                                          "")))
     (parameterize ([current-directory base]
                    [current-load-relative-directory base])
       (define prog-stx 
         (call-with-input-file name 
           (lambda (ip)
             (read-syntax #f ip))))                  
       (request/response 'ready?)
       
       (eval (annotate-applications prog-stx))
       (dynamic-require `',module-name #f))]))


(printf "Press enter to quit.\n")
(void (read-line))
(void (request/response 'quit!))
(thread-wait t)