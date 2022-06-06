#lang racket/base

(require koyo/dispatch
         koyo/url
         koyo/cors
         koyo/static
         web-server/dispatch
         web-server/web-server
         web-server/servlet-dispatch
         web-server/http/json
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         racket/list
         "abstract.rkt")

;;; Dispatches
(define-values (dispatch url roles)
  (dispatch-rules+roles
   [("") (lambda (_) (response/jsexpr "server is on."))]

   [("api" "save-group")
    #:method "post"
    save-group]

   [("api" "get-group" (string-arg) (string-arg))
    #:method "get"
    get-group]
   
   [("api" "get-groups" (string-arg))
    #:method "get"
    get-groups]
   
   [("api" "delete-group" (string-arg) (string-arg))
    #:method "get"
    mdelete-group]))


(current-cors-origin "*")

(define (stack handler)
  (wrap-cors handler))


(define dispatchers
  (list
   (dispatch/servlet (stack dispatch))
   (make-static-dispatcher "static")))

(define stop
  (serve
   #:dispatch (apply sequencer:make (filter-map values dispatchers))
   #:listen-ip #f
   #:port 8990))

(with-handlers
  ([exn:break? (lambda (_) (stop))])
  (sync/enable-break never-evt))
 
