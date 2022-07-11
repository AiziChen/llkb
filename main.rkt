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
   [("api" "get-accounts-all")
    #:method "get"
    get-accounts-all]
   
   [("api" "get-account" (string-arg))
    #:method "get"
    get-account]
   
   [("api" "save-account" (string-arg) (string-arg))
    #:method "get"
    save-account]
   
   [("api" "delete-account" (string-arg))
    #:method "get"
    mdelete-account]
   [("api" "delete-group" (string-arg) (string-arg))
    #:method "get"
    mdelete-group]

   [("api" "save-group")
    #:method "post"
    save-group]
   
   [("api" "get-groups-by-account" (string-arg))
    #:method "get"
    get-groups-by-account]
   
   [("api" "do-login" (string-arg) (string-arg))
    #:method "get"
    do-login]

   [("api" "do-search-user" (string-arg) (string-arg))
    #:method "get"
    do-search-user]))


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
   #:port 8929))

(with-handlers
  ([exn:break? (lambda (_) (stop))])
  (sync/enable-break never-evt))
 
