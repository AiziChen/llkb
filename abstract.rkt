#lang racket/base

(require koyo/http
         koyo/json
         web-server/servlet
         web-server/http/redirect
         json
         db
         racket/format
         racket/contract
         racket/string
         "api.rkt"
         "model.rkt")

(provide
 save-group
 get-group
 mdelete-group
 get-groups)

;;; save or update `group`
(define (save-group req)
  (define group (request-post-data/raw req))
  (cond
    [(and group (bytes->jsexpr group))
     =>
     (lambda (group)
       (define rs
         (cond
           [(query-group (hash-ref group 'group-id #f))
            (update-group
             (hash-ref group 'account-id #f)
             (hash-ref group 'group-id #f)
             (hash-ref group 'group-name #f)
             (hash-ref group 'msgs "")
             (hash-ref group 'enabled #t)
             (hash-ref group 'delay-start 0)
             (hash-ref group 'delay-end 0))]
           [else
            (add-new-group
             (hash-ref group 'account-id #f)
             (hash-ref group 'group-id #f)
             (hash-ref group 'group-name #f)
             (hash-ref group 'msgs "")
             (hash-ref group 'enabled #t)
             (hash-ref group 'delay-start 0)
             (hash-ref group 'delay-end 0))]))
       (if rs
           (response/json (hasheq 'code 200 'msg "save group success"))
           (response/json (hasheq 'code 500 'msg "save group error"))))]
    [else
     (response/json (hasheq 'code 500 'msg "parameter error"))]))


;;; get groups
(define (get-groups req account-id)
  (cond
    [(query-groups account-id)
     =>
     (lambda (groups)
       (response/json
        (hasheq 'code 200
                'groups (for/list ([group groups])
                          (hasheq 'account-id (group-account-id group)
                                  'group-id (group-group-id group)
                                  'group-name (group-group-name group)
                                  'delay-start (group-delay-start group)
                                  'delay-end (group-delay-end group)
                                  'enabled (group-enabled group)
                                  'msgs (if (sql-null? (group-msgs group)) "" (group-msgs group)))))))]
    [else
     (response/json (hasheq 'code 500 'msg "get groups ocurred error"))]))


;;; get group
(define (get-group req account-id group-id)
  (cond
    [(query-group group-id)
     =>
     (lambda (group)
       (response/json
        (hasheq 'code 200
                'group 
                (hasheq 'account-id (group-account-id group)
                        'group-id (group-group-id group)
                        'group-name (group-group-name group)
                        'delay-start (group-delay-start group)
                        'delay-end (group-delay-end group)
                        'enabled (group-enabled group)
                        'msgs (if (sql-null? (group-msgs group)) "" (group-msgs group))))))]
    [else
     (response/json (hasheq 'code 500 'msg "get group ocurred error"))]))

;; delete group
(define (mdelete-group req account-id group-id)
  (cond
    [(query-group account-id group-id)
     =>
     (lambda (group)
       (if (mdelete-group account-id group-id)
           (response/json (hasheq 'code 200 'msg "delete group successfully"))
           (response/json (hasheq 'code 500 'msg "delete group failed"))))]
    [else
     (response/json (hasheq 'code 500 'msg "delete group failed: group not found"))]))

