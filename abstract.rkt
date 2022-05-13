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
 do-login
 get-accounts-all
 get-account
 save-account
 save-group
 delete-account
 get-groups-by-account)

;;; do login
(define/contract (do-login req account password)
  (-> request? non-empty-string? non-empty-string? response?)
  (cond
    [(and (non-empty-string? account) (non-empty-string? password))
     (define login-rs (app-user-login account password))
     (cond
       [(and login-rs
             (= (hash-ref login-rs 'err -1) 0)
             (hash-ref (hash-ref login-rs 'data (hasheq)) 'token #f))
        =>
        (lambda (token)
          (define user-info (get-user-info token))
          (cond
            [(and user-info
                  (= (hash-ref user-info 'err -1) 0)
                  (hash-ref (hash-ref user-info 'data (hasheq)) 'uid #f))
             =>
             (lambda (uid)
               (define user-token (get-user-token token))
               (cond
                 [(and user-token
                       (= (hash-ref user-token 'err -1) 0)
                       (hash-ref (hash-ref user-token 'data (hasheq)) 'token #f))
                  =>
                  (lambda (imtoken)
                    (response/json
                     (hasheq 'code 200
                             'data (hasheq 'userid uid
                                           'token imtoken))))]
                 [else
                  (response/json
                   (hasheq 'code 500
                           'msg "get user token error"))]))]
            [else
             (response/json
              (hasheq 'code 500
                      'msg "get user information error"))]))]
       [else
        (response/json
         (hasheq 'code 500
                 'msg "login error"))])]
    [else
     (response/json
      (hasheq 'code 500
              'msg "account or password not be null"))]))


;;; query all accounts
(define (get-accounts-all req)
  (define all-accounts (query-accounts-all))
  (cond
    [all-accounts
     (response/json
      (hasheq 'code 200
              'accounts
              (for/list ([account all-accounts])
                (hasheq 'account (account-account account)
                        'password (account-password account)))))]
    [else
     (response/json
      (hasheq 'code 500
              'msg "get accounts occured error"))]))


;;; get account by `account`
(define (get-account req account)
  (define ac (query-account account))
  (cond
    [ac
     (response/json
      (hasheq 'code 200
              'account (account-account ac)
              'password (account-password ac)))]
    [else
     (response/json
      (hasheq 'code 500
              'msg "get account occured error"))]))


;;; save or update `account`
(define (save-account req account password)
  (cond
    [(query-account account)
     (cond
       [(update-password password)
        (response/json (hasheq 'code 200 'msg "update password successful"))]
       [else
        (response/json (hasheq 'code 500 'msg "update password failed."))])]
    [(add-new-account account password)
     (response/json (hasheq 'code 200 'msg "success"))]
    [else
     (response/json (hasheq 'code 500 'msg "save account error"))]))

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
             (hash-ref group 'account #f)
             (hash-ref group 'group-id #f)
             (hash-ref group 'group-name #f)
             (hash-ref group 'user-name1 sql-null)
             (hash-ref group 'user-id1 sql-null)
             (hash-ref group 'user-name2 sql-null)
             (hash-ref group 'user-id2 sql-null)
             (hash-ref group 'msgs "")
             (hash-ref group 'enabled #t)
             (hash-ref group 'delay-start 0)
             (hash-ref group 'delay-end 0))]
           [else
            (add-new-group
             (hash-ref group 'account #f)
             (hash-ref group 'group-id #f)
             (hash-ref group 'group-name #f)
             (hash-ref group 'user-name1 sql-null)
             (hash-ref group 'user-id1 sql-null)
             (hash-ref group 'user-name2 sql-null)
             (hash-ref group 'user-id2 sql-null)
             (hash-ref group 'msgs "")
             (hash-ref group 'enabled #t)
             (hash-ref group 'delay-start 0)
             (hash-ref group 'delay-end 0))]))
       (if rs
           (response/json (hasheq 'code 200 'msg "save group success"))
           (response/json (hasheq 'code 500 'msg "save group error"))))]
    [else
     (response/json (hasheq 'code 500 'msg "parameter error"))]))

;;; delete `account`
(define (delete-account req account)
  (cond
    [(query-account account)
     (cond
       [(delete-account account)
        (response/json (hasheq 'code 200 'msg "delete account successfull"))]
       [else
        (response/json (hasheq 'code 500 'msg "delete account occurred error"))])]
    [else
     (response/json (hasheq 'code 400 'msg "account does not exists."))]))


;;; get groups by account
(define (get-groups-by-account req account)
  (cond
    [(query-groups-by-account account)
     =>
     (lambda (groups)
       (response/json
        (hasheq 'code 200
                'groups (for/list ([group groups])
                          (hasheq 'group-id (group-group-id group)
                                  'group-name (group-group-name group)
                                  'user-name1 (if (sql-null? (group-user-name1 group)) "" (group-user-name1 group))
                                  'user-id1 (if (sql-null? (group-user-id1 group)) "" (group-user-id1 group))
                                  'user-name2 (if (sql-null? (group-user-name2 group)) "" (group-user-name2 group))
                                  'user-id2 (if (sql-null? (group-user-id2 group)) "" (group-user-id2 group))
                                  'delay-start (group-delay-start group)
                                  'delay-end (group-delay-end group)
                                  'enabled (group-enabled group)
                                  'msgs (if (sql-null? (group-msgs group)) "" (group-msgs group)))))))]
    [else
     (response/json (hasheq 'code 500 'msg "get groups ocurred error"))]))


