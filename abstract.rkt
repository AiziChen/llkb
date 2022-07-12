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
 login-check
 do-login
 do-search-user
 get-accounts-all
 get-account
 save-account
 save-group
 mdelete-account
 mdelete-group
 get-groups-by-account)


;;; login check
(define/contract (login-check req account password)
  (-> request? non-empty-string? non-empty-string? response?)
  (cond
    [(and (non-empty-string? account) (non-empty-string? password))
     (define login-check (app-login-check account password))
     (cond
       [(and login-check
             (= (hash-ref login-check 'err -1) 0)
             (hash-ref login-check 'data #f))
        =>
        (lambda (check-data)
          (define check-status (hash-ref check-data 'status -1))
          (response/json
           (hasheq 'code 200
                   'status (and (number? check-status) (= check-status 0)))))]
       [else
        (response/json
         (hasheq 'code 500
                 'msg "login check error"))])]
    [else
     (response/json
      (hasheq 'code 500
              'msg "account and password can not be empty"))]))

;;; do login
(define/contract (do-login req account password [code #f])
  (->* (request? non-empty-string? non-empty-string?) ((or/c #f non-empty-string?)) response?)
  (cond
    [(and (non-empty-string? account) (non-empty-string? password))
     (define login-rs (app-user-login account password code))
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
                                           'token token
                                           'imtoken imtoken))))]
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
              'msg "account and password can not be empty"))]))

(define (do-search-user req token search-value)
  (-> request? non-empty-string? non-empty-string? response?)
  (cond
    [(and (non-empty-string? token) (non-empty-string? search-value))
     (define search-rs (user-search token search-value))
     (cond
       [(and search-rs (= (hash-ref search-rs 'err -1) 0)
             (hash-ref search-rs 'data #f))
        =>
        (lambda (data)
          (cond
            [(= (hash-ref data 'type 0) 0)
             (response/json (hasheq 'code 200 'type 0 'msg (hash-ref data 'content "user does not exists")))]
            [else
             (response/json (hasheq 'code 200
                                    'type (hash-ref data 'type -1)
                                    'msg "search success"
                                    'data (hash-ref data 'info (hasheq))))]))]
       [else
        (response/json
         (hasheq 'code 500
                 'msg "search user occurred error"))])]
    [else
     (response/json
      (hasheq 'code 500
              'msg "token and search value can not be empty"))]))


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
       [(update-password account password)
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
           [(query-group (hash-ref group 'account #f) (hash-ref group 'group-id #f))
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
(define (mdelete-account req account)
  (cond
    [(query-account account)
     (cond
       [(delete-account account)
        (response/json (hasheq 'code 200 'msg "delete account successfull"))]
       [else
        (response/json (hasheq 'code 500 'msg "delete account occurred error"))])]
    [else
     (response/json (hasheq 'code 400 'msg "account does not exists."))]))

(define (mdelete-group req account group-id)
  (cond
    [(query-group account group-id)
     (cond
       [(delete-group account group-id)
        (response/json (hasheq 'code 200 'msg "delete group successfull"))]
       [else
        (response/json (hasheq 'code 500 'msg "delete group occurred error"))])]
    [else
     (response/json (hasheq 'code 400 'msg "group does not exists."))]))


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


