#lang racket/base

(require deta
         db
         threading
         gregor
         racket/sequence
         racket/string
         racket/contract
         "sql-connection.rkt")

(provide
 (schema-out group)
 (schema-out account)
 ;; add
 add-new-account
 add-new-group
 ;; query
 query-account
 query-accounts-all
 query-group
 query-groups-by-account
 ;; update
 update-group
 update-password
 ;; delete
 delete-account
 delete-group
 delete-group-by-name)

(define-schema group
  ([id id/f #:auto-increment #:primary-key]
   [group-id string/f #:contract non-empty-string?]
   [group-name string/f #:contract non-empty-string?]
   [user-name1 string/f #:nullable]
   [user-id1 string/f #:nullable]
   [user-name2 string/f #:nullable]
   [user-id2 string/f #:nullable]
   [msgs string/f #:nullable]
   [[delay-start 0] integer/f]
   [[delay-end 0] integer/f]
   [[enabled 0] boolean/f]
   [account string/f]))


(define-schema account
  ([account string/f #:contract non-empty-string? #:primary-key]
   [password string/f #:contract non-empty-string?]))

;;; INITIALIZE
(unless (table-exists? *llkb-connect* "accounts")
  (create-table! *llkb-connect* 'account))
(unless (table-exists? *llkb-connect* "groups")
  (create-table! *llkb-connect* 'group))


;;; INSERT
;;; insert a new group
(define (add-new-group account group-id group-name
                       [user-name1 sql-null]
                       [user-id1 sql-null]
                       [user-name2 sql-null]
                       [user-id2 sql-null]
                       [msgs ""]
                       [enabled #t]
                       [delay-start 0]
                       [delay-end 0])
  (insert-one! *llkb-connect*
               (make-group #:account account
                           #:group-name group-name
                           #:group-id group-id
                           #:user-name1 user-name1
                           #:user-id1 user-id1
                           #:user-name2 user-name2
                           #:user-id2 user-id2
                           #:msgs msgs
                           #:enabled enabled
                           #:delay-start delay-start
                           #:delay-end delay-end)))
;;; insert a new account
(define (add-new-account account password)
  (insert-one! *llkb-connect*
               (make-account #:account account
                             #:password password)))


;;; QUERY
;;; query group
(define (query-group group-id)
  (lookup *llkb-connect*
          (~> (from group #:as g)
              (where (= g.group-id ,group-id)))))
;;; query group by `account-id`
(define (query-groups-by-account account)
  (in-entities *llkb-connect*
          (~> (from group #:as g)
              (where (= g.account ,account)))))
;;; query account
(define (query-account account)
  (lookup *llkb-connect*
          (~> (from account #:as a)
              (where (= a.account ,account)))))
;;; query all accounts
(define (query-accounts-all)
  (in-entities *llkb-connect* (from account #:as a)))


;;; UPDATE
;;; update group
(define (update-group account
                      group-id group-name
                      [user-name1 sql-null]
                      [user-id1 sql-null]
                      [user-name2 sql-null]
                      [user-id2 sql-null]
                      [msgs sql-null]
                      [enabled #t]
                      [delay-start 0]
                      [delay-end 0])
  (query-exec *llkb-connect*
   (~> (from group #:as g)
       (update [group-name ,group-name]
               [enabled ,enabled]
               [msgs ,msgs]
               [delay-start ,delay-start]
               [delay-end ,delay-end]
               [user-name1 ,user-name1]
               [user-id1 ,user-id1]
               [user-name2 ,user-name2]
               [user-id2 ,user-id2])
       (where (and (= g.group-id ,group-id)
                   (= g.account ,account))))))

;;; update password
(define (update-password account password)
  (query-exec *llkb-connect*
              (~> (from account #:as a)
                  (update [password ,password])
                  (where (= a.account ,account)))))


;;; DELETE
;;; delete group by id
(define (delete-group group-id)
  (query-exec *llkb-connect*
              (delete (~> (from group #:as g)
                          (where (= g.group-id ,group-id))))))
;;; delete group by name
(define (delete-group-by-name group-name)
  (query-exec *llkb-connect*
              (delete (~> (from group #:as g)
                          (where (= g.group-name ,group-name))))))
;;; delete account by id
(define (delete-account account)
  (query-exec *llkb-connect*
              (delete (~> (from group #:as g)
                          (where (= g.account ,account)))))
  (query-exec *llkb-connect*
              (delete (~> (from account #:as a)
                          (where (= a.account ,account))))))
