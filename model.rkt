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
 ;; add
 add-new-group
 ;; query
 query-group
 query-groups
 ;; update
 update-group
 ;; delete
 delete-group
 delete-group-by-name)

(define-schema group
  ([id id/f #:auto-increment #:primary-key]
   [account-id string/f #:contract non-empty-string?]
   [group-id string/f #:contract non-empty-string?]
   [group-name string/f #:contract non-empty-string?]
   [msgs string/f #:nullable]
   [[delay-start 0] integer/f]
   [[delay-end 0] integer/f]
   [[enabled 0] boolean/f]))


;;; INITIALIZE
(unless (table-exists? *llkb-connect* "groups")
  (create-table! *llkb-connect* 'group))


;;; INSERT
;;; insert a new group
(define (add-new-group account-id group-id group-name
                       [msgs ""]
                       [enabled #t]
                       [delay-start 0]
                       [delay-end 0])
  (insert-one! *llkb-connect*
               (make-group #:account-id account-id
                           #:group-name group-name
                           #:group-id group-id
                           #:msgs msgs
                           #:enabled enabled
                           #:delay-start delay-start
                           #:delay-end delay-end)))

;;; QUERY
;;; query group
(define (query-group account-id group-id)
  (lookup *llkb-connect*
          (~> (from group #:as g)
              (where (and (= g.group-id ,group-id)
                          (= g.account-id ,account-id))))))
;;; query group
(define (query-groups account-id)
  (in-entities *llkb-connect*
               (~> (from group #:as g)
                   (where (= g.account-id ,account-id)))))


;;; UPDATE
;;; update group
(define (update-group account-id group-id group-name
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
                          [delay-end ,delay-end])
                  (where (and (= g.group-id ,group-id)
                              (= g.account-id ,account-id))))))


;;; DELETE
;;; delete group by id
(define (delete-group account-id group-id)
  (query-exec *llkb-connect*
              (delete (~> (from group #:as g)
                          (where (and (= g.group-id ,group-id)
                                      (= g.account-id ,account-id)))))))
;;; delete group by name
(define (delete-group-by-name account-id group-name)
  (query-exec *llkb-connect*
              (delete (~> (from group #:as g)
                          (where (and (= g.group-name ,group-name)
                                      (= g.account-id ,account-id)))))))

