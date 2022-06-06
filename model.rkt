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
(define (add-new-group group-id group-name
                       [msgs ""]
                       [enabled #t]
                       [delay-start 0]
                       [delay-end 0])
  (insert-one! *llkb-connect*
               (make-group
                           #:group-name group-name
                           #:group-id group-id
                           #:msgs msgs
                           #:enabled enabled
                           #:delay-start delay-start
                           #:delay-end delay-end)))

;;; QUERY
;;; query group
(define (query-group group-id)
  (lookup *llkb-connect*
          (~> (from group #:as g)
              (where (= g.group-id ,group-id)))))
;;; query group
(define (query-groups)
  (in-entities *llkb-connect*
          (~> (from group #:as g))))


;;; UPDATE
;;; update group
(define (update-group
                      group-id group-name
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
       (where (= g.group-id ,group-id)))))


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

