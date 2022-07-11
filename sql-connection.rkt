#lang racket/base

(require db)

(provide
 *llkb-connect*)

(define *llkb-connect*
  (virtual-connection
   (connection-pool
    (lambda ()
      (postgresql-connect #:database "cljg"
                          #:user "postgres"
                          #:password "quanyec")))))

