#lang racket/base

(require net/http-easy
         json
         racket/contract
         racket/string
         racket/hash)

(provide
 app-user-login
 app-login-check
 get-user-info
 get-user-token
 user-search)

(define *base-header*
  (hasheq 'User-Agent "Mozilla/5.0 (Linux; Android 9; 16s Build/PKQ1.190202.001; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/70.0.3538.111 Mobile Safari/537.36 uni-app Html5Plus/1.0 (Immersed/28.0)"
          'Host "limg.liaoliaokan.ink"
          'f-ua "app"
          'platform "android"
          'Connection "Keep-Alive"
          'Accept-Encoding "gzip"))
(define *base-host* "http://limg.liaoliaokan.ink")


(define/contract (app-user-login account password code)
  (-> non-empty-string? non-empty-string? (or/c #f non-empty-string?) (or/c eof-object? jsexpr? #f))
  (define params
    `((username . ,account)
            (password . ,password)
            (login_type . "phone")
            (client_id . "94611482affcf395458f775ef4cc1094")
            (deviceId . "867401041480114,867401041480122")
            (platform . "android")
            (form_model . "meizu/16s")
            (system_version . "Android 9")
            (form . "app")))
  (define res
    (post (string-append *base-host* "/im/in/login")
          #:form
          (if code (cons `(code . ,code) params) params)
          #:auth (bearer-auth "")
          #:headers *base-header*))
  (if (= (response-status-code res) 200)
      (response-json res)
      #f))

(define/contract (app-login-check account password)
  (-> non-empty-string? non-empty-string? (or/c eof-object? jsexpr? #f))
  (define res
    (post (string-append *base-host* "/im/in/check")
          #:form
          `((username . ,account)
            (password . ,password)
            (login_type . "phone")
            (client_id . "94611482affcf395458f775ef4cc1094")
            (deviceId . "867401041480114,867401041480122")
            (platform . "android")
            (form_model . "meizu/16s")
            (system_version . "Android 9")
            (form . "app"))
          #:auth (bearer-auth "")
          #:headers *base-header*))
  (if (= (response-status-code res) 200)
      (response-json res)
      #f))


(define/contract (get-user-info auth-code)
  (-> non-empty-string? (or/c eof-object? jsexpr? #f))
  (define res
    (post (string-append *base-host* "/api/user/myInfo")
          #:form '()
          #:headers
          (hash-union *base-header*
                      (hasheq 'Authorization (string-append "bearer  " auth-code))
                      #:combine/key (lambda (k v1 v2) v2))))
  (if (= (response-status-code res) 200)
      (response-json res)
      #f))


(define/contract (get-user-token auth-code)
  (-> non-empty-string? (or/c eof-object? jsexpr? #f))
  (define res
    (post (string-append *base-host* "/api/user/getToken")
          #:form '()
          #:headers
          (hash-union *base-header*
                      (hasheq 'Authorization (string-append "bearer  " auth-code))
                      #:combine/key (lambda (k v1 v2) v2))))
  (if (= (response-status-code res) 200)
      (response-json res)
      #f))


(define/contract (user-search auth-code search-value)
  (-> non-empty-string? non-empty-string? (or/c eof-object? jsexpr? #f))
  (define res
    (post (string-append *base-host* "/api/user/search")
          #:form `((searchValue . ,search-value))
          #:headers
          (hash-union *base-header*
                      (hasheq 'Authorization (string-append "bearer  " auth-code))
                      #:combine/key (lambda (k v1 v2) v2))))
  (if (= (response-status-code res) 200)
      (response-json res)
      #f))
