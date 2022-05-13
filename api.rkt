#lang racket/base

(require net/http-easy
         json
         racket/contract
         racket/string
         racket/hash)

(provide
 app-user-login
 get-user-info
 get-user-token)

(define *base-header*
  (hasheq 'User-Agent "Mozilla/5.0 (Linux; Android 9; 16s Build/PKQ1.190202.001; wv) AppleWebKit/537.36 (KHTML, like Gecko) Version/4.0 Chrome/70.0.3538.111 Mobile Safari/537.36 uni-app Html5Plus/1.0 (Immersed/28.0)"
          'Host "lapi.liaoliaokan.ink"
          'f-ua "app"
          'platform "android"
          'Connection "Keep-Alive"
          'Accept-Encoding "gzip"))


(define/contract (app-user-login account password)
  (-> non-empty-string? non-empty-string? (or/c eof-object? jsexpr? #f))
  (define res
   (post "http://lapi.liaoliaokan.ink/im/in/login"
         #:form
         `((username . ,account)
           (password . ,password)
           (login_type . "phone")
           (client_id . "b094c280654b50b82798a61b3bb91ddb")
           (device_id . "867401041480114")
           (platform . "android")
           (form_model . "meizu 16s")
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
    (post "http://lapi.liaoliaokan.ink/api/user/myInfo"
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
    (post "http://lapi.liaoliaokan.ink/api/user/getToken"
          #:form '()
          #:headers
          (hash-union *base-header*
                      (hasheq 'Authorization (string-append "bearer  " auth-code))
                      #:combine/key (lambda (k v1 v2) v2))))
  (if (= (response-status-code res) 200)
      (response-json res)
      #f))
