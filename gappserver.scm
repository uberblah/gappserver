#!/usr/bin/env guile
!#

(use-modules (web server)
	     (web request)
	     (web response)
	     (web uri)
	     (srfi srfi-26) ;cut
	     (srfi srfi-1) ;last
	     (ice-9 ftw)
	     (ice-9 pretty-print)
	     (ice-9 rdelim)
	     (ice-9 regex)
	     (rnrs io ports)) ;binary ports

(define sep file-name-separator-string)
;(define addr "10.30.178.161")

(define args (cdr (command-line)))
(define port
  (if (null? args)
      8080
      (begin
	(let ((fst (car args)))
	  (set! args (cdr args))
	  (string->number fst)))))

(define rootdir
  (if (null? args)
      (getcwd)
      (begin
	(let ((fst (car args)))
	  (set! args (cdr args))
	  fst))))

(display (string-append "running with root: " rootdir "\n"))

(define (tostr val)
  (let* ((str (open-output-string))
	 (toss (pretty-print val str)))
    (get-output-string str)))
	
(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (not-found request)
  (values (build-response #:code 404)
	  (tostr `(not-found ,(request-path-components request)))))

(define (no-such-method request)
  (values (build-response #:code 404)
	  (tostr `(no-such-method ,(car (request-path-components request))))))

(define (pret . vals)
  (display (string-join vals ""))
  (last vals))

(define (tifnotf val)
  (if (eqv? val #f)
      #f
      #t))

(define app-args)

(define (server-handler request body)
  (let* ((path-components (request-path-components request))
	 (empty (null? path-components))
	 (method (if empty "" (car path-components)))
	 (subpath (if empty "" (string-join (cdr path-components) sep)))
	 (path (string-append rootdir sep subpath))
	 (access (access? path R_OK))
	 (statv (if access (stat path) '()))
	 (type (if access (stat:type statv) "DNE"))
	 (isdir (if access (eq? type 'directory) #f))
	 (host (request-host request))
	 (toss (display (string-append "REQUEST for (" subpath ") from " (tostr host))))
	 (toss (set! app-args `((path . ,subpath)
				(host . ,host)
				(request . ,request)))))
    (cond
     (empty (values '((content-type . (text/plain)))
		    (pret "SERVING guide yields...\n"
			  (string-append
			   (tostr `(directory ,(scandir path)))
			   "; use /execute/item to execute an item\n"
			   "; use /download/item to download the item\n"
			   "; use /read/item to read the item's plaintext\n"))))
     ((not access) (not-found request))
     (isdir (values '((content-type . (text/plain)))
		    (pret "SERVING dir (" subpath ") as "
			  (tostr `(directory ,(scandir path))))))
     ((equal? method "execute")
      (values '((content-type . (text/plain)))
	      (pret "SERVING app (" subpath ") yields...\n"
		    (tostr (primitive-load path)))))
     ((equal? method "read")
      (values '((content-type . (text/plain)))
	      (pret "SERVING scmd (" subpath ") yields...\n"
		    (read-string (open-input-file path)))))
     ((equal? method "download")
      (values '((content-type . (application/octet-stream)))
	      (call-with-input-file path
		(lambda (in-port)
		  (get-bytevector-all in-port)))))
     (else (no-such-method request)))))

(run-server server-handler 'http `(#:port ,port #:addr ,INADDR_ANY))
