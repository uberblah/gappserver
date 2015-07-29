#!/usr/bin/env guile
!#

#|
ARGUMENTS
  provide the name of a config file as the only argument
  if the file doesn't exist, server2 will write a template and exit
  if the file exists, server2 will launch using specified settings
  if no arguments are provided server2 will print usage string
|#

(use-modules (web server)
	     (web request)
	     (web response)
	     (web uri)
	     (srfi srfi-26) ;cut
	     (srfi srfi-1) ;last
	     (srfi srfi-98) ;environment variables
	     (ice-9 ftw)
	     (ice-9 pretty-print)
	     (ice-9 rdelim)
	     (ice-9 regex)
	     (rnrs io ports)) ;binary ports

(debug-set! width 1000)

#|UTILITY FUNCTIONS|#
(define (fatal str)
  (display str)
  (exit 1))

(define (tifnotf val)
  (if (eqv? val #f)
      #f
      #t))

(define (tostr val)
  (let* ((str (open-output-string))
	 (toss (pretty-print val str)))
    (get-output-string str)))

(define sep file-name-separator-string)

(define (pret . vals)
  (display (string-join
	    (map (lambda (x) (if (string? x) x (tostr x)))
		 vals)
	    ""))
  (last vals))

#|CONFIGURATION HANDLING|#
(define usage "Usage: server2 <config.scm>\n")

(define args (command-line))

(define cfg-name
  (if (null? (cdr args))
      (fatal usage)
      (cadr args)))

(define cfg-default-str
  (string-join
   (list "(define (not-found info)"
	 "  (values (build-response #:code 404)"
	 "          (tostr `(not-found"
	 "                   ,(assoc-ref info 'subpath)))))"
	 ""
	 "(define (invalid-command info)"
	 "  (values (build-response #:code 500) ;internal server error"
	 "          (tostr `(invalid-command"
	 "                   ,(assoc-ref info 'command)))))"
	 ""
	 "(define (no-command info)"
	 "  (let* ((isdir (assoc-ref info 'isdir))"
	 "         (path  (assoc-ref info 'path )))"
	 "    (if isdir"
	 "        (values '((content-type . (text/plain)))"
	 "                (tostr `(directory ,(scandir path))))"
	 "        (values '((content-type . (text/plain)))"
	 "                (read-string (open-input-file path))))))"
	 ""
	 "(define (make-info cfg request body)"
	 "  (let* ((uri (request-uri request))"
	 "         (subpath (or (uri-path uri) \"\"))"
	 "         (command (or (uri-query uri) 'no-command))"
	 "         (path (string-append (assoc-ref cfg 'root) sep subpath))"
	 "         (name (if (equal? subpath \"\")"
	 "                   (last (split-and-decode-uri-path subpath))"
	 "                   \"\"))"
	 "         (access (access? path R_OK))"
	 "         (statv (if access (stat path) #f))"
	 "         (type (if access (stat:type statv) #f))"
	 "         (isdir (eq? type 'directory))"
	 "         (peer (request-from request)))"
	 "    (append"
	 "      `((request . ,request)"
	 "        (body    . ,body   )"
	 "        (subpath . ,subpath)"
	 "        (command . ,command)"
	 "        (path    . ,path   )"
	 "        (exists  . ,access )"
	 "        (isdir   . ,isdir  )"
	 "        (peer    . ,peer   )"
	 "        (name    . ,name   ))"
	 "    cfg)))"
	 "#| RETURN THE CONFIG |#"
	 "`((host . ,INADDR_ANY)"
	 "  (port . 8080)"
	 (string-append "  (root . \"" (get-environment-variable "HOME") "\")")
	 "  (methods . "
	 "           ((not-found . ,not-found)"
	 "            (invalid-command . ,invalid-command)"
	 "            (no-command . ,no-command)))"
	 "  (make-info . ,make-info))")
   "\n"))

(define cfg
  (if (access? cfg-name R_OK)
      (begin
	(display (string-append "loaded config: " cfg-name "\n"))
	(primitive-load cfg-name))
      (begin
	(display cfg-default-str (open-output-file cfg-name))
	(display (string-append "wrote config: " cfg-name "\n"))
	(exit 0))))

(display "CONFIG: ") (newline)
(pretty-print cfg)

(define host      (assoc-ref cfg 'host     ))
(define port      (assoc-ref cfg 'port     ))
(define root      (assoc-ref cfg 'root     ))
(define methods   (assoc-ref cfg 'methods  ))
(define make-info (assoc-ref cfg 'make-info))

(display "HOST: ") (newline)
(pretty-print host)
(display "PORT: ") (newline)
(pretty-print port)

(define (server-handler request body)
  (let* ((info (make-info cfg request body))
	 (command (assoc-ref info 'command))
	 (action  (assoc-ref methods command))
	 (exists  (assoc-ref info 'exists )))
    (if exists
	(if (and command action)
	    (action info)
	    ((assoc-ref methods 'invalid-command) info))
	((assoc-ref methods 'not-found) info))))

(run-server server-handler 'http `(#:port ,port #:addr ,host))
