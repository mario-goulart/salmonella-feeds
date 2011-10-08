(import irregex)
(use atom rfc3339 ports files posix salmonella salmonella-log-parser setup-api)

(define (rfc3339-now)
  (let* ((now (seconds->local-time (current-seconds)))
         ($ (lambda (pos) (vector-ref now pos))))
    (rfc3339->string (make-rfc3339 (+ ($ 5) 1900)
                                   (+ ($ 4) 1)
                                   ($ 3)
                                   ($ 2)
                                   ($ 1)
                                   ($ 0)
                                   0
                                   0))))

(define (feed-id egg #!optional action)
  (sprintf "tests.call-cc.org:salmonella:~a:~a:~a~a"
           egg
           (or action "egg")
           (current-seconds)
           (current-milliseconds)))

(define ok "[ok]")
(define fail "[fail]")


(define (report-link egg section salmonella-report-uri)
  (make-link
   uri: (make-pathname (list salmonella-report-uri section)
                       (symbol->string egg)
                       "html")))


(define (egg-feed egg log feeds-dir feeds-web-dir feeds-server salmonella-report-uri)
  (write-atom-doc
   (make-atom-doc
    (make-feed
     title: (make-title (sprintf "~a egg -- Salmonella report" egg))
     authors: (list (make-author name: "salmonella-feeds"))
     updated: (rfc3339-now)
     id: (feed-id egg)
     links: (list (make-link uri: (make-pathname
                                   (list feeds-server feeds-web-dir)
                                   (symbol->string egg)
                                   "xml")))
     generator: (make-generator
                 "salmonella-feeds"
                 uri: "http://wiki.call-cc.org/egg/salmonella-feeds")
     entries: (append
               (list
                ;; Installation
                (make-entry
                 id: (feed-id egg 'install)
                 title: (make-title
                         (sprintf "Installation status: ~a"
                                  (let ((status (install-status egg log)))
                                    (if (and status (zero? status))
                                        ok
                                        fail))))
                 updated: (rfc3339-now)
                 published: (rfc3339-now)
                 links: (list (report-link egg
                                           "install"
                                           salmonella-report-uri)))
                ;; Test
                (make-entry
                 id: (feed-id egg 'test)
                 title: (make-title
                         (sprintf "Test status: ~a"
                                  (let ((status (test-status egg log)))
                                    (if (and status (zero? status))
                                        ok
                                        fail))))
                 updated: (rfc3339-now)
                 published: (rfc3339-now)
                 links: (list (report-link egg
                                           "test"
                                           salmonella-report-uri))))
               ;; Warnings
               (filter-map
                (lambda (entry)
                  (let ((action (report-action entry)))
                    (and (memq action '(check-dependencies
                                        check-category
                                        check-license
                                        check-author))
                         (eq? (report-egg entry) egg)
                         (make-entry
                          id: (feed-id egg (conc "warning:" action))
                          title: (make-title
                                  (sprintf "Warning: ~a" (report-message entry)))
                          updated: (rfc3339-now)
                          published: (rfc3339-now)
                          links: (list
                                  (make-link
                                   uri: salmonella-report-uri))))))
                log))))))


(define (write-egg-feeds! log-file feeds-dir feeds-web-dir feeds-server salmonella-report-uri)
  (let ((log (read-log-file log-file)))
    (for-each
     (lambda (egg)
       (with-output-to-file (make-pathname feeds-dir (symbol->string egg) "xml")
         (lambda ()
           (egg-feed egg log feeds-dir feeds-web-dir feeds-server salmonella-report-uri))))
     (log-eggs log))))


(define (cmd-line-arg option args)
  ;; Returns the argument associated to the command line option OPTION
  ;; in ARGS or #f if OPTION is not found in ARGS or doesn't have any
  ;; argument.
  (let ((val (any (lambda (arg)
                    (irregex-match
                     `(seq ,(->string option) "=" (submatch (* any)))
                     arg))
                  args)))
    (and val (irregex-match-substring val 1))))


(define (die . msg)
  (with-output-to-port (current-error-port)
    (lambda ()
      (for-each display msg)
      (newline)
      (flush-output)))
  (exit 1))


(define (create-dir dir)
  (unless (directory-exists? dir)
    (when (file-exists? dir)
      (die dir " is a file."))
    (parameterize ((setup-verbose-mode #f)
                   (run-verbose #f))
      (create-directory/parents dir))))


(define (usage #!optional exit-code)
  (let ((this (pathname-strip-directory (program-name))))
    (display #<#EOF
#this [ -h | --help ]
#this <options>

<options>:

--log-file=<file>
  The salmonella log file.

--feeds-dir=<dir>
  Directory where feeds are stored.

--feeds-web-dir=<dir>
  The web directory (i.e., the directory which HTTP clients request) where
  feeds are located.

--feeds-server=<server address>
  Feeds server address (e.g., "http://tests.call-cc.org")

--salmonella-report-uri=<URI>
  The URI where salmonella reports can be located.
EOF
)
    (newline)
    (when exit-code (exit exit-code))))


(let ((args (command-line-arguments)))

  (when (or (member "-h" args)
            (member "--help" args))
    (usage 0))

  (when (null? args)
    (usage 1))

  (let ((log-file (cmd-line-arg '--log-file args))
        (feeds-dir (or (cmd-line-arg '--feeds-dir args)
                       (die "Missing --feeds-dir=<dir>")))
        (feeds-web-dir (or (cmd-line-arg '--feeds-web-dir args)
                           (die "Missing --feeds-web-dir=<dir>")))
        (feeds-server (or (cmd-line-arg '--feeds-server args)
                          (die "Missing --feeds-server=<server address>")))
        (salmonella-report-uri
         (or (cmd-line-arg '--salmonella-report-uri args)
             (die "Missing --salmonella-report-uri=<URI>"))))

    (create-dir feeds-dir)

    (write-egg-feeds! log-file feeds-dir feeds-web-dir feeds-server salmonella-report-uri)))
