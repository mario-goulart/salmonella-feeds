(import irregex)
(use atom rfc3339 ports files posix salmonella salmonella-log-parser setup-api)

(define ok "[ok]")
(define fail "[fail]")


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


(define (custom-install-entry egg ignore log salmonella-report-uri)
  (let ((status (install-status egg log)))
    (if (and status (zero? status))
        '()
        (list
         (make-entry
          id: (feed-id egg 'custom-install)
          title: (make-title
                  (sprintf "~a's installation status: ~a" egg fail))
          updated: (rfc3339-now)
          published: (rfc3339-now)
          links: (list (report-link egg "install" salmonella-report-uri)))))))


(define (custom-test-entry egg ignore log salmonella-report-uri)
  (if (memq 'ignore-tests ignore)
      '()
      (let ((status (test-status egg log)))
        (if (and status (zero? status))
            '()
            (list
             (make-entry
              id: (feed-id egg 'custom-test)
              title: (make-title (sprintf "~a's test status: ~a" egg fail))
              updated: (rfc3339-now)
              published: (rfc3339-now)
              links: (list (report-link egg "test" salmonella-report-uri))))))))


(define (custom-warnings-entry egg ignore log salmonella-report-uri)
  (if (memq 'ignore-warnings ignore)
      '()
      (filter-map
       (lambda (entry)
         (let ((action (report-action entry)))
           (and (memq action '(check-dependencies
                               check-category
                               check-license
                               check-author))
                (eq? (report-egg entry) egg)
                (make-entry
                 id: (feed-id egg (conc "custom-warning:" action))
                 title: (make-title
                         (sprintf "Warning: ~a" (report-message entry)))
                 updated: (rfc3339-now)
                 published: (rfc3339-now)
                 links: (list (make-link uri: salmonella-report-uri))))))
       log)))


(define (custom-feed custom-conf-file log custom-feeds-dir custom-feeds-web-dir feeds-server salmonella-report-uri)
  (let ((config-data (handle-exceptions exn
                       #f
                       (read-file custom-conf-file))))
    (if (and config-data (not (null? config-data)))
        (let ((title (and-let* ((value (alist-ref 'title config-data)))
                       (car value)))
              (eggs (filter-map (lambda (config-item)
                                  (and (eq? (car config-item) 'egg)
                                       (cdr config-item)))
                                config-data))
              (custom-file (pathname-file custom-conf-file)))
          (write-atom-doc
           (make-atom-doc
            (make-feed
             title: (make-title
                     (or title
                         (string-append "Salmonella custom feed for "
                                        custom-conf-file)))
             authors: (list (make-author name: custom-file))
             updated: (rfc3339-now)
             id: (feed-id custom-file 'custom)
             links: (list (make-link uri: (make-pathname
                                           (list feeds-server
                                                 custom-feeds-web-dir)
                                           custom-file
                                           "xml")))
             generator: (make-generator
                         "salmonella-feeds"
                         uri: "http://wiki.call-cc.org/egg/salmonella-feeds")
             entries: (fold
                       (lambda (egg/ignore k)
                         (let ((egg (if (pair? egg/ignore)
                                        (car egg/ignore)
                                        egg/ignore))
                               (ignore (if (pair? egg/ignore)
                                           (cdr egg/ignore)
                                           '())))
                           (append
                            (custom-install-entry egg ignore log salmonella-report-uri)
                            (custom-test-entry egg ignore log salmonella-report-uri)
                            (custom-warnings-entry egg ignore log salmonella-report-uri)
                            k)))
                       '()
                       eggs))))))
    ""))


(define (write-custom-feeds! log-file custom-feeds-dir custom-feeds-web-dir feeds-server salmonella-report-uri)
  (let ((log (read-log-file log-file)))
    (for-each
     (lambda (custom-conf-file)
       (with-output-to-file (make-pathname custom-feeds-dir
                                           (pathname-file custom-conf-file)
                                           "xml")
         (lambda ()
           (custom-feed custom-conf-file log custom-feeds-dir custom-feeds-web-dir feeds-server salmonella-report-uri))))
     (glob (make-pathname custom-feeds-dir "*.scm")))))


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
  Directory where to write feed files.

--custom-feeds-dir=<dir>
  Directory where custom feeds can be read from (optional).

--feeds-web-dir=<dir>
  The web directory (i.e., the directory which HTTP clients request) where
  feeds are located.

--custom-feeds-web-dir=<dir>
  The web directory (i.e., the directory which HTTP clients request) where
  custom feeds are located.

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
        (custom-feeds-dir (cmd-line-arg '--custom-feeds-dir args))
        (feeds-web-dir (or (cmd-line-arg '--feeds-web-dir args)
                           (die "Missing --feeds-web-dir=<dir>")))
        (custom-feeds-web-dir (cmd-line-arg '--custom-feeds-web-dir args))
        (feeds-server (or (cmd-line-arg '--feeds-server args)
                          (die "Missing --feeds-server=<server address>")))
        (salmonella-report-uri
         (or (cmd-line-arg '--salmonella-report-uri args)
             (die "Missing --salmonella-report-uri=<URI>"))))

    (create-dir feeds-dir)

    (when custom-feeds-dir
      (create-dir custom-feeds-dir))

    (when (and custom-feeds-dir custom-feeds-web-dir)
      (write-custom-feeds! log-file
                           custom-feeds-dir
                           custom-feeds-web-dir
                           feeds-server
                           salmonella-report-uri))

    (write-egg-feeds! log-file
                      feeds-dir
                      feeds-web-dir
                      feeds-server
                      salmonella-report-uri)))
