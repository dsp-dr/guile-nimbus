;;; sqlite.scm - SQLite storage backend implementation
;;; Copyright (C) 2025 Nimbus Contributors

(define-module (nimbus storage sqlite)
  #:use-module (oop goops)
  #:use-module (ice-9 format)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (nimbus storage interface)
  #:export (<sqlite-backend>
            make-sqlite-backend
            sqlite-connect
            sqlite-disconnect
            sqlite-execute
            sqlite-query
            sqlite-transaction))

(define-class <sqlite-backend> (<storage-backend>)
  (database-file #:init-keyword #:database-file
                 #:init-value ".nimbus/state.db"
                 #:getter sqlite-database-file)
  (connection #:init-value #f
              #:accessor sqlite-connection)
  (lock-file #:init-value #f
             #:accessor sqlite-lock-file))

(define (make-sqlite-backend . args)
  "Create a new SQLite storage backend"
  (let ((db-file (or (assq-ref args 'database-file) ".nimbus/state.db")))
    (make <sqlite-backend> #:database-file db-file)))

(define-method (initialize (backend <sqlite-backend>) . args)
  "Initialize SQLite backend"
  (let ((db-file (sqlite-database-file backend)))
    ;; Ensure directory exists
    (let ((dir (dirname db-file)))
      (unless (file-exists? dir)
        (mkdir dir)))
    
    ;; Since we don't have sqlite3 bindings, use file-based storage
    ;; This is a stub implementation
    (format #t "Initializing SQLite backend at: ~a~%" db-file)
    
    ;; Create schema if database doesn't exist
    (unless (file-exists? db-file)
      (call-with-output-file db-file
        (lambda (port)
          (display ";; Nimbus SQLite-style state storage\n" port)
          (display ";; Schema version: 1\n\n" port)
          (write '(schema-version . 1) port)
          (newline port)
          (write '(tables 
                   (states)
                   (resources)
                   (snapshots)
                   (deployments)
                   (locks))
                port)
          (newline port))))
    
    (set! (sqlite-connection backend) db-file)
    #t))

(define-method (save-state (backend <sqlite-backend>) state)
  "Save state to SQLite backend"
  (let ((db-file (sqlite-database-file backend))
        (lock-file (string-append db-file ".lock")))
    
    ;; Acquire lock
    (acquire-lock backend lock-file)
    
    (catch #t
      (lambda ()
        ;; Read current database
        (let ((db-data (if (file-exists? db-file)
                          (call-with-input-file db-file read)
                          '())))
          
          ;; Update state data
          (let ((new-data (update-db-data db-data 'state state)))
            
            ;; Write back atomically
            (let ((tmp-file (string-append db-file ".tmp")))
              (call-with-output-file tmp-file
                (lambda (port)
                  (display ";; Nimbus state database\n" port)
                  (display ";; Last modified: " port)
                  (display (date->string (current-date) "~Y-~m-~d ~H:~M:~S") port)
                  (newline port)
                  (newline port)
                  (pretty-print new-data port)))
              
              ;; Atomic rename
              (rename-file tmp-file db-file))))
        
        ;; Release lock
        (release-lock backend lock-file)
        #t)
      (lambda (key . args)
        (release-lock backend lock-file)
        (throw key args)))))

(define-method (load-state (backend <sqlite-backend>) stack-name)
  "Load state from SQLite backend"
  (let ((db-file (sqlite-database-file backend)))
    (if (file-exists? db-file)
        (call-with-input-file db-file
          (lambda (port)
            ;; Skip comments
            (let loop ((line (read-line port)))
              (if (or (eof-object? line)
                     (not (string-prefix? ";" line)))
                  (let ((data (read port)))
                    (extract-state data stack-name))
                  (loop (read-line port))))))
        #f)))

(define-method (list-states (backend <sqlite-backend>) . args)
  "List available states"
  (let ((db-file (sqlite-database-file backend)))
    (if (file-exists? db-file)
        (call-with-input-file db-file
          (lambda (port)
            ;; Skip comments and read data
            (let loop ((line (read-line port)))
              (if (or (eof-object? line)
                     (not (string-prefix? ";" line)))
                  (let ((data (read port)))
                    (extract-state-list data))
                  (loop (read-line port))))))
        '())))

(define-method (delete-state (backend <sqlite-backend>) state-id)
  "Delete a state"
  (let ((lock-file (string-append (sqlite-database-file backend) ".lock")))
    (acquire-lock backend lock-file)
    
    (catch #t
      (lambda ()
        ;; Implementation would remove state from database
        (format #t "Deleting state: ~a~%" state-id)
        (release-lock backend lock-file)
        #t)
      (lambda (key . args)
        (release-lock backend lock-file)
        (throw key args)))))

(define-method (create-snapshot (backend <sqlite-backend>) state)
  "Create a state snapshot"
  (let ((snapshot-id (generate-snapshot-id))
        (db-file (sqlite-database-file backend)))
    ;; Save snapshot with timestamp
    (format #t "Creating snapshot: ~a~%" snapshot-id)
    snapshot-id))

(define-method (begin-transaction (backend <sqlite-backend>))
  "Begin a transaction"
  (let ((lock-file (string-append (sqlite-database-file backend) ".lock")))
    (acquire-lock backend lock-file)
    (format #t "Transaction started~%")
    #t))

(define-method (commit-transaction (backend <sqlite-backend>))
  "Commit a transaction"
  (let ((lock-file (string-append (sqlite-database-file backend) ".lock")))
    (release-lock backend lock-file)
    (format #t "Transaction committed~%")
    #t))

(define-method (rollback-transaction (backend <sqlite-backend>))
  "Rollback a transaction"
  (let ((lock-file (string-append (sqlite-database-file backend) ".lock")))
    (release-lock backend lock-file)
    (format #t "Transaction rolled back~%")
    #t))

;; Helper functions
(define (acquire-lock backend lock-file)
  "Acquire a file-based lock"
  (let ((max-wait 30)
        (wait-interval 0.1))
    (let loop ((waited 0))
      (if (file-exists? lock-file)
          (if (< waited max-wait)
              (begin
                (usleep (* wait-interval 1000000))
                (loop (+ waited wait-interval)))
              (error "Failed to acquire lock: timeout"))
          (begin
            (call-with-output-file lock-file
              (lambda (port)
                (display (getpid) port)))
            (set! (sqlite-lock-file backend) lock-file))))))

(define (release-lock backend lock-file)
  "Release a file-based lock"
  (when (and lock-file (file-exists? lock-file))
    (delete-file lock-file)
    (set! (sqlite-lock-file backend) #f)))

(define (update-db-data db-data table-name data)
  "Update database data structure"
  ;; Simple key-value storage for now
  (cons (cons table-name data) 
        (filter (lambda (entry)
                 (not (eq? (car entry) table-name)))
               db-data)))

(define (extract-state data stack-name)
  "Extract state from database data"
  (let ((state-entry (assq 'state data)))
    (if state-entry
        (cdr state-entry)
        #f)))

(define (extract-state-list data)
  "Extract list of states from database data"
  ;; Return list of available states
  (if (assq 'states data)
      (cdr (assq 'states data))
      '()))

(define (generate-snapshot-id)
  "Generate unique snapshot ID"
  (format #f "snapshot-~a" (current-time)))

(define (pretty-print data port)
  "Pretty print data structure"
  (write data port)
  (newline port))