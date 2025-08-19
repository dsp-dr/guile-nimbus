;;; -*- mode: scheme; -*-
;;; secrets.scm - Secrets management models for Nimbus IAC Platform

(define-module (nimbus models secrets)
  #:use-module (oop goops)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 hash-table)
  #:use-module (gcrypt cipher)
  #:use-module (gcrypt base64)
  #:use-module (rnrs bytevectors)
  #:export (<encryption-key>
            <secret>
            <secret-access>
            make-encryption-key
            make-secret
            make-secret-access
            encryption-key-id
            encryption-key-alias
            encryption-key-material
            encryption-key-algorithm
            encryption-key-active?
            encryption-key-created-at
            encryption-key-rotated-at
            secret-id
            secret-name
            secret-namespace
            secret-encrypted-value
            secret-encryption-key-id
            secret-metadata
            secret-created-at
            secret-updated-at
            secret-version
            secret-access-id
            secret-access-secret-id
            secret-access-accessor-id
            secret-access-type
            secret-access-accessed-at
            get-cipher
            decrypt-secret
            encrypt-value
            rotate-key))

;; Encryption Key Class
(define-class <encryption-key> ()
  (id #:init-keyword #:id
      #:getter encryption-key-id)
  (key-alias #:init-keyword #:key-alias
             #:getter encryption-key-alias)
  (key-material #:init-keyword #:key-material
                #:getter encryption-key-material)
  (algorithm #:init-keyword #:algorithm
             #:init-form "AES-256-CBC"
             #:getter encryption-key-algorithm)
  (is-active #:init-keyword #:is-active
             #:init-form #t
             #:accessor encryption-key-active?)
  (created-at #:init-keyword #:created-at
              #:init-form (current-date)
              #:getter encryption-key-created-at)
  (rotated-at #:init-keyword #:rotated-at
              #:init-form #f
              #:accessor encryption-key-rotated-at))

;; Secret Class
(define-class <secret> ()
  (id #:init-keyword #:id
      #:getter secret-id)
  (name #:init-keyword #:name
        #:getter secret-name)
  (namespace #:init-keyword #:namespace
             #:getter secret-namespace)
  (encrypted-value #:init-keyword #:encrypted-value
                   #:accessor secret-encrypted-value)
  (encryption-key-id #:init-keyword #:encryption-key-id
                     #:getter secret-encryption-key-id)
  (metadata #:init-keyword #:metadata
            #:init-form (make-hash-table)
            #:accessor secret-metadata)
  (created-at #:init-keyword #:created-at
              #:init-form (current-date)
              #:getter secret-created-at)
  (updated-at #:init-keyword #:updated-at
              #:init-form (current-date)
              #:accessor secret-updated-at)
  (version #:init-keyword #:version
           #:init-form 1
           #:accessor secret-version))

;; Secret Access Class
(define-class <secret-access> ()
  (id #:init-keyword #:id
      #:getter secret-access-id)
  (secret-id #:init-keyword #:secret-id
             #:getter secret-access-secret-id)
  (accessor-id #:init-keyword #:accessor-id
               #:getter secret-access-accessor-id)
  (access-type #:init-keyword #:access-type
               #:getter secret-access-type)
  (accessed-at #:init-keyword #:accessed-at
               #:init-form (current-date)
               #:getter secret-access-accessed-at))

;; Constructor functions
(define* (make-encryption-key #:key id key-alias key-material 
                              (algorithm "AES-256-CBC")
                              (is-active #t))
  "Create a new encryption key instance"
  (make <encryption-key>
    #:id id
    #:key-alias key-alias
    #:key-material key-material
    #:algorithm algorithm
    #:is-active is-active))

(define* (make-secret #:key id name namespace encrypted-value 
                      encryption-key-id
                      (metadata (make-hash-table))
                      (version 1))
  "Create a new secret instance"
  (make <secret>
    #:id id
    #:name name
    #:namespace namespace
    #:encrypted-value encrypted-value
    #:encryption-key-id encryption-key-id
    #:metadata metadata
    #:version version))

(define* (make-secret-access #:key id secret-id accessor-id access-type)
  "Create a new secret access record"
  (make <secret-access>
    #:id id
    #:secret-id secret-id
    #:accessor-id accessor-id
    #:access-type access-type))

;; Helper function to derive an initialization vector from key material
(define (derive-iv key-material)
  "Derive a 16-byte IV from key material"
  (let* ((key-bytes (base64-decode key-material))
         (iv-length 16))
    (if (>= (bytevector-length key-bytes) iv-length)
        (bytevector-copy key-bytes 0 iv-length)
        (let ((iv (make-bytevector iv-length 0)))
          (bytevector-copy! key-bytes 0 iv 0 (bytevector-length key-bytes))
          iv))))

;; Methods for Encryption Key
(define-method (get-cipher (key <encryption-key>) #:key (encrypt? #t))
  "Get cipher object for encryption/decryption"
  (let* ((key-bytes (base64-decode (encryption-key-material key)))
         (iv (derive-iv (encryption-key-material key)))
         (cipher-spec (string->cipher (encryption-key-algorithm key))))
    (make-cipher cipher-spec 
                 key-bytes 
                 iv
                 #:direction (if encrypt? 'encrypt 'decrypt))))

;; Methods for Secret
(define-method (decrypt-secret (secret <secret>) (key <encryption-key>))
  "Decrypt the secret value using the provided encryption key"
  (let* ((cipher (get-cipher key #:encrypt? #f))
         (encrypted-bytes (base64-decode (secret-encrypted-value secret)))
         (decrypted-bytes (cipher-decrypt cipher encrypted-bytes)))
    (utf8->string decrypted-bytes)))

(define-method (encrypt-value value (key <encryption-key>))
  "Encrypt a value with the given encryption key"
  (let* ((cipher (get-cipher key #:encrypt? #t))
         (value-bytes (string->utf8 value))
         (encrypted-bytes (cipher-encrypt cipher value-bytes)))
    (base64-encode encrypted-bytes)))

;; Key rotation method
(define-method (rotate-key (key <encryption-key>) new-material)
  "Rotate an encryption key with new key material"
  (set! (encryption-key-active? key) #f)
  (set! (encryption-key-rotated-at key) (current-date))
  (make-encryption-key 
    #:id (string-append (encryption-key-id key) "-rotated")
    #:key-alias (encryption-key-alias key)
    #:key-material new-material
    #:algorithm (encryption-key-algorithm key)
    #:is-active #t))