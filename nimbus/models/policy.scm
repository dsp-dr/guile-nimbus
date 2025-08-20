;;; -*- mode: scheme; -*-
;;; policy.scm - Policy engine models for Nimbus IAC Platform

(define-module (nimbus models policy)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 regex)
  #:export (<policy-effect>
            policy-effect-value
            <policy-rule>
            <policy>
            <policy-binding>
            <policy-evaluation>
            make-policy-rule
            make-policy
            make-policy-binding
            make-policy-evaluation
            policy-effect-allow
            policy-effect-deny
            policy-rule-actions
            policy-rule-resources
            policy-rule-conditions
            policy-id
            policy-name
            policy-description
            policy-rules
            policy-effect
            policy-conditions
            policy-active?
            policy-created-at
            policy-updated-at
            policy-binding-id
            policy-binding-policy-id
            policy-binding-subject-type
            policy-binding-subject-id
            policy-binding-resource-pattern
            policy-binding-created-at
            policy-evaluation-id
            policy-evaluation-policy-id
            policy-evaluation-action
            policy-evaluation-resource
            policy-evaluation-subject
            policy-evaluation-allowed?
            policy-evaluation-context
            policy-evaluation-evaluated-at
            evaluate-policy
            add-rule
            remove-rule))

;; Policy Effect Enumeration
(define-class <policy-effect> ()
  (value #:init-keyword #:value
         #:getter policy-effect-value))

(define policy-effect-allow (make <policy-effect> #:value 'allow))
(define policy-effect-deny (make <policy-effect> #:value 'deny))

;; Policy Rule Class
(define-class <policy-rule> ()
  (actions #:init-keyword #:actions
           #:init-form '()
           #:accessor policy-rule-actions)
  (resources #:init-keyword #:resources
             #:init-form '()
             #:accessor policy-rule-resources)
  (conditions #:init-keyword #:conditions
              #:init-form #f
              #:accessor policy-rule-conditions))

;; Policy Class
(define-class <policy> ()
  (id #:init-keyword #:id
      #:getter policy-id)
  (name #:init-keyword #:name
        #:getter policy-name)
  (description #:init-keyword #:description
               #:getter policy-description)
  (rules #:init-keyword #:rules
         #:init-form '()
         #:accessor policy-rules)
  (effect #:init-keyword #:effect
          #:init-form policy-effect-allow
          #:accessor policy-effect)
  (conditions #:init-keyword #:conditions
              #:init-form (make-hash-table)
              #:accessor policy-conditions)
  (is-active #:init-keyword #:is-active
             #:init-form #t
             #:accessor policy-active?)
  (created-at #:init-keyword #:created-at
              #:init-form (current-date)
              #:getter policy-created-at)
  (updated-at #:init-keyword #:updated-at
              #:init-form (current-date)
              #:accessor policy-updated-at))

;; Policy Binding Class
(define-class <policy-binding> ()
  (id #:init-keyword #:id
      #:getter policy-binding-id)
  (policy-id #:init-keyword #:policy-id
             #:getter policy-binding-policy-id)
  (subject-type #:init-keyword #:subject-type
                #:getter policy-binding-subject-type)
  (subject-id #:init-keyword #:subject-id
              #:getter policy-binding-subject-id)
  (resource-pattern #:init-keyword #:resource-pattern
                    #:getter policy-binding-resource-pattern)
  (created-at #:init-keyword #:created-at
              #:init-form (current-date)
              #:getter policy-binding-created-at))

;; Policy Evaluation Class
(define-class <policy-evaluation> ()
  (id #:init-keyword #:id
      #:getter policy-evaluation-id)
  (policy-id #:init-keyword #:policy-id
             #:getter policy-evaluation-policy-id)
  (action #:init-keyword #:action
          #:getter policy-evaluation-action)
  (resource #:init-keyword #:resource
            #:getter policy-evaluation-resource)
  (subject #:init-keyword #:subject
           #:getter policy-evaluation-subject)
  (allowed #:init-keyword #:allowed
           #:accessor policy-evaluation-allowed?)
  (evaluation-context #:init-keyword #:evaluation-context
                      #:init-form (make-hash-table)
                      #:getter policy-evaluation-context)
  (evaluated-at #:init-keyword #:evaluated-at
                #:init-form (current-date)
                #:getter policy-evaluation-evaluated-at))

;; Constructor functions
(define* (make-policy-rule #:key (actions '()) (resources '()) (conditions #f))
  "Create a new policy rule"
  (make <policy-rule>
    #:actions actions
    #:resources resources
    #:conditions conditions))

(define* (make-policy #:key id name description 
                      (rules '())
                      (effect policy-effect-allow)
                      (conditions (make-hash-table))
                      (is-active #t))
  "Create a new policy instance"
  (make <policy>
    #:id id
    #:name name
    #:description description
    #:rules rules
    #:effect effect
    #:conditions conditions
    #:is-active is-active))

(define* (make-policy-binding #:key id policy-id subject-type 
                              subject-id resource-pattern)
  "Create a new policy binding"
  (make <policy-binding>
    #:id id
    #:policy-id policy-id
    #:subject-type subject-type
    #:subject-id subject-id
    #:resource-pattern resource-pattern))

(define* (make-policy-evaluation #:key id policy-id action resource 
                                 subject allowed 
                                 (evaluation-context (make-hash-table)))
  "Create a new policy evaluation record"
  (make <policy-evaluation>
    #:id id
    #:policy-id policy-id
    #:action action
    #:resource resource
    #:subject subject
    #:allowed allowed
    #:evaluation-context evaluation-context))

;; Helper function to convert glob pattern to regex
(define (glob->regex pattern)
  "Convert a glob pattern with * to a regex pattern"
  (let* ((escaped (regexp-substitute/global #f 
                                            "[.+?^${}()|\\[\\]\\\\]"
                                            pattern 
                                            'pre "\\" 0 'post))
         (with-stars (regexp-substitute/global #f 
                                               "\\*" 
                                               escaped 
                                               'pre ".*" 'post)))
    (string-append "^" with-stars "$")))

;; Pattern matching helpers
(define (matches-action? action patterns)
  "Check if action matches any pattern in the list"
  (any (lambda (pattern)
         (let ((regex (make-regexp (glob->regex pattern))))
           (regexp-exec regex action)))
       patterns))

(define (matches-resource? resource patterns)
  "Check if resource matches any pattern in the list"
  (any (lambda (pattern)
         (let ((regex (make-regexp (glob->regex pattern))))
           (regexp-exec regex resource)))
       patterns))

(define (evaluate-conditions context conditions)
  "Evaluate conditions against context"
  (if (not conditions)
      #t
      (let ((conditions-table (if (hash-table? conditions)
                                  conditions
                                  (alist->hash-table conditions))))
        (hash-fold (lambda (key expected result)
                    (and result
                         (equal? (hash-ref context key) expected)))
                  #t
                  conditions-table))))

;; Methods for Policy
(define-method (evaluate-policy (policy <policy>) action resource context)
  "Evaluate if the policy allows the action on the resource"
  (let ((effect-is-allow (eq? (policy-effect-value (policy-effect policy)) 'allow)))
    (let loop ((rules (policy-rules policy)))
      (if (null? rules)
          (not effect-is-allow)  ; If no rules match, return opposite of effect
          (let ((rule (car rules)))
            (if (and (matches-action? action (policy-rule-actions rule))
                     (matches-resource? resource (policy-rule-resources rule))
                     (evaluate-conditions context (policy-rule-conditions rule)))
                effect-is-allow  ; Rule matches, return the effect
                (loop (cdr rules))))))))  ; Try next rule

(define-method (add-rule (policy <policy>) (rule <policy-rule>))
  "Add a rule to the policy"
  (set! (policy-rules policy) 
        (append (policy-rules policy) (list rule)))
  (set! (policy-updated-at policy) (current-date)))

(define-method (remove-rule (policy <policy>) rule-index)
  "Remove a rule from the policy by index"
  (when (and (>= rule-index 0) 
             (< rule-index (length (policy-rules policy))))
    (set! (policy-rules policy)
          (append (take (policy-rules policy) rule-index)
                  (drop (policy-rules policy) (+ rule-index 1))))
    (set! (policy-updated-at policy) (current-date))))