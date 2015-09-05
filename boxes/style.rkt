#lang racket/base
;; CSS-like properties, implemented with parameters.

(require racket/generic)
(require struct-defaults)

(require (for-syntax racket/base
                     racket/syntax))

(module+ test
  (require racket/match)
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Components

;; A ComponentId is an Any, an equal?-comparable identifier for a component.
;;  - #f is not permitted as a ComponentId.
;;  - Usually, symbols are used, but other types are OK too.

;; A ComponentClass is an Any, an equal?-comparable classifier for a component.
;;  - Usually, symbols are used, but other types are OK too.

;; A ComponentType is a Symbol naming a type of component.

(struct component (type ;; ComponentType
                   id ;; (Option ComponentId)
                   classes ;; (ListSetof ComponentClass)
                   )
  #:transparent)
(struct component/children component (children ;; (Listof Component)
                                      )
  #:transparent)

(define component-children component/children-children)

(begin-for-syntax
  (define (type->ctor type-stx)
    (format-id type-stx ":~a" type-stx)))

(define-syntax define-component
  (lambda (stx)
    (syntax-case stx ()
      [(_ type (field ...))
       #'(define-component type (field ...) #:defaults ())]
      [(_ type (field ...) #:defaults (default ...))
       (with-syntax ((:type (type->ctor #'type)))
         #'(begin (struct type component (field ...) #:transparent)
                  (define-struct-defaults :type type (#::type [component-type 'type]
                                                      #:id [component-id #f]
                                                      #:classes [component-classes '()]
                                                      default ...))))])))

(define-syntax define-component/children
  (lambda (stx)
    (syntax-case stx ()
      [(_ type (field ...))
       #'(define-component/children type (field ...) #:defaults ())]
      [(_ type (field ...) #:defaults (default ...))
       (with-syntax ((:type (type->ctor #'type)))
         #'(begin (struct type component/children (field ...) #:transparent)
                  (define-struct-defaults :type type (#::type [component-type 'type]
                                                      #:id [component-id #f]
                                                      #:classes [component-classes '()]
                                                      default ...)
                    #:rest component/children-children)))])))

(module+ test
  (define (expand/n n stx)
    (if (zero? n)
        (syntax->datum stx)
        (expand/n (- n 1) (expand-once stx))))

  (define-component span (content))
  (define-component/children p ())
  (define-component input (name value)
    #:defaults (#:name input-name
                #:value [input-value ""]))
  (:span "hello")
  (:p "hi" (:span "there") "world")
  (:p #:id 'the-p "hi" (:span "there") "world")
  (check-equal? (match (:p #:id 'the-p "hi" (:span "there") "world")
                  [(:p kids ...) kids])
                (list "hi" (:span "there") "world"))
  (:input #:name 'name)
  (:input #:name 'name #:value "initial value")
  (check-equal? (match (:input #:name 'name #:value "initial value")
                  [(:input #:name n) n])
                'name)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Styles

;; Parent components, innermost first in the list.
;; The current component is always present as the car of the list.
(define ancestry (make-parameter '()))

;; Used to record a parent's parameters for use by inherit.
(define inherited-parameterization (make-parameter #f))

;; Accesses the value of a parameter in the parent's environment.
(define (inherit parameter)
  (call-with-parameterization (inherited-parameterization) parameter))

;; Wanted: parameters that can be compound values or update portions
;; of a compound value. For example, "margin" which should have
;; margin-top margin-right margin-bottom and margin-left partial
;; aliases.

;; Wanted: a way of defining a match expander that expands to a
;; sequence of forms, so I can write "(has-classes? foo ...)" and have
;; it expand to "#:classes (? (lambda (cs) (for/and [(c (list foo
;; ...))] (set-member? cs c))))".

;; (style [(:p kids ...)]
;;   (display 'block)
;;   (margin (:margin 0 0 0 0)))

;; (style [(? (has-classes? 'row) (:div))
;;         (:p)]
;;   (border-style (:border 'solid 'black (pixels 1))))
