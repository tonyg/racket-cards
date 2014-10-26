#lang send-exp racket/gui

(require relax)
(require relax/geometric)
(require relax/point)
(require relax/alias)

(require "costume.rkt")

(provide morph%
	 group-morph%
	 rectangle-morph%)

(define morph%
  (class object%
    (init [(parent** parent) #f])
    (super-new)

    (define parent* #f)

    (define {parent self} parent*)

    (define {top-morph self}
      (if parent*
	  {top-morph parent*}
	  self))

    (define {invalidate! self}
      (when parent* {invalidate! parent*}))

    (define {reparent! self new-parent}
      (when parent* {remove! parent* self})
      (set! parent* new-parent)
      (when parent* {add! parent* self}))

    (when parent** {reparent! this parent**})

    (define {children self} (seteq))
    (define {constraints self [acc '()]} acc)
    (define {costume self} the-blank-costume)
    (define {handle-event self evt} (void))
    (define {activate! self} (void))
    (define {deactivate! self} (void))
    ))

(define group-morph%
  (class morph%
    (super-new)

    (define children* (seteq))
    (define constraints* (seteq))

    (define costume* #f)

    (define {add! self child}
      (set! children* (set-add children* child))
      (set! costume* #f)
      (when (not (eq? {parent child} self)) {reparent! child self}))

    (define {remove! self child}
      (set! children* (set-remove children* child))
      (set! costume* #f)
      (when (eq? {parent child} self) {reparent! child #f}))

    (define/override {children self} children*)

    (define {add-constraint! self c} (set! constraints* (set-add constraints* c)))
    (define {remove-constraint! self c} (set! constraints* (set-remove constraints* c)))

    (define/override {constraints self [acc '()]}
      (for/fold [(acc (append (set->list constraints*) acc))]
	  ([child (in-set children*)])
	{constraints child acc}))

    (define/override {costume self}
      (unless costume*
	(set! costume*
	      (new group-costume%
		   [actor self]
		   [costumes (for/list ([c children*]) {costume c})])))
      costume*)
    ))

(define rectangle-morph%
  (class morph%
    (super-new)

    (field [TL (point 0 0)]
	   [BR (point 0 0)]
	   [TR (alias [x BR x] [y TL y])]
	   [BL (alias [x TL x] [y BR y])]
	   [CC (point-median TL BR)]
	   [TC (alias [x CC x] [y TL y])]
	   [BC (alias [x CC x] [y BR y])]
	   [CL (alias [x TL x] [y CC y])]
	   [CR (alias [x BR x] [y CC y])])

    (define brush-box (box {find-or-create-brush the-brush-list "yellow" 'solid}))

    (define costume* (new rectangle-costume%
			  [actor this]
			  [top-left TL]
			  [bottom-right BR]
			  [pen (box {find-or-create-pen the-pen-list "black" 1 'solid})]
			  [brush brush-box]))

    (define/override {costume self} costume*)

    (define/override {activate! self}
      (set-box! brush-box {find-or-create-brush the-brush-list "green" 'solid})
      {invalidate! self})

    (define/override {deactivate! self}
      (set-box! brush-box {find-or-create-brush the-brush-list "yellow" 'solid})
      {invalidate! self})
    ))
