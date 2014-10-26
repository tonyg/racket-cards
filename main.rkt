#lang send-exp racket/gui

(require relax)
(require relax/geometric)
(require relax/point)
(require relax/alias)

(require "costume.rkt")
(require "events.rkt")

(define frame (new frame%
		   [label "The Frame"]
		   [width 480]
		   [height 700]))

(define menu-bar (new menu-bar% [parent frame]))
(define edit-menu (new menu% [label "Edit"] [parent menu-bar]))
(append-editor-operation-menu-items edit-menu #f)

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

(define mouse-point (readonly-view (point 0 0)))

(define view%
  (class canvas%
    (super-new)

    (define morph #f)
    (define active-morph #f)
    (define current-size (point {get-width this} {get-height this}))
    (field [TL (point 0 0)]
	   [BR (point-copy current-size)])

    (define {add! self m}
      (when (not (eq? morph m))
	(define old-morph morph)
	(set! morph m)
	(when old-morph {reparent! old-morph #f})
	(when (not (eq? {parent m} self)) {reparent! m self})))

    (define {remove! self m}
      (when (eq? morph m)
	(define old-morph morph)
	(set! morph #f)
	(when old-morph {reparent! old-morph #f})))

    (define {invalidate! self}
      {refresh self})

    (define base-constraints
      (map (lambda (c) (prioritize-constraint 1 c))
	   (list (fixed-coordinate-constraint TL (point 0 0))
		 (fixed-coordinate-constraint BR current-size))))

    (define/override (on-paint)
      (when morph
	(solve-constraints!)
	(define dc {get-dc this})
	(define costume {costume morph})
	{draw costume dc}))

    (define (solve-constraints!)
      (when morph
	(define cs {constraints morph base-constraints})
	(solve-constraints/timeout! cs 100)))

    (define/override (on-size w h)
      (set-point-x! current-size w)
      (set-point-y! current-size h)
      (solve-constraints!))

    (define/override (on-char evt) (dispatch (translate-key-event evt)))
    (define/override (on-event evt) (dispatch (translate-mouse-event evt)))

    (define (dispatch evt)
      (set-point-x! (readonly-view-underlying mouse-point) (ui-event-x evt))
      (set-point-y! (readonly-view-underlying mouse-point) (ui-event-y evt))
      (define new-costume {costume-under {costume morph} mouse-point})
      (define new-morph (and new-costume {actor new-costume}))
      (when (not (eq? active-morph new-morph))
	(when active-morph {deactivate! active-morph})
	(set! active-morph new-morph)
	(when active-morph {activate! active-morph}))
      (log-info "DISPATCH ~v -> ~v" evt active-morph)
      (when active-morph {handle-event active-morph evt}))
    ))

(define v (new view% [parent frame]))
(define g (new group-morph% [parent v]))
(let ((r (new rectangle-morph% [parent g])))
  {add-constraint! g (difference-constraint (alias [value (get-field TL v) x])
  					    (alias [value (get-field TL r) x])
  					    (box 40))}
  {add-constraint! g (difference-constraint (alias [value (get-field TL v) y])
  					    (alias [value (get-field TL r) y])
  					    (box 20))}

  ;; {add-constraint! g (heading-constraint (get-field TL v) (get-field TL r) (/ pi 4))}
  ;; {add-constraint! g (length-constraint (get-field TL v) (get-field TL r) 20)}

  {add-constraint! g (heading-constraint (get-field BR r) (get-field BR v) (/ pi 4))}
  {add-constraint! g (length-constraint (get-field BR v) (get-field BR r) 20)}

  (void))

{focus v}
{show frame #t}
