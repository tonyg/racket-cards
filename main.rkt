#lang send-exp racket/gui

(require relax)
(require relax/geometric)
(require relax/point)
(require relax/alias)

(require "costume.rkt")
(require "events.rkt")
(require "morphs.rkt")

(define frame (new frame%
		   [label "The Frame"]
		   [width 480]
		   [height 700]))

(define mouse-point (readonly-view (point 0 0)))

(define view%
  (class (rectangle-dimensions-mixin canvas%)
    (inherit-field TL BR)
    (super-new)

    (define morph #f)
    (define active-morph #f)
    (define current-size (point 0 0))

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
(let ((r1 (new rectangle-morph% [parent g]))
      #;(r2 (new rectangle-morph% [parent g])))

  {add-constraint! g (point-difference-constraint {point-TL v} {point-TL r1} (point 32 16))}
  {add-constraint! g (point-difference-constraint {point-BR r1} {point-BR v} (point 16 16))}

  (void))

{focus v}
{show frame #t}
