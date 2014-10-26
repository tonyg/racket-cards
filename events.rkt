#lang send-exp racket/gui

(provide translate-key-event
	 translate-mouse-event

	 (struct-out ui-event)
	 (struct-out mouse-event)
	 (struct-out key-event)
	 (struct-out scroll-event)

	 key-press-event?
	 key-release-event?

	 mouse-enter-event?
	 mouse-leave-event?
	 mouse-motion-event?
	 mouse-down-event?
	 mouse-up-event?)

(struct ui-event (type x y time-stamp shift? control? meta? alt? caps? mod3? mod4? mod5?) #:prefab)
(struct mouse-event ui-event (which left? middle? right?) #:prefab)
(struct key-event ui-event (code) #:prefab)
(struct scroll-event ui-event (dx dy) #:prefab)

(define most-recent-x 0)
(define most-recent-y 0)

(define (translate-key-event e)
  (define press-code {get-key-code e})
  (define release-code {get-key-release-code e})
  (match press-code
    [(or 'wheel-up 'wheel-down 'wheel-left 'wheel-right)
     (scroll-event 'scroll
		   most-recent-x most-recent-y
		   {get-time-stamp e} {get-shift-down e} {get-control-down e}
		   {get-meta-down e} {get-alt-down e} {get-caps-down e}
		   {get-mod3-down e} {get-mod4-down e} {get-mod5-down e}
		   (match press-code ['wheel-right -1] ['wheel-left 1] [_ 0])
		   (match press-code ['wheel-down -1] ['wheel-up 1] [_ 0]))]
    [_
     (key-event (if (eq? press-code 'release) 'release 'press)
		most-recent-x most-recent-y
		{get-time-stamp e} {get-shift-down e} {get-control-down e}
		{get-meta-down e} {get-alt-down e} {get-caps-down e}
		{get-mod3-down e} {get-mod4-down e} {get-mod5-down e}
		(if (eq? press-code 'release) release-code press-code))]))

(define (translate-mouse-event e)
  (define-values (event-type which)
    (match {get-event-type e}
      ['enter (values 'enter #f)]
      ['leave (values 'leave #f)]
      ['left-down (values 'down 'left)]
      ['left-up (values 'up 'left)]
      ['middle-down (values 'down 'middle)]
      ['middle-up (values 'up 'middle)]
      ['right-down (values 'down 'right)]
      ['right-up (values 'up 'right)]
      ['motion (values 'motion #f)]))
  (set! most-recent-x {get-x e})
  (set! most-recent-y {get-y e})
  (mouse-event event-type
	       most-recent-x most-recent-y
	       {get-time-stamp e} {get-shift-down e} {get-control-down e}
	       {get-meta-down e} {get-alt-down e} {get-caps-down e}
	       {get-mod3-down e} {get-mod4-down e} {get-mod5-down e}
	       which
	       {get-left-down e}
	       {get-middle-down e}
	       {get-right-down e}))

(define (key-press-event? e) (and (key-event? e) (eq? (ui-event-type e) 'press)))
(define (key-release-event? e) (and (key-event? e) (eq? (ui-event-type e) 'release)))

(define (mouse-enter-event? e) (and (mouse-event? e) (eq? (ui-event-type e) 'enter)))
(define (mouse-leave-event? e) (and (mouse-event? e) (eq? (ui-event-type e) 'leave)))
(define (mouse-motion-event? e) (and (mouse-event? e) (eq? (ui-event-type e) 'motion)))
(define (mouse-down-event? e) (and (mouse-event? e) (eq? (ui-event-type e) 'down)))
(define (mouse-up-event? e) (and (mouse-event? e) (eq? (ui-event-type e) 'up)))
