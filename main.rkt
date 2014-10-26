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
  (rectangle-dimensions-mixin
   (class canvas%
     (field [TL (readonly-view (point 0 0))]
     	    [BR (readonly-view (point 0 0))])

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
       (list (fixed-coordinate-constraint (readonly-view-underlying TL) (point 0 0))
       	     (fixed-coordinate-constraint (readonly-view-underlying BR) current-size)))

     (define/override (on-paint)
       (when morph
	 (solve-constraints!)
	 (define dc {get-dc this})
	 {set-background dc "black"}
	 {clear dc}
	 {set-smoothing dc 'smoothed}
	 (define costume {costume morph})
	 {draw costume dc}))

     (define (solve-constraints!)
       (when morph
	 (define cs {constraints morph base-constraints})
	 (time (solve-constraints/timeout! cs 100))))

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
     )))

(define (general-chain! constraint-maker holder morphs)
  (when (pair? morphs)
    (define container (or holder {parent (car morphs)}))
    (let loop ((prev (car morphs)) (rest (cdr morphs)))
      (when (pair? rest)
	(define next (car rest))
	{add-constraint! container (constraint-maker prev next)}
	(loop next (cdr rest))))))

(define (general-abut! prev-anchor-getter next-anchor-getter spacing holder morphs)
  (define spacing-box (box spacing))
  (general-chain! (lambda (p n) (scalar-difference-constraint (prev-anchor-getter p)
							      (next-anchor-getter n)
							      spacing-box))
		  holder
		  morphs))

(define (abut-horizontally! #:spacing [spacing 0] #:constraint-holder [holder #f] . morphs)
  (general-abut! {value-R} {value-L} spacing holder morphs))

(define (abut-vertically! #:spacing [spacing 0] #:constraint-holder [holder #f] . morphs)
  (general-abut! {value-B} {value-T} spacing holder morphs))

(define (align! #:constraint-holder [holder #f] anchor-getter . morphs)
  (general-abut! anchor-getter anchor-getter 0 holder morphs))

(define (pin! #:offset [offset (point 0 0)] #:constraint-holder [holder #f]
	      target anchor-getter . morphs)
  (for [(morph (in-list morphs))]
    {add-constraint! (or holder {parent morph})
		     (point-difference-constraint target (anchor-getter morph) offset)}))

(define v (new view% [parent frame]))
(define g (new group-morph% [parent v]))
(let ((r1 (new rectangle-morph% [parent g]))
      (r2 (new rectangle-morph% [parent g]))
      (r3 (new rectangle-morph% [parent g])))

  (pin! {point-TL v} #:offset (point 20 20) {point-TL} r1)
  (pin! {point-BR v} #:offset (point -20 -20) {point-BR} r2)

  (align! {value-L} r1 r2)
  (align! {value-R} r1 r2)
  (abut-vertically! r1 r2)

  ;; {add-constraint! g (scalar-difference-constraint {value-T r1} {value-B r1} (box 50))}
  ;; {add-constraint! g (scalar-difference-constraint {value-T r2} {value-B r2} (box 50))}

  {add-constraint! g (equivalence-constraint {point-TL r1} {point-BR r1}
  					     {point-TL r2} {point-BR r2})}

  (pin! {point-CC v} {point-CC} r3)
  (pin! {point-TL r3} #:offset (point 100 200) {point-BR} r3)

  (void))

{focus v}
{show frame #t}
