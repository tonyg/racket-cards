#lang send-exp racket/gui

(require relax)
(require relax/geometric)
(require relax/point)
(require relax/alias)

(provide costume%
	 blank-costume%
	 the-blank-costume
	 rectangle-costume%
	 group-costume%)

(define costume%
  (class object%
    (init [(actor* actor)])
    (define actor** actor*)

    (super-new)

    (define {actor self} actor**)

    (define {draw self dc}
      (void))

    (define {costume-under self p}
      #f)))

(define blank-costume%
  (class costume%
    (super-new)))

(define the-blank-costume
  (make-object blank-costume% #f))

(define rectangle-costume%
  (class costume%
    (init-field top-left bottom-right pen brush)
    (super-new)

    (define (compute-corners)
      (define x1 (@ top-left x))
      (define y1 (@ top-left y))
      (define x2 (@ bottom-right x))
      (define y2 (@ bottom-right y))
      (values (min x1 x2) (min y1 y2) (max x1 x2) (max y1 y2)))

    (define/override {draw self dc}
      (define-values (x1 y1 x2 y2) (compute-corners))
      {set-pen dc (@ pen value)}
      {set-brush dc (@ brush value)}
      {draw-rectangle dc x1 y1 (- x2 x1) (- y2 y1)})

    (define/override {costume-under self p}
      (define-values (x1 y1 x2 y2) (compute-corners))
      (define px (@ p x))
      (define py (@ p y))
      (and (<= x1 px)
	   (<= y1 py)
	   (< px x2)
	   (< py y2)
	   this))))

(define group-costume%
  (class costume%
    (init-field costumes)
    (super-new)

    (define rev-costumes (reverse costumes))

    (define/override {draw self dc}
      (for [(c (in-list costumes))] {draw c dc}))

    (define/override {costume-under self p}
      (for/or [(c (in-list rev-costumes))] {costume-under c p}))))
