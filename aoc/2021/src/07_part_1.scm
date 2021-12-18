;;; R7RS Scheme

(import (only (scheme list) fold)
        (only (scheme sort) list-sort))

(define (median ns)
  (let* ((len (length ns))
         (mid (quotient len 2)))
    (if (even? len)
        (quotient (+ (list-ref ns mid) (list-ref ns (+ mid 1))) 2)
        (list-ref ns mid))))

;; Type functor fusion.
(define (total-distances x ys)
  (fold (lambda (y s) (+ (abs (- x y)) s))
        0
        ys))

;; Test input.
(define pos0 '(16 1 2 0 4 2 7 1 2 14))

(let ((med (median (list-sort < pos0))))
  (display (total-distances med pos1))
  (newline))
