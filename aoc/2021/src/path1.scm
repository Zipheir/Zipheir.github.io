;;; miniKanren solution using the CHICKEN mini-kanren egg (a
;;; slightly old-fashioned version without some of the TRS2ed forms).
;;; Unfortunately, this is too slow to find all of the solutions to
;;; a full-sized graph (e.g. the real puzzle input).  Perhaps tuning
;;; the arco "lookup relation" would help.
(import mini-kanren)

(define (patho a b p)
  (tracko a b `(,a) p))

(define (tracko a c pi po)
  (fresh (b q)
    (conde ((== a c) (== pi po))
           ((arco a b)
            (conda ((smallmemo b pi) fail)
                   ((conso b pi q)
                    (tracko b c q po)))))))

(define (smallmemo x p)
  (fresh ()  ; no conj in old mini-kanren
    (smallo x)
    (membero x p)))

;; Sample graph:
;;
;;     start
;;     /   \
;; c--A-----b--d
;;     \   /
;;      end
;;
(define (arco x y)
  (assoco x y arcs))

(define arcs
  '((start . A)
    (start . b)
    (A . c)
    (A . b)
    (b . d)
    (A . end)
    (b . end)))

;; Succeeds if (x . y) or (y . x) is in as.
;; (true relational assq)
(define (assoco k v as)
  (fresh (p)
    (conde ((== p `(,k . ,v)) (membero p as))
           ((== p `(,v . ,k)) (membero p as)))))

;; Succeeds if x is a "small" cave.
(define (smallo x)
  (membero x '(start b c d end)))

(display (length (run* (p) (patho 'start 'end p))))
(newline)
