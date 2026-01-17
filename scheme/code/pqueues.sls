;;; $Id: pqueues.sls,v 1.18 2026/01/17 18:50:49 wcm Exp $
;;;
;;; Priority queue library based on King (1994) and Brodal &
;;; Okasaki (1996).
;;;
;;; SPDX-FileCopyrightText: 2024--2026 Wolfgang Corcoran-Mathe
;;;
;;; SPDX-License-Identifier: MIT

(library (pqueues)
  (export pqueue
          pqueue-element-comparator
          pqueue-length
          pqueue?
          pqueue-empty?
          pqueue-insert
          pqueue-meld
          pqueue-min
          pqueue-delete-min
          pqueue-pop
          pqueue-fold-left
          pqueue-fold-right
          )
  (import (rnrs base (6))
          (rnrs control (6))
          (prefix (srfi :1 lists) s1:)
          (prefix (srfi :9 records) s9:)
          (prefix (srfi :128 comparators) s128:)
          )

  ;;;; Utility

  ;; Right fold over a non-empty list, using the final element of *xs*
  ;; as the initial seed.
  (define (fold-right1 proc xs)
    (define (fold-it xs)
      (if (null? (cdr xs))
          (car xs)
          (proc (car xs) (fold-it (cdr xs)))))

    (assert (pair? xs))
    (fold-it xs))

  (define (snoc xs x)
    (append xs (list x)))

  ;;;; Binomial trees & forests

  ;; Root nodes store a tree's rank as well as the usual data.
  (s9:define-record-type <root>
    (make-root element rank children)
    root?
    (element root-element)
    (rank root-rank)
    (children root-children))

  (s9:define-record-type <node>
    (make-node element children)
    node?
    (element node-element)
    (children node-children))

  (define (root->internal-node root)
    (make-node (root-element root) (root-children root)))

  (define (internal-node->root rank node)
    (make-root (node-element node)
               rank
               (node-children node)))

  ;; Link two binomial trees of rank r to form a new tree of rank r + 1
  ;; while maintaining the heap-ordering property.
  (define (link element<? t1 t2)
    (assert (= (root-rank t1) (root-rank t2)))
    (let ((x1 (root-element t1))
          (cs1 (root-children t1))
          (x2 (root-element t2))
          (cs2 (root-children t2)))
      (if (element<? x1 x2)
          (let ((t2* (root->internal-node t2))
                (r (+ (root-rank t1) 1)))
            (make-root x1 r (cons t2* cs1)))
          (let ((t1* (root->internal-node t1))
                (r (+ (root-rank t2) 1)))
            (make-root x2 r (cons t1* cs2))))))

  (define (insert-tree element<? tree trees)
    (if (null? trees)
        (list tree)
        (let ((t (car trees)) (ts (cdr trees)))
          (if (< (root-rank tree) (root-rank t))
              (s1:cons* tree t ts)
              (insert-tree element<? (link element<? tree t) ts)))))

  (define (forest-insert-element element<? obj trees)
    (insert-tree element<? (make-root obj 0 '()) trees))

  (define (forest-meld element<? trees1 trees2)
    (define (meld ts1 ts2)
      (cond ((null? ts1) ts2)
            ((null? ts2) ts1)
            (else (meld-non-empty ts1 ts2))))

    (define (meld-non-empty ts1 ts2)
      (let* ((t1 (car ts1))
             (ts1* (cdr ts1))
             (t2 (car ts2))
             (ts2* (cdr ts2))
             (r1 (root-rank t1))
             (r2 (root-rank t2)))
        (cond ((< r1 r2) (cons t1 (meld ts1* ts2)))
              ((< r2 r1) (cons t2 (meld ts1 ts2*)))
              (else  ; r1 = r2
               (insert-tree element<?
                            (link element<? t1 t2)
                            (meld ts1* ts2*))))))

    (meld trees1 trees2))

  ;; Returns the tree in *trees* with the least root element.
  (define (forest-min-tree element<? trees)
    (assert (pair? trees))
    (let ((t (car trees)) (ts (cdr trees)))
      (let loop ((t-min t) (ts ts))
        (if (null? ts)
            t-min
            (let ((t-new (car ts)) (rest (cdr ts)))
              (loop (if (element<? (root-element t-min)
                                   (root-element t-new))
                        t-min
                        t-new)
                    rest))))))

  (define (forest-find-min element<? trees)
    (root-element (forest-min-tree element<? trees)))

  ;; Meld a forest of child trees into *trees*, assigning them
  ;; ranks rank, rank-1, ..., 0.
  (define (insert-children element<? children trees rank)
    ;; Fusion of
    ;; (reverse (map internal-node->root (r (- r 1) ... 0) children))
    (define (make-roots r ts roots)
      (if (null? ts)
          roots
          (let ((t (internal-node->root r (car ts))))
            (make-roots (- r 1) (cdr ts) (cons t roots)))))

    (forest-meld element<?
                 (make-roots rank children '())
                 trees))

  ;; Find & pop the least element contained in *trees*.  Returns the
  ;; element and the updated forest as values.
  (define (forest-pop-min element<? trees)
    (define (pop-min ts)
      (assert (pair? ts))
      (let ((t (car ts)) (rest (cdr ts)))
        (if (null? rest)
            (values t '())
            (let-values (((t* ts*) (pop-min rest)))
              (if (element<? (root-element t) (root-element t*))
                  (values t rest)
                  (values t* (cons t ts*)))))))

    (let*-values (((tree rest) (pop-min trees))
                  ((r) (- (root-rank tree) 1))
                  ((trees*)
                   (insert-children element<?
                                    (root-children tree)
                                    rest
                                    r)))
      (values (root-element tree) trees*)))

  ;; Returns the number of elements in *trees*.
  (define (forest-size trees)
    (s1:fold (lambda (t k) (+ k (expt 2 (root-rank t))))
             0
             trees))

  ;;;; Priority queues

  (s9:define-record-type <pqueue>
    (make-pqueue comparator forest)
    pqueue?
    (comparator pqueue-element-comparator)
    (forest pqueue-forest))

  ;;; Fused accessors for pqueue comparator procs

  (define (pqueue-type-predicate pq)
    (s128:comparator-type-test-predicate
     (pqueue-element-comparator pq)))

  (define (pqueue-ordering-predicate pq)
    (s128:comparator-ordering-predicate
     (pqueue-element-comparator pq)))
     
  ;; If *obj* isn't of the right type for *pq*, raise an exception.
  (define (pqueue-check-element who pq obj)
    (unless ((pqueue-type-predicate pq) obj)
      (assertion-violation who
                           "object of incorrect type for priority queue"
                           obj
                           pq)))

  ;;;; Queue operations

  ;;; Constructors

  (define (pqueue comparator . elements)
    (assert (s128:comparator? comparator))
    (let ((elt<? (s128:comparator-ordering-predicate comparator)))
      (define (insert elt forest)
        (unless ((s128:comparator-type-test-predicate comparator)
                 elt)
          (assertion-violation
           'pqueue
           "object of incorrect type for priority queue"
           elt))
        (forest-insert-element elt<? elt forest))
      (make-pqueue comparator
                   (s1:fold insert '() elements))))

  ;;; Predicates
  
  (define (pqueue-empty? pq)
    (assert (pqueue? pq))
    (null? (pqueue-forest pq)))

  ;;; Updaters

  (define (pqueue-insert pq . elements)
    (assert (pqueue? pq))
    (let ((elt<? (pqueue-ordering-predicate pq)))
      (define (insert elt forest)
        (pqueue-check-element 'pqueue-insert pq elt)
        (forest-insert-element elt<? elt forest))
      (make-pqueue (pqueue-element-comparator pq)
                   (s1:fold insert (pqueue-forest pq) elements))))

  (define (pqueue-meld . pqs)
    (assert (pair? pqs))
    (let* ((pq0 (car pqs))
           (comp (pqueue-element-comparator pq0))
           (elt<? (s128:comparator-ordering-predicate comp)))
      (make-pqueue comp
                   (fold-right1 (lambda (trees1 trees2)
                                  (forest-meld elt<? trees1 trees2))
                                (map pqueue-forest pqs)))))

  ;;; Accessors

  ;;; We may want additional accessors for the priority of the
  ;;; least-priority element.

  (define (pqueue-min pq)
    (assert (pqueue? pq))
    (when (pqueue-empty? pq)
      (assertion-violation 'pqueue-min "empty pqueue" pq))
    (forest-find-min (s128:comparator-ordering-predicate
                      (pqueue-element-comparator pq))
                     (pqueue-forest pq)))

  (define (pqueue-delete-min pq)
    (assert (pqueue? pq))
    (let ((ts (pqueue-forest pq)))
      (when (null? ts)
        (assertion-violation 'pqueue-delete-min "empty pqueue" pq))
      (let-values (((_e ts*) (forest-pop-min
                              (s128:comparator-ordering-predicate
                               (pqueue-element-comparator pq))
                              ts)))
        (make-pqueue (pqueue-element-comparator pq) ts*))))

  ;; Returns the number of elements in pq.
  (define (pqueue-length pq)
    (assert (pqueue? pq))
    (forest-size (pqueue-forest pq)))

  ;; Return the least element of *pq* and the rest of the queue.
  (define (pqueue-pop pq)
    (assert (pqueue? pq))
    (let*-values (((trees) (pqueue-forest pq))
                  ((e trees*)
                   (begin
                    (when (null? trees)
                      (assertion-violation 'pqueue-pop
                                           "empty pqueue"
                                           pq))
                    (forest-pop-min (pqueue-ordering-predicate pq)
                                    trees))))
      (values e
              (make-pqueue (pqueue-element-comparator pq) trees*))))

  ;;; Iteration

  (define pqueue-fold-right
    (case-lambda
      ((proc base pq)
       (pqueue-fold-right-single proc base pq))
      ((proc base . pqs)
       (pqueue-fold-right-multi proc base pqs))))

  (define (pqueue-fold-right-single proc base pq)
    (let ((elt<? (pqueue-ordering-predicate pq))
          (forest (pqueue-forest pq)))
      (define (fold-it ts)
        (if (null? ts)
            base
            (let-values (((x ts*) (forest-pop-min elt<? ts)))
              (proc x (fold-it ts*)))))
      (fold-it forest)))

  ;; Apply forest-pop-min to each of the *forests*, and return a list
  ;; of minimum elements and a list of forest "tails".
  (define (forests-pop-all elt<? forests)
    (s1:unzip2 (map (lambda (f)
                      (call-with-values (lambda ()
                                          (forest-pop-min elt<? f))
                                        list))
                    forests)))

  (define (pqueue-fold-right-multi proc base pqs)
    (define elt<? (pqueue-ordering-predicate (car pqs)))
 
    (define (foldr fs)
      (cond ((s1:any null? fs)
             (unless (s1:every null? fs)
               (assertion-violation 'pqueue-fold-right
                                    "pqueues have different lengths"
                                    pqs))
             base) ; done
            (else
             (let-values (((mins fs-tails) (forests-pop-all elt<? fs)))
               (apply proc (snoc mins (foldr fs-tails)))))))
 
    (foldr (map pqueue-forest pqs)))

  (define pqueue-fold-left
    (case-lambda
      ((proc base pq)
       (pqueue-fold-left-single proc base pq))
      ((proc base . pqs)
       (pqueue-fold-left-multi proc base pqs))))

  (define (pqueue-fold-left-single proc base pq)
    (let ((elt<? (pqueue-ordering-predicate pq))
          (forest (pqueue-forest pq)))
      (define (fold-it z ts)
        (if (null? ts)
            z
            (let-values (((x ts*) (forest-pop-min elt<? ts)))
              (fold-it (proc z x) ts*))))
      (fold-it base forest)))

  (define (pqueue-fold-left-multi proc base pqs)
    (define elt<? (pqueue-ordering-predicate (car pqs)))
 
    (define (foldl seed fs)
      (cond ((s1:any null? fs)
             (unless (s1:every null? fs)
               (assertion-violation 'pqueue-fold-left
                                    "pqueues have different lengths"
                                    pqs))
             seed) ; done
            (else
             (let-values (((mins fs-tails) (forests-pop-all elt<? fs)))
               (foldl (apply proc seed mins) fs-tails)))))

    (foldl base (map pqueue-forest pqs)))
  )
