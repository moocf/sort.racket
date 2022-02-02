#lang racket

; (list.merge pred loi1 loi2 to): F X L X L X L -> L
; merges loi1 and loi2 using pred to list "to"
; : loi1=null & loi2=null                             -> to
; : loi2=null | (loi1!=null & (pred loi1[0] loi2[0])) -> (loi1[0] . (list.merge pred loi1[1..end] loi2 to))
; : else                                              -> (loi2[0] . (list.merge pred loi1 loi2[1..end] to))
(define list.merge
  (lambda (pred loi1 loi2 to)
    (if (and (null? loi1) (null? loi2))
        to
        (if (or (null? loi2) (and (not (null? loi1)) (pred (car loi1) (car loi2))))
            (cons (car loi1) (list.merge pred (cdr loi1) loi2 to))
            (cons (car loi2) (list.merge pred loi1 (cdr loi2) to))))))

; (list.sort-by-to pred loi to): F X L X L -> L
; returns list of elements sorted by pred to list "to"
; (this is insertion sort as it merges a single number with a sorted list of numbers)
(define list.sort-by-to
  (lambda (pred loi to)
    (if (null? loi)
        to
        (list.merge pred (list (car loi)) (list.sort-by-to pred (cdr loi) (list)) to))))

; (list.sort lst): L -> L
; returns a list of elements sorted in ascending order
(define list.sort
  (lambda (loi)
    (list.sort-by-to <= loi (list))))
