#lang racket

; (merge.lst pred loi1 loi2 to): F X L X L X L -> L
; merges loi1 and loi2 using pred to list "to"
; : loi1=null & loi2=null                             -> to
; : loi2=null | (loi1!=null & (pred loi1[0] loi2[0])) -> (loi1[0] . (merge.lst pred loi1[1..end] loi2 to))
; : else                                              -> (loi2[0] . (merge.lst pred loi1 loi2[1..end] to))
(define merge.lst
  (lambda (pred loi1 loi2 to)
    (if (and (null? loi1) (null? loi2))
        to
        (if (or (null? loi2) (and (not (null? loi1)) (pred (car loi1) (car loi2))))
            (cons (car loi1) (merge.lst pred (cdr loi1) loi2 to))
            (cons (car loi2) (merge.lst pred loi1 (cdr loi2) to))))))

; (sort.lst pred loi to): F X L X L -> L
; returns list of elements sorted by pred to list "to"
; (this is insertion sort as it merges a single number with a sorted list of numbers)
(define sort.lst
  (lambda (pred loi to)
    (if (null? loi)
        to
        (merge.lst pred (list (car loi)) (sort.lst pred (cdr loi) (list)) to))))

; (sort lst): L -> L
; returns a list of elements sorted in ascending order
(define sort
  (lambda (loi)
    (sort.lst <= loi (list))))
