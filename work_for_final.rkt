#lang racket

(define log2
  (lambda (n)
    (if (= n 1) 0 (+ 1 (log2 (quotient (+ n 1) 2)))
        )

    ))


;(sumevenlist '(1 2 3 4 5 6) 0)

(define sumevenlist
  (lambda (lst total)
    (if (empty? lst)(display total)
        (if (even? (car lst)) (if (empty? lst)(sumevenlist (lst)((+ (car lst) total)))(sumevenlist (cdr lst) (+ (car lst) total)));condition null o dersin tüm notları
            (sumevenlist (cdr lst) total)
            )
        )
    ))

(define sumevenlistnottail
  (lambda (lst)
    (if (empty? lst) 0
        (+ (car lst) (sumevenlistnottail (cdr lst)))
        )
    )
  )
;10.7a - with rotate
(define x
  (lambda (lst)
    (cons (car (reverse lst)) (reverse(cdr (reverse lst)))
          )))

(define rotate
  (lambda (lst total)
    (if (= (length total) (length lst))(display total)
        (rotate (x lst) (cons (x lst) total))
        )))

;(define rotatenottail
;  (lambda (lst)
;    (if (equal? lst (x lst)) (lst)
;        (cons (x lst) (rotatenottail (x lst))
;        ))))

;10.7b - not work correctly
;(define comp
;  (lambda (cr num lst)
;    (let loop ((lst lst) (nw-lst nw-lst))
;      (if (empty? lst) (nw-lst)
;          (if (cr (car lst) num)(loop (cdr lst) (cons (car lst) nw-lst))
;              (loop (cdr lst) (nw-lst))
;                    )))))

;(define filter
;  (lambda (criteria lst)
;    (comp (car criteria)(caddr criteria) lst)))


;10.8
(define permut
  (lambda (lst)
    (cons (rotate lst '())(rotate (reverse lst) '()))
    ))
    
;10.10
(define (flatten tree)
  (cond ((null? tree) '())
        ((pair? tree) (append (flatten (car tree)) (flatten (cdr tree))))
        (else (list tree))
  )
)

(define same-fringe
  (lambda (T1 T2)
    (equal (flatten T1) (flatten T2))))
    
;(same-fringe `(1 ((2) 3) (4)) '(1 (2 (3 4))))

;10.14
   (define (partition compare l1)
      (cond
         ((null? l1) '())
         ((compare (car l1)) (cons (car l1) (partition compare (cdr l1))))
         (else (partition compare (cdr l1)))))

   (define (quicksort l1)
      (cond
         ((null? l1) '())
         (else (let ((pivot (car l1)))
            (append (append (quicksort (partition (lambda (x) (< x pivot)) l1))
                       (partition (lambda (x) (= x pivot)) l1))
                    (quicksort (partition (lambda (x) (> x pivot)) l1)))))))
