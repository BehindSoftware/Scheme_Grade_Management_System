#lang racket
(define students-grades '((ali CENG212 "DC")(ayse CENG463 "BB")(ahmet CENG212 "AA")(berkay CENG212 "AA")(berna CENG212 "FD")))
(define course-list '((CENG212 Concepts-of-Programming-Languages) (CENG463 Introduction-to-Machine-Learning) (CENG213 Theory-of-Computation)))
(define insert 0)
(define select 1)

(define query
  (lambda (process criteria students-grades course-list)
    (cond ((= process 0)(insert-process criteria students-grades course-list))
          ((= process 1)(select-process criteria students-grades course-list))
          (else (display "invalid process"))  
          )
    ))

(define insert-process
  (lambda (criteria students-grades course-list)
    (cond ((= (length criteria) 3)(append (list criteria) students-grades))
          ((= (length criteria) 2)(append (list criteria) course-list))
          (else (display "invalid criteria"))
          )
    ))

(define select-process
  (lambda (criteria students-grades course-list)
    (parse-criteria-for-course criteria (car criteria) students-grades course-list) 
    ))

(define parse-criteria-for-course
  (lambda (criteria course students-grades course-list)
    (if (= (length criteria) 1)(scan-specific-course course null null '() students-grades course-list);specific course all notes
    (parse-criteria-for-grade criteria course (caddr criteria) students-grades course-list))
    ))

(define parse-criteria-for-grade
  (lambda (criteria course grade students-grades course-list)
    (grade-enumator criteria course grade (cadr criteria) students-grades course-list)
    ))

(define parse-criteria-for-condition
  (lambda (criteria course grade condition students-grades course-list)
    (scan-specific-course course grade condition '() students-grades course-list)
    ))

(define scan-specific-course
  (lambda (course grade condition find-list students-grades course-list)
    (let loop ((students-grades students-grades)(find-list find-list))
      (if (equal? "all" course)(grade-enumator-for-list course grade condition  students-grades course-list)
      (if (null? students-grades) (if (null? condition)(display find-list)(grade-enumator-for-list course grade condition  find-list course-list));condition null o dersin tüm notları
          (if (equal? course (cadar students-grades)) (loop (cdr students-grades)(append (list (car students-grades)) find-list))
              (loop (cdr students-grades) find-list))
          ))
      )))

(define grade-enumator-for-list
  (lambda (course grade condition  find-list course-list)
    (let loop((find-list find-list)(new-list '()))
      (if (null? find-list)(scan-specific-grade course grade condition '() new-list course-list) 
          (cond ((equal? (caddar find-list) "AA")(loop (cdr find-list)(cons (reverse(cons 8(cdr(reverse(car find-list))))) new-list)))
                ((equal? (caddar find-list) "BA")(loop (cdr find-list)(cons (reverse(cons 7(cdr(reverse(car find-list))))) new-list)))
                ((equal? (caddar find-list) "BB")(loop (cdr find-list)(cons (reverse(cons 6(cdr(reverse(car find-list))))) new-list)))
                ((equal? (caddar find-list) "BC")(loop (cdr find-list)(cons (reverse(cons 5(cdr(reverse(car find-list))))) new-list)))
                ((equal? (caddar find-list) "CC")(loop (cdr find-list)(cons (reverse(cons 4(cdr(reverse(car find-list))))) new-list)))
                ((equal? (caddar find-list) "DC")(loop (cdr find-list)(cons (reverse(cons 3(cdr(reverse(car find-list))))) new-list)))
                ((equal? (caddar find-list) "DD")(loop (cdr find-list)(cons (reverse(cons 2(cdr(reverse(car find-list))))) new-list)))
                ((equal? (caddar find-list) "FD")(loop (cdr find-list)(cons (reverse(cons 1(cdr(reverse(car find-list))))) new-list)))
                ((equal? (caddar find-list) "FF")(loop (cdr find-list)(cons (reverse(cons 0(cdr(reverse(car find-list))))) new-list)))
          )
      )
    )))

(define arrange-grade
  (lambda (find-list)
    (let loop((find-list find-list)(new-list '()))
      (if (null? find-list)(display new-list) 
          (cond ((= (caddar find-list) 8)(loop (cdr find-list)(cons (reverse(cons "AA"(cdr(reverse(car find-list))))) new-list)))
                ((= (caddar find-list) 7)(loop (cdr find-list)(cons (reverse(cons "BA"(cdr(reverse(car find-list))))) new-list)))
                ((= (caddar find-list) 6)(loop (cdr find-list)(cons (reverse(cons "BB"(cdr(reverse(car find-list))))) new-list)))
                ((= (caddar find-list) 5)(loop (cdr find-list)(cons (reverse(cons "BC"(cdr(reverse(car find-list))))) new-list)))
                ((= (caddar find-list) 4)(loop (cdr find-list)(cons (reverse(cons "CC"(cdr(reverse(car find-list))))) new-list)))
                ((= (caddar find-list) 3)(loop (cdr find-list)(cons (reverse(cons "DC"(cdr(reverse(car find-list))))) new-list)))
                ((= (caddar find-list) 2)(loop (cdr find-list)(cons (reverse(cons "DD"(cdr(reverse(car find-list))))) new-list)))
                ((= (caddar find-list) 1)(loop (cdr find-list)(cons (reverse(cons "FD"(cdr(reverse(car find-list))))) new-list)))
                ((= (caddar find-list) 0)(loop (cdr find-list)(cons (reverse(cons "FF"(cdr(reverse(car find-list))))) new-list)))
          )
      )
    )))

(define scan-specific-grade
  (lambda (course grade condition print-list find-list course-list)
    (let loop ((find-list find-list)(print-list print-list))
      (if (null? find-list) (arrange-grade print-list)
          (if (take-condition condition grade (caddar find-list)) (loop (cdr find-list)(append (list (car find-list)) print-list))
              (loop (cdr find-list) print-list))
          )
      )))

(define take-condition
  (lambda (condition grade list-of-grade)
    (cond ((equal? condition "<")(< list-of-grade grade))
          ((equal? condition ">")(> list-of-grade grade))
          ((equal? condition "=")(= list-of-grade grade))
          ((equal? condition ">=")(>= list-of-grade grade))
          ((equal? condition "<=")(<= list-of-grade grade))
          )
    ))

(define grade-enumator
  (lambda (criteria course grade condition students-grades course-list)
    (cond ((equal? grade "AA")(parse-criteria-for-condition criteria course 8 condition students-grades course-list))
          ((equal? grade "BA")(parse-criteria-for-condition criteria course 7 condition students-grades course-list))
          ((equal? grade "BB")(parse-criteria-for-condition criteria course 6 condition students-grades course-list))
          ((equal? grade "BC")(parse-criteria-for-condition criteria course 5 condition students-grades course-list))
          ((equal? grade "CC")(parse-criteria-for-condition criteria course 4 condition students-grades course-list))
          ((equal? grade "DC")(parse-criteria-for-condition criteria course 3 condition students-grades course-list))
          ((equal? grade "DD")(parse-criteria-for-condition criteria course 2 condition students-grades course-list))
          ((equal? grade "FD")(parse-criteria-for-condition criteria course 1 condition students-grades course-list))
          ((equal? grade "FF")(parse-criteria-for-condition criteria course 0 condition students-grades course-list))
          )
    ))

(define length
  (lambda (input-list)
    (if (null? input-list)
        0
        (+ 1 (length (cdr input-list))))))

;(query insert '(berkay CENG212 "AA") students-grades course-list)
;(query insert '(CENG313 Operating-Systems) students-grades course-list)
;(query select '(CENG212) students-grades course-list)
;(query select '(CENG212 "=" "AA") students-grades course-list)
;(query select '(CENG212 ">" "FD") students-grades course-list)
;(query select '("all" ">" "FD") students-grades course-list)