#lang racket
 (struct student (name id# dorm))
(define owner (student 'Tristan 1 'RotesZimmer))
(define Evi (student 'Evi 2 'Wohnzimmer))
(define Julia (student 'Julia 3 'blauesZimmer))
(define Bewohner (list owner Evi Julia))


(struct student-body (freshman sophomores juniors seniors))
(define all-student
  (student-body (list owner Evi)
                (list owner Julia)
                (list owner (student 'Vasco 4 'blauesRotesZimmer))
                (list (student 'Irina 5 'blauesStinkZimmer))))

;abfragen
#|(student-dorm owner)
(student-id# (first Bewohner))
(student-dorm (second (student-body-sophomores all-student)))
(student? owner)
(student? (student 'Paul 5 'amEingang)) ; ergibt #t, obwohl Paul nirgends vorher und nachher definiert
(equal? owner (student 'Tristan 1 'Roteszimmer ))
(equal? owner (student 'Tristan 1 'RotesZimmer ))|#

(define (gerade x) (if (even? x) 
                       'gerade
                       (if (= x 7)
                       'achtung7 
                       'ungerade)))

(define (gerade2 x) (cond [(= x 7) 'achtung7]
                          [(even? x) 'gerade]
                          [else 'ungerade]))
;first recursion mit tests
(define (laenge LISTE)
  (if (list? LISTE)
  (if (empty? LISTE) 0
      (add1 (laenge (rest LISTE))))
  "Not a list"))

(define (laenge2 LISTE)
  (if (not (list? LISTE)) "Not a List" 
  (if (empty? LISTE) 0
      (add1 (laenge2 (rest LISTE))))))

(require rackunit)

(define (laenge3 LISTE)
  (check-true (list? LISTE) "Not a List") 
  (if (empty? LISTE) 0
      (add1 (laenge3 (rest LISTE)))))
