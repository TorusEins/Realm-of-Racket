#lang racket
(require 2htdp/universe 2htdp/image)

(struct pit (snake goos) #:transparent)
(struct snake (dir segs) #:transparent)
(struct posn (x y) #:transparent)
(struct goo (loc expireTime) #:transparent)

(define WIDTH 40)
(define HEIGHT 50)
(define SIZE 200)
(define EXPIRATION-TIME 50)
(define TICK-RATE 10)
(define SEG-SIZE 30) ;???

(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (list (fresh-goo)(fresh-goo)
                       (fresh-goo)(fresh-goo)))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))
            
(define (fresh-goo w)
  (goo (posn (add1 (random (sub1 SIZE)))
             (add1 (random (sub1 SIZE))))
       EXPIRATION-TIME))

(define (next-pit w)
  (define snake (pit-snake w))
  (define goos (pit-goos w))
  (define goo-to-eat! (can-eat? snake goos))
  (if goo-to-eat!
      (pit (grow snake) (age-goo (eat goos goo-to-eat!)))
      (pit (slither snake) (age-goo goos))))

(define {can-eat? snake goos}
  (cond[(empty? goos) #f]
       [else(if(close? (snake-head snake) (first goos))
               (first goos)
               (can-eat? snake (rest goos)))]))

(define (close? head g)
 (equal? head (goo-loc g)))

(define {close2? h goo}; hopefully faster than "close?" by avoiding "equal"
  (define g (goo-loc goo))
  (and (posn? h) (posn? g)
       (= (posn-x h) (posn-x g))
       (=(posn-y h) (posn-y g))))

{define (snake-head sn) (first (snake-segs sn))}
{define (snake-body sn) (rest (snake-segs sn))}
{define (snake-tail sn) (last (snake-segs sn))}

(define (next-head sn)
  (define head (snake-head sn))
  (define dir (snake-dir sn))
  (cond [ (string=? dir "up") (posn-move head 0 -1)]
        [ (string=? dir "down") (posn-move head 0 1)]
        [ (string=? dir "left") (posn-move head -1 0)]
        [ (string=? dir "right") (posn-move head 1 0)]))

(define (posn-move p dx dy)
  (posn (+ (posn-x p) dx)
        (+ (posn-y p) dy)))

(define (grow sn)
  (snake (snake-dir sn)
         (cons (next-head sn) (snake-segs sn))))

(define (slither sn)
  (snake (snake-dir sn)
         (cons (next-head sn)
               (all-but-last (snake-segs sn)))))

(define (all-but-last segs) 
  (cond[(empty? (rest segs)) '()]
       [else (cons(first segs)(all-but-last(rest segs)))]))

(define (age-goo goos)
  (rot (renew goos)))

(define (rot goos) 
  (cond [(empty? goos) empty]
        [else (cons (decay (first goos))
                    (rot (rest goos)))]))

(define (renew goos)
  (cond[(empty? goos) (empty)]
       [(rotten? (first goos)) 
        (cons (fresh-goo) (renew (rest goos)))]
       [else (cons (first goos) renew (rest goos))]))

(define (decay g)
  (goo (goo-loc g) (sub1 (goo-expireTime g))))

(define (rotten? g)
  (zero? (goo-expireTime g)))

(define (eat goos goo-to-eat)
  (cons {fresh-goo} {remove goo-to-eat goos}))

(define (direct-snake w ke)
  (cond[(dir? ke) (world-change-dir w ke)]
       [else w]))
  
{define (dir? k)
  (or (key=? k "up")
      (key=? k "down")
      (key=? k "left")
      (key=? k "right"))}

{define {world-change-dir w d}
  {define snake (pit-snake w)}
  {cond[(opposite-dir? (snake-dir snake) d) w]
       [else (pit (snake-change-dir snake d)
                  (pit-goos w))]}}
        
{define {opposite-dir? d1 d2} 
  {cond [(string=? d1 "up") (string=? d2 "down")]
        [(string=? d1 "down") (string=? d2 "up")]
        [(string=? d1 "left") (string=? d2 "right")]
        [(string=? d1 "right") (string=? d2 "left")]}}
                 
{define (snake-change-dir s d)
  (snake d (snake-segs s))}

(define (dead? w) '...)

;RENDERING RENDERING RENDERING


(define SEG-IMG '...)

(define (render-pit w)
  (snake+scene (pit-snake w)
               (goo-list+scene (pit-goos w) MT-SCENE)))

(define (snake+scene snake scene)
  (define snake-body-scene 
    (img-list+scene (snake-body snake) SEG-IMG scene))
  (define dir (snake-dir snake))
  (img+scene (snake-head snake)
             (cond[(string=? "up" dir) HEAD-UP-IMG]
                  [(string=? "down" dir) HEAD-DOWN-IMG]
                  [(string=? "right" dir) HEAD-RIGHT-IMG]                  
                  [(string=? "left" dir) HEAD-LEFT-IMG])
             snake-body-scene))

(define (img-list+scene posns img scene)
  (cond[(empty posns) scene]
       [else (img+scene
              (first posns)
              img
              (img-list+scene (rest posns)
                              img scene))]))

(define (img+scene posn img scene)
  (place-image img
               (* (posn-x posn) SEG-SIZE)
               (* (posn-y posn) SEG-SIZE)
               scene))

(define (goo-list+scene goos scene)
  (define (get-posns-from-goo goos)
    (cond[(empty? goos) empty]
         [else (cons (goo-loc (first goos))
                     (get-posns-from-goo (rest goos)))]))
  (img-list+scene (get-posns-from-goo goos)
                  GOO-IMG scene))
               
(define (render-end w) '...)
                       


;TESTS
(require rackunit rackunit/text-ui)

(define snake-example (snake "up" 
                             (list (posn 1 1) (posn 1 2) (posn 1 3))))
(define goos-example (list (goo (posn 1 0) 3) (goo (posn 5 8) 15)))
(define pit-example (pit snake-example goos-example))
(define snake-going-left (snake "left" (list (posn 2 18))))
(define plain-world (pit snake-going-left empty))

(check-equal? (close2? (snake-head snake-example) (first goos-example))
              (close? (snake-head snake-example) (second goos-example)) )
(check-equal? (snake-dir snake-example) "up")
