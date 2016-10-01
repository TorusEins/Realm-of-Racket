#lang racket
(require 2htdp/universe 2htdp/image)

(struct pit (snake snake2 goos ObCe) #:transparent)
(struct snake (dir segs) #:transparent)
(struct posn (x y) #:transparent)
(struct goo (loc expireTime) #:transparent)
(struct obstacleCenter (loc idleTime) #:transparent)

(define WIDTH 30)
(define HEIGHT 15)
(define EXPIRATION-TIME 50)
(define MOVES-PER-SECOND 7)
(define NUMBER-OF-GOOS 8)
(define OBSTACLE-DURATION 100) ; obstacle are currently constant
 
(define (my-map func lst)
  (cond[(empty? lst) empty]
       [else (cons (func (first lst))
                   (my-map func (rest lst)))]))

;ORIGINAL PROGRAMM
(define (initial-goo-list n)
  (if (= 0 n) empty
      (cons fresh-goo (initial-goo-list (sub1 n)))))
 
(define TICK-RATE (/ 1 MOVES-PER-SECOND)) 

(define center-x (quotient WIDTH 2))
(define center-y (quotient HEIGHT 2))

(define (createObstacle OC)
  (define x (posn-x (obstacleCenter-loc OC)))
  (define y (posn-y (obstacleCenter-loc OC)))
  (list (posn x (sub1 y))
        (posn x y)
        (posn x (add1 y))))                   
  
(define (start-snake)
  (big-bang (pit (snake "right" (list (posn 1 1)))
                 (snake "left" (list (posn (sub1 WIDTH) (sub1 HEIGHT))))
                 (list (fresh-goo)(fresh-goo)
                       (fresh-goo)(fresh-goo))
                 (obstacleCenter (posn center-x center-y) OBSTACLE-DURATION))
            (on-tick next-pit TICK-RATE)
            (on-key direct-snake)
            (to-draw render-pit)
            (stop-when dead? render-end)))
            
(define (fresh-goo)
  (goo (posn (add1 (random (sub1 WIDTH)))
             (add1 (random (sub1 HEIGHT))))
       EXPIRATION-TIME))

(define (next-pit w)
  (define snake (pit-snake w))
  (define snake2 (pit-snake2 w))
  (define goos (pit-goos w))
  (define obs (pit-ObCe w))
  (define goo-to-eat! (can-eat? snake goos))
  (define goo-to-eat!2 (can-eat? snake2 goos))
  (cond[(and goo-to-eat! goo-to-eat!2)
        (pit (grow snake) (grow snake2) (age-goo (eat (eat goos goo-to-eat!) goo-to-eat!2)) obs)]
       [(and goo-to-eat! (not goo-to-eat!2))
        (pit (grow snake) (slither snake2) (age-goo (eat goos goo-to-eat!)) obs)]
       [(and (not goo-to-eat!) goo-to-eat!2)
        (pit (slither snake) (grow snake2) (age-goo (eat goos goo-to-eat!2)) obs)]
       [else (pit (slither snake) (slither snake2) (age-goo goos) obs)]))
        
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
  (my-map (lambda (f)
            (goo (goo-loc f) (sub1 (goo-expireTime f))))
          goos))

(define (renew goos)
  (cond[(empty? goos) empty]
       [(rotten? (first goos)) 
        (cons (fresh-goo) (renew (rest goos)))]
       [else (cons (first goos) (renew (rest goos)))]))

(define (rotten? g)
  (zero? (goo-expireTime g)))

(define (eat goos goo-to-eat)
  (cons {fresh-goo} {remove goo-to-eat goos}))

(define (direct-snake w ke)
  (cond[(dir? ke) (world-change-dir w ke 1)]
       [(dir2? ke) (world-change-dir w (dir2? ke) 2)]
       [else w]))
  
{define (dir? k)
  (or (key=? k "up")
      (key=? k "down")
      (key=? k "left")
      (key=? k "right"))}

(define (dir2? k)
  (cond [ (key=? k "w") "up"]
        [ (key=? k "s") "down"]
        [ (key=? k "a") "left"]
        [ (key=? k "d") "right"]
        [else #f]))

{define {world-change-dir w d snake#}
  {define snake (pit-snake w)}
  {define snake2 (pit-snake2 w)}
  {cond[(= snake# 1) (pit (snake-change-dir snake d)
                          snake2
                          (pit-goos w)
                          (pit-ObCe w))]
       [else (pit snake
                  (snake-change-dir snake2 d)
                  (pit-goos w)
                  (pit-ObCe w))]}}
                 
{define (snake-change-dir s d)
  (snake d (snake-segs s))}

(define (dead? w) 
  (define snake (pit-snake w))
  (define snake2 (pit-snake2 w))
  (define obstacle (createObstacle (pit-ObCe w)))
  (or (snake-colliding? snake snake) (snake-colliding? snake2 snake2)
      (snake-colliding? snake snake2) (snake-colliding? snake2 snake)
      (wall-colliding? snake) (wall-colliding? snake2)
      (obstacle-colliding snake obstacle) (obstacle-colliding snake2 obstacle)))

(define (snake-colliding? s s2)
  (cons? (member (snake-head s) (snake-body s2))))

(define   (wall-colliding? snake)
  (define x (posn-x (snake-head snake)))
  {define y (posn-y (snake-head snake))}
  (or (= 0 x) (= x WIDTH)
      (= 0 y) (= y HEIGHT)))

(define (obstacle-colliding s o)
  (cons? (member (snake-head s) o)))
          
;RENDERING RENDERING RENDERING
;; GRAPHICAL BOARD
(define SEG-SIZE 30) ;größe des Rasters
(define WIDTH-PX  (* SEG-SIZE WIDTH))
(define HEIGHT-PX (* SEG-SIZE HEIGHT))
(define ENDGAME-TEXT-SIZE 50) ;???

;; Visual constants
(define MT-SCENE (empty-scene WIDTH-PX HEIGHT-PX))
(define GOO-IMG (bitmap "graphics/goo.gif"))
(define SEG-IMG  (bitmap "graphics/body.gif"))
(define HEAD-IMG (bitmap "graphics/head.gif"))
(define HEAD-UP-IMG (rotate 270 HEAD-IMG))
(define HEAD-DOWN-IMG (rotate 90 HEAD-IMG))
(define HEAD-LEFT-IMG (rotate 0 HEAD-IMG))
(define HEAD-RIGHT-IMG (rotate 180 HEAD-IMG))
(define OBSTACLE-IMG (square 30 "solid" "black"))

(define (render-pit w)
  (snake+scene (pit-snake2 w)
               (snake+scene (pit-snake w)
                            (goo-list+scene (pit-goos w)
                                            (img-list+scene (createObstacle (pit-ObCe w)) 
                                                            OBSTACLE-IMG 
                                                            MT-SCENE)))))

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
  (cond[(empty? posns) scene]
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
    (my-map goo-loc goos))
  (img-list+scene (get-posns-from-goo goos)
                  GOO-IMG scene))

(define (render-end w) 
  (overlay (text(string-append "Game Over! "  (number->string (length (snake-body (pit-snake w))))
                               " goos eaten") ENDGAME-TEXT-SIZE "black")
           (render-pit w)))                       

;TESTS
(require rackunit rackunit/text-ui)

(define snake-example (snake "up" 
                             (list (posn 1 1) (posn 1 2) (posn 1 3))))
(define snake-example2 (snake "down" 
                             (list (posn 5 6) (posn 5 7) (posn 5 8))))

(define obstacleCenters-example (obstacleCenter (posn center-x center-y)
                                       OBSTACLE-DURATION))

(define goos-example (list (goo (posn 1 0) 3) (goo (posn 5 8) 15)))
(define pit-example (pit snake-example snake-example2 goos-example empty))
(define snake-going-left (snake "left" (list (posn 2 18))))
(define plain-world (pit snake-going-left snake-example2 empty empty))

(check-equal? (close2? (snake-head snake-example) (first goos-example))
              (close? (snake-head snake-example) (second goos-example)) )
(check-equal? (snake-dir snake-example) "up")
