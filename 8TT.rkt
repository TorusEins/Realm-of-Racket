#lang racket
(require rackunit)
(require 2htdp/universe 2htdp/image)

;Player CONSTANS
(define MAX-HEALTH 35)
(define MAX-AGILITY 35)
(define MAX-STRENGTH 35)

(define ATTACKS# 4)
(define STAB-DAMAGE 2)
(define FLAIL-DAMAGE 3)
(define HEALING 8)

(define MONSTER# 12)
(define PER-ROW 4)

(define INSTRUCTIONS-2 "Select a monster using the arrow keys")
(define INSTRUCTIONS-1
  "Press S to stab a monster | Press F to Flail wildly | Press H to Heal")


(struct orc-world (player lom attack# target) #:mutable)
(struct player (health agility strength) #:mutable #:transparent)


(struct monster (image (health  #:mutable)) #:transparent)
(struct orc monster (club) #:transparent  #:mutable)
(struct hydra monster ())
(struct slime monster (sliminess))
(struct brigand monster ())


;Monster Attributes
(define MONSTER-HEALTH0 9)
(define CLUB-STRENGTH 8)
(define SLIMINESS 5)

;MAIN========================================
(define (start)
  (big-bang (initialize-orc-world)
            (on-key player-acts-on-monster)
            (to-draw render-orc-battle)
            (stop-when end-of-orc-battle? render-the-end)))

(define (initialize-orc-world)
  (define player0 (initialize-player))
    (define lom0 (initialize-monsters))
    (orc-world player0 lom0 (random-number-of-attacks player0) 0))

(define (player-acts-on-monster w k)
  (cond
    [(zero? (orc-world-attack# w)) (void)]
    [(key=? "s" k) (stab w)]
    [(key=? "h" k) (heal w)]
    [(key=? "f" k) (flail w)]
    [(key=? "right" k) (move-target w +1)]
    [(key=? "left" k) (move-target w -1)]
    [(key=? "down" k) (move-target w (+ PER-ROW))]
    [(key=? "up" k) (move-target w (- PER-ROW))])
  (give-monster-turn-if-attack#=0 w)
  w)
  

(define (render-orc-battle w)
  (render-orc-world w (orc-world-target w) (instructions w)))

(define (end-of-orc-battle? w)
  (or (win? w) (lose? w)))

(define (render-the-end w)
  (render-orc-world w #f (message (if (lose?w) LOSE WIN))))

;subfunction for 4 main functions in BigBang
(define (initialize-player)
  (player MAX-HEALTH MAX-AGILITY MAX-STRENGTH))

(define (random-number-of-attacks p)
  (random-quotient (player-agility p) ATTACKS#))

(define (random-quotient x y)
  (define div (quotient x y))
  (if (> 0 div) 0 (random+ (add1 div))))

(define (random+ n)
  (add1 (random n)))

(define (initialize-monsters)
  (build-list
   MONSTER#
   (lambda (_)
     (define health (random+ MONSTER-HEALTH0))
     (case (random 4)
       [(0) (orc ORC-IMAGE health (random+ CLUB-STRENGTH))]
       [(1) (hydra HYDRA-IMAGE health)]
       [(2) (slime SLIME-IMAGe health (random+ SLIMINESS))]
       [(3) (brigand BRIGAND-IMAGE health)]))))

;defines instructions that will be shown onscreen
(define (instructions w)
  (define na (number->string (orc-world-attack# w)))
  (define ra (string-append REMAINING na))
  (define txt (text ra INSTRUCTION-TEXT-SIZE ATTACK-COLOR))
  (above txt INSTRUCTION-TEXT)

(define(message str)
  (text str MESSAGE-SIZE MESSAGE-COLOR))

;function

;(it eats 3 parameters and returns a function with 2 parameters)
(define (player-update! setter selector  mx)
  (lambda(player delta)
    (setter player (interval+ (selector player) delta mx))))


;synxtax (playerhealth+ playername healthdelta) (the two parameters from the lambda function in player-update
(define player-health+
  (player-update! set-player-health! player-health MAX-HEALTH))

(define player-agility+
  (player-update! set-player-agility! player-agility MAX-AGILITY))

(define player-strength+
  (player-update! set-player-strength! player-strength MAX-STRENGTH))

;helper function

; adds an n m but stays with 0 and 100 
(define (interval+ n m (max-value 100))
  (min (max (+ n m) 0) max-value))




;test

(define lom (list (orc 9 3) (orc 9 4) (orc 9 1)))
(check-equal? (list-ref lom 0) (orc 9 3))
(define testplayer (player 'health1 'agility2 'strength3))
(define testworld (orc-world testplayer lom 'attack#3 0))
(define testorc (orc MONSTER-HEALTH0 2))

(check-equal? (list-ref (orc-world-lom testworld) 2) (orc 9 1))

;equality checks (seem unitutive
(define player2 testplayer)
(define player3 (player 'health1 'agility2 'strength3))
(check-equal? player3 testplayer)
(check-false (eq? player3 testplayer))
(set-player-health! player2 99)
(check-equal? (player-health testplayer) 99)
(define players (list testplayer player2 player3))
(set-player-health! (second players) 50)      ;$$$
(check-equal? (player-health player2) 50)     ;$$$
(check-true (eq? (second players) player2))   ;$$$

(player-health+ player2 100)
(check-equal? (player-health player2) MAX-HEALTH) ;

(check-equal? (let ((p (player 1 2 3))) (player-agility+ p +3) p)   ;let creates local copy for testing, better version than above marked with $$$$
              (player 1 5 3))
(check-equal? (let ((p (player 1 2 3))) (player-strength+ p +3)  p)
              (player 1 2 6))



;Test mutators
(define (stab-orc tOrc)
  (set-monster-health! tOrc (- (monster-health tOrc) 3)))

(stab-orc testorc)
(check-equal? testorc (orc 6 2))      ; interacts with monster-health test;(check-equal? (monster-health testorc) 6)
(check-equal? (orc-club testorc) 2)

;other tests
(check-equal? (orc-world-player testworld) testplayer)
(check-equal? (player-strength (orc-world-player testworld)) 'strength3)
(check-true (orc? testorc))
(check-true (monster? testorc))


;z is an facultative third argument, that if not specified differently in the function call, will be 100
(define (test x y (z 100))
                  (+ x z)) 