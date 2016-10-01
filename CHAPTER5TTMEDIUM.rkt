#lang racket
(require 2htdp/universe 2htdp/image)

(struct interval+ (small big nrOfGuesses) #:transparent)

(define WIDTH 600)
(define HEIGHT 400)
(define TEXT-SIZE 20)
(define SIZE 72)
(define TEXT-X (/ WIDTH 2))
(define TEXT-UPPER-Y 50)
(define TEXT-LOWER-Y (- HEIGHT 50))
(define HELP-TEXT (text "↑ for larger numbers, ↓ for smaller numbers" TEXT-SIZE "blue"))   ;text translates strings into images
(define HELP-TEXT2 (text "Press = when your number is guessed" TEXT-SIZE "blue"))
(define COLOR "red")

(define (start lower upper) ;MAIN
  (big-bang (interval+ lower upper 0)
            (on-key deal-with-guess)
            (to-draw render)
            (stop-when single? render-last-scene)))

(define MT-SC
  (place-image/align HELP-TEXT TEXT-X TEXT-UPPER-Y "center" "top"
                     (place-image/align HELP-TEXT2 TEXT-X TEXT-LOWER-Y "center" "bottom"
                                        (empty-scene WIDTH HEIGHT))))

(define (deal-with-guess w key )
  (cond[(key=? key "up") (bigger w)]
       [(key=? key "down") (smaller w)]
       [(key=? key "q") (stop-with w)]
       [(key=? key "=") (stop-with w)]))

(define (bigger w)
  (interval+ (min (add1 (guess w)) (interval+-big w))
            (interval+-big w) (add1 (interval+-nrOfGuesses w))))

(define (smaller w)
  (interval+ (interval+-small w)
            (max (sub1 (guess w)) (interval+-small w)) (add1 (interval+-nrOfGuesses w))))

(define (guess w)
  (quotient (+ (interval+-small w) (interval+-big w)) 2))

(define (render w)
  (overlay (text (string-append "after " (number->string (interval+-nrOfGuesses w)) " guesses: " (number->string (guess w)) "?") 
                 SIZE COLOR) MT-SC))
                      
{define (render-last-scene w)
  (overlay (text (string-append (number->string (guess w))"!") 
                 SIZE COLOR) MT-SC)}

{define (single? w)
  (= (interval+-small w) (interval+-big w))}

