;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname racket-invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define INVADER-HEIGHT/2 (/ (image-height INVADER) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))
(define INVADER-WIDTH/2 (/ (image-width INVADER) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))
(define MISSILE-HEIGHT/2 (/ (image-height MISSILE) 2))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit I1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit I1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit I1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))



(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;; =================
;; Functions:

;; Game -> Game
;; start the world with (main G0)
;; 
(define (main game)
  (big-bang game                        ; Game
    (on-tick   update-game)             ; Game -> Game
    (to-draw   render-game)             ; Game -> Image
    (stop-when invader-reached-bottom?) ; Game -> Boolean
    (on-key    handle-key)))           ; Game KeyEvent -> Game

;; Game -> Game
;; Move the tank and all missiles and invaders, randomly create a new invader, and deleting invaders hit by missiles
(check-expect (update-game
               (make-game empty empty (make-tank (/ WIDTH 2) 1))) ;only the tank in the center, going right
              (make-game (update-invaders (make-game empty empty (make-tank (/ WIDTH 2) 1)))
                         empty
                         (update-tank (make-tank (/ WIDTH 2) 1)))) 
(check-expect (update-game
               (make-game (cons
                           (make-invader 50 60 INVADER-X-SPEED)
                           (cons (make-invader 100 200 (- INVADER-X-SPEED)) empty))
                          empty
                          (make-tank 100 (- 1))) ; tank going left, no missiles, two invaders in opposite directions
               ) 
              (make-game
               (update-invaders
                (make-game (cons
                            (make-invader 50 60 INVADER-X-SPEED)
                            (cons (make-invader 100 200 (- INVADER-X-SPEED)) empty))
                           empty
                           (make-tank 100 (- 1))))
               empty
               (update-tank  (make-tank 100 (- 1)))))
(check-expect (update-game
               (make-game (cons
                           (make-invader 50 60 INVADER-X-SPEED) empty)
                          (cons (make-missile 30 50) empty)
                          (make-tank 100 (- 1))) ; one missile
               ) 
              (make-game
               (update-invaders
                (make-game (cons
                            (make-invader 50 60 INVADER-X-SPEED) empty)
                           (cons (make-missile 30 50) empty)
                           (make-tank 100 (- 1))))
               (update-missiles   (make-game (cons
                                              (make-invader 50 60 INVADER-X-SPEED) empty)
                                             (cons (make-missile 30 50) empty)
                                             (make-tank 100 (- 1))))
               (update-tank  (make-tank 100 (- 1)))))                



(define (update-game game) game) ;stub

;; Game -> listof Invaders
;; update the invaders by:
;;  - removing invaders hit by missiles
;;  - moving invaders not hit by missiles
;;  - generating new invaders at the same x-position as tank
;; !!! the generation rate has to be updated

(check-expect (update-invaders (make-game empty empty (make-tank (/ WIDTH 2) 1)))
              (cons
               (make-invader (/ WIDTH 2) (- INVADER-HEIGHT/2) INVADER-X-SPEED) empty)) ; no previous invader, just generate a new one
(check-expect (update-invaders (make-game
                                (cons
                                 (make-invader (/ WIDTH 2) (+ INVADER-HEIGHT/2 0) INVADER-X-SPEED) empty)
                                empty
                                (make-tank (/ WIDTH 3) -1)))
              (cons
               (make-invader (/ WIDTH 3) (- INVADER-HEIGHT/2) INVADER-X-SPEED)
               (cons (make-invader (+ (/ WIDTH 2) INVADER-X-SPEED)
                                   (+ INVADER-HEIGHT/2 INVADER-Y-SPEED)
                                   INVADER-X-SPEED) empty))) ;a new one was generated at a new location, the existing was moved
(check-expect (update-invaders (make-game
                                (cons
                                 (make-invader (- WIDTH INVADER-X-SPEED) 100 INVADER-X-SPEED)
                                 (cons (make-invader (+ (/ WIDTH 2) INVADER-X-SPEED)
                                                     (+ INVADER-HEIGHT/2 INVADER-Y-SPEED)
                                                     INVADER-X-SPEED) empty))
                                (cons
                                 (make-missile (+ (/ WIDTH 2) INVADER-X-SPEED) (+  (+ INVADER-HEIGHT/2 INVADER-Y-SPEED) 10)) empty)
                                (make-tank (/ WIDTH 4) -1)))
              (cons
               (make-invader (/ WIDTH 4) (- INVADER-HEIGHT/2) INVADER-X-SPEED)
               (cons (make-invader (- WIDTH INVADER-X-SPEED)
                                   (+ 100 INVADER-Y-SPEED)
                                   (- INVADER-X-SPEED)) empty)))             

(define (update-invaders game) empty) ;stub

;; Tank -> Tank
;; move (or bounce) the tank
(check-expect (update-tank (make-tank (/ WIDTH 2) 1)) (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1))
(check-expect (update-tank (make-tank (/ WIDTH 2) -1)) (make-tank (- (/ WIDTH 2) TANK-SPEED) -1))
(check-expect (update-tank (make-tank (- WIDTH TANK-WIDTH/2 TANK-SPEED) 1)) (make-tank (- WIDTH TANK-WIDTH/2) -1))
(check-expect (update-tank (make-tank (+ TANK-WIDTH/2 TANK-SPEED) -1)) (make-tank TANK-WIDTH/2 1))

(define (update-tank tank) tank) ;stub

;; Game -> listof Missile
;; move the missiles and remove the ones that hit
(check-expect (update-missiles
               (make-game
                (cons
                 (make-invader (- WIDTH INVADER-X-SPEED) 100 INVADER-X-SPEED)
                 (cons (make-invader (+ (/ WIDTH 2) INVADER-X-SPEED)
                                     (+ INVADER-HEIGHT/2 INVADER-Y-SPEED)
                                     INVADER-X-SPEED) empty))
                (cons
                 (make-missile (+ (/ WIDTH 2) INVADER-X-SPEED) (+  (+ INVADER-HEIGHT/2 INVADER-Y-SPEED) 10)) empty)
                (make-tank (/ WIDTH 4) -1)))
              empty) ; one missile hit, so no one left
(check-expect (update-missiles
               (make-game
                (cons
                 (make-invader (- WIDTH INVADER-X-SPEED) 100 INVADER-X-SPEED)
                 (cons (make-invader (+ (/ WIDTH 2) INVADER-X-SPEED)
                                     (+ INVADER-HEIGHT/2 INVADER-Y-SPEED)
                                     INVADER-X-SPEED) empty))
                (cons
                 (make-missile (+ (/ WIDTH 2) INVADER-X-SPEED) (+  (+ INVADER-HEIGHT/2 INVADER-Y-SPEED) 10))
                 (cons (make-missile 10 10) empty))
                (make-tank (/ WIDTH 4) -1)))
              (cons (make-missile 10 (- 10 MISSILE-SPEED)) empty))

; (define (update-missiles game) empty) ;stub

(define (update-missiles game)
  (retire-missiles (move-missiles (remove-missiles game))))

;; Game -> listofMissiles
;; remove missiles that hit invaders
;; !!!
(check-expect (remove-missiles                (make-game
                                               (cons
                                                (make-invader (- WIDTH INVADER-X-SPEED) 100 INVADER-X-SPEED)
                                                (cons (make-invader (+ (/ WIDTH 2) INVADER-X-SPEED)
                                                                    (+ INVADER-HEIGHT/2 INVADER-Y-SPEED)
                                                                    INVADER-X-SPEED) empty))
                                               (cons
                                                (make-missile (+ (/ WIDTH 2) INVADER-X-SPEED) (+  (+ INVADER-HEIGHT/2 INVADER-Y-SPEED) 10)) empty)
                                               (make-tank (/ WIDTH 4) -1)))
              empty) ;one missile hit, so no one left
(check-expect (remove-missiles
               (make-game
                (cons
                 (make-invader (- WIDTH INVADER-X-SPEED) 100 INVADER-X-SPEED)
                 (cons (make-invader (+ (/ WIDTH 2) INVADER-X-SPEED)
                                     (+ INVADER-HEIGHT/2 INVADER-Y-SPEED)
                                     INVADER-X-SPEED) empty))
                (cons
                 (make-missile (+ (/ WIDTH 2) INVADER-X-SPEED) (+  (+ INVADER-HEIGHT/2 INVADER-Y-SPEED) 10))
                 (cons (make-missile 10 10) empty))
                (make-tank (/ WIDTH 4) -1)))
              (cons (make-missile 10 10) empty))


; (define (remove-missiles game) empty) ;stub

;; listof Missile -> listof Missile
;; move missiles
;; !!!
(check-expect (move-missiles 
               (cons
                (make-missile (+ (/ WIDTH 2) INVADER-X-SPEED) (+  (+ INVADER-HEIGHT/2 INVADER-Y-SPEED) 10)) empty)
               )
              (cons
               (make-missile (+ (/ WIDTH 2) INVADER-X-SPEED) (- (+  (+ INVADER-HEIGHT/2 INVADER-Y-SPEED) 10) INVADER-Y-SPEED)) empty))

(define (move-missiles lom) empty) ;stub

;; listofMissiles -> listofMissiles
;; retire missiles that pass the top boundary
;; !!!
(check-expect (retire-missiles
               (cons
                (make-missile (+ (/ WIDTH 2) INVADER-X-SPEED) (+  (+ INVADER-HEIGHT/2 INVADER-Y-SPEED) 10)) empty))
               (cons
                (make-missile (+ (/ WIDTH 2) INVADER-X-SPEED) (+  (+ INVADER-HEIGHT/2 INVADER-Y-SPEED) 10)) empty))

(check-expect (retire-missiles
               (cons
                (make-missile (+ (/ WIDTH 2) INVADER-X-SPEED) MISSILE-HEIGHT/2) empty))
 empty) 
                                

(define (retire-missiles game) empty) ;stub

;; Game -> Image
;; Draw the tank and all missiles and invaders
;; !!!

(define (render-game game) BACKGROUND) ;stub

;; Game -> Boolean
;; produce true if one of the invaders hits the bottom
;; !!!

(define (invader-reached-bottom? game) false) ;stub

;; Game Key-Event -> Game
;; change direction of tank if arrow is pressed and create new missile 
;; !!!

(define (handle-key game ke) game) ;stub
#;
(define (handle-key ws ke)
  (cond [(key=? ke " ") (... ws)]
        [else 
         (... ws)]))
