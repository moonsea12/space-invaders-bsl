;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define TANK-Y (- HEIGHT TANK-HEIGHT/2))
(define TANK-TURNR (- WIDTH (/ (image-width TANK) 2)))
(define TANK-TURNL (/ (image-width TANK) 2))
(define INVADER-TURNR (- WIDTH (/ (image-width INVADER) 2)))
(define INVADER-TURNL (/ (image-width INVADER) 2))
(define MISSILE (ellipse 5 15 "solid" "red"))

(define GAME_OVER_BACKGROUND 
  (place-image (text "Game over" 24 "black") (/ WIDTH 2) (- (/ HEIGHT 2) 30) BACKGROUND))

;; ========================================================================================================================
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



(define-struct invader (x y dir))
;; Invader is (make-invader Number Number Integer[-1, 1])
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader moves INVADER-SPEED-X pixels per clock tick left if dir -1, right if dir 1
(define I1 (make-invader 150 100 1))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -1))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 1)) ;> landed, moving right


#;
(define (fn-for-invader i)
  (... (invader-x i) (invader-y i) (invader-dir i)))


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


;; ListOfMissile is one of:
;; - empty
;; - (cons Missle ListOfMissle)
;; interp. a list of missles

(define LOM1 empty)
(define LOM2 (cons M1 empty))
(define LOM3 (cons M2 (cons M3 empty)))
(define LOM4 (cons M1 (cons M2 (cons M3 empty))))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]                    ;BASE CASE
        [else (... (first lom)                  ;Missle
                   (fn-for-lom (rest lom)))]))  ;NATURAL RECURSION


;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Missle ListOfMissle)
;; - self-reference: (rest lom) is ListOfMissle


;; ListOfInvader is one of:
;; - empty
;; - (cons Invader ListOfInvader)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (cons I1 empty))
(define LOI3 (cons I2 (cons I3 empty)))
(define LOI4 (cons I1 (cons I2 (cons I3 empty))))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]                    ;BASE CASE
        [else (... (first loi)                  ;Invader
                   (fn-for-loi (rest loi)))]))  ;NATURAL RECURSION


;; Template rules used:
;; - one of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons Invader ListOfInvader)
;; - self-reference: (rest loi) is ListOfInvader




;;===============================================================================================================
;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty (make-tank(/ WIDTH 2) 1)))
 
(define (main g)
  (big-bang g                    
            (on-tick   tock)            ; Game -> Game
            (to-draw   render-game)     ; Game -> Image
            (on-key    handle-key)      ; Game KeyEvent -> Image 
            ))      

;; Game -> Game
;; produce the next state of a game
;;(check-expect (tock (make-game (list I1) (list M1) T1)) (make-game (list I1) (list M1) T1))

;(define (tock g) g)  ;stub
(define (tock g)
  (cond
    [(and (boolean? g) (not g)) false]
    [(game-over? g) false]
    [else (make-game (new-invader (move-invaders (remove-invaders (game-invaders g) (game-missiles g))))
             (move-missiles (remove-missiles (game-missiles g) (game-invaders g)))
             (move-tank (game-tank g)))]))

;; ListOfInvader ListOfMissile -> ListOfInvader
;; removes invaders which were hit by missiles
(check-expect (remove-invaders empty empty) empty)
(check-expect (remove-invaders empty (cons (make-missile 150 100) empty)) empty)
(check-expect (remove-invaders (cons (make-invader 150 100 12) (cons (make-invader 50 20 3) empty)) empty) (cons (make-invader 150 100 12) (cons (make-invader 50 20 3) empty)))
(check-expect (remove-invaders (cons (make-invader 150 100 12) (cons (make-invader 50 20 3) empty)) (cons (make-missile 150 100) empty)) (cons (make-invader 50 20 3) empty)) 

;(define (remove-invaders loi lom) loi) ;stub
(define (remove-invaders loi lom)
  (cond [(empty? loi) empty]
        [(empty? lom) loi]
        [else (if (invader-hit? (first loi) lom)
                  (remove-invaders (rest loi) lom)
                  (cons (first loi) (remove-invaders (rest loi) lom)))]))  

;; Invader ListOfMissiles -> Boolean
;; returns true if a given invader was hit by any missile
(check-expect (invader-hit? (make-invader 150 100 1) (cons (make-missile 150 300) (cons (make-missile 50 30) empty))) false)
(check-expect (invader-hit? (make-invader 50 10 -1) (cons (make-missile 15 300) (cons (make-missile 50 10) empty))) true)
(check-expect (invader-hit? (make-invader 50 100 1) (cons (make-missile 45 100) (cons (make-missile 50 10) empty))) true)
(check-expect (invader-hit? (make-invader 50 100 -1) (cons (make-missile 60 110) (cons (make-missile 60 90) empty))) true)

;(define (invader-hit? i lom) false) ;stub
(define (invader-hit? i lom)
  (cond [(empty? lom) false]
        [else (or (hit? i (first lom))              
                  (invader-hit? i (rest lom)))]))

;; Invader Missile -> Boolean
;; produces true if the x and y position of invader and missile are the same
(check-expect (hit? (make-invader 30 30 1) (make-missile 30 30)) true)
(check-expect (hit? (make-invader 30 30 1) (make-missile 50 30)) false)
(check-expect (hit? (make-invader 100 100 -1) (make-missile 105 110)) true)
(check-expect (hit? (make-invader 100 100 -1) (make-missile 99 110)) true)
(check-expect (hit? (make-invader 100 100 1) (make-missile 100 120)) false)
(check-expect (hit? (make-invader 100 100 -1) (make-missile 89 100)) false)

;(define (hit? i m) false) ;stub
(define (hit? i m)
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))

        
;; ListOfInvader -> ListOfInvader
;; updates the position of all invaders
(check-expect (move-invaders empty) empty)  
(check-expect (move-invaders LOI3) (cons (make-invader (+ 150 (* INVADER-X-SPEED -1)) (+ HEIGHT INVADER-Y-SPEED) -1) (cons (make-invader (+ 150 (* INVADER-X-SPEED 1)) (+ HEIGHT 10 INVADER-Y-SPEED) 1) empty)) ) 

;(define (move-invaders loi) loi) ;stub
(define (move-invaders loi)
  (cond [(empty? loi) empty]                          
        [else (cons (move-invader (first loi))      
                    (move-invaders (rest loi)))]))  

;; Invader -> Invader
;; updates the position of an invader
(check-expect (move-invader I1) (make-invader (+ 150 (* INVADER-X-SPEED 1)) (+ 100 INVADER-Y-SPEED) 1))
(check-expect (move-invader I3) (make-invader (+ 150 (* INVADER-X-SPEED 1)) (+ HEIGHT 10 INVADER-Y-SPEED) 1))

;(define (move-invader i) i) ;stub
(define (move-invader i)
  (cond [(> (invader-x i) INVADER-TURNR) (make-invader (+ (invader-x i) (* INVADER-X-SPEED (- 0 (invader-dir i)))) (+ (invader-y i) INVADER-Y-SPEED) (- 0 (invader-dir i)))]
        [(< (invader-x i) INVADER-TURNL) (make-invader (+ (invader-x i) (* INVADER-X-SPEED (- 0 (invader-dir i)))) (+ (invader-y i) INVADER-Y-SPEED) (- 0 (invader-dir i)))]
        [else (make-invader (+ (invader-x i) (* INVADER-X-SPEED (invader-dir i))) (+ (invader-y i) INVADER-Y-SPEED) (invader-dir i))]
))

;; ListOfInvader -> ListOfInvader
;; creates a new invader at a random position x  and adds it to the list
(check-random (new-invader empty) (cond [(> INVADE-RATE (random 2000)) (cons (make-invader (random WIDTH) 10 1) empty)] [else empty]))
(check-random (new-invader LOI3) (cond [(> INVADE-RATE (random 2000)) (cons (make-invader (random WIDTH) 10 1) LOI3)] [else LOI3]))

;(define (new-invader loi) loi)  ;stub
(define (new-invader loi)
  (cond [(> INVADE-RATE (random 2000)) (cons (make-invader (random WIDTH) 10 1) loi)]        
        [else loi]))   

;; ListOfMissile ListOfInvader -> ListOfMissile
;; removes missiles that hit any invader
(check-expect (remove-missiles (cons (make-missile 20 20) (cons (make-missile 150 100) empty)) (cons (make-invader 150 100 12) (cons (make-invader 50 20 3) empty))) (cons (make-missile 20 20) empty))

;(define (remove-missiles lom loi) lom) ; stub
(define (remove-missiles lom loi)
  (cond [(empty? lom) empty]
        [(missile-hit? (first lom) loi) (remove-missiles (rest lom) loi)]
        [else (cons (first lom) (remove-missiles (rest lom) loi))]))


;; Missile ListOfInvader -> Boolean
;; returns true if a given missile hit any invader
(check-expect (missile-hit? (make-missile 150 10) (cons (make-invader 150 300 1) (cons (make-invader 150 15 1) empty))) true)
(check-expect (missile-hit? (make-missile 50 10) (cons (make-invader 15 300 -1) (cons (make-invader 50 100 -1) empty))) false)

;(define (missile-hit? m loi) false) ;stub
(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else (or (m-hit? m (first loi))              
                  (missile-hit? m (rest loi)))]))

;; Missile Invader -> Boolean
;; produces true if the x and y position of invader and missile are within a HIT-RANGE
(check-expect (m-hit? (make-missile 30 30) (make-invader 30 30 1)) true)
(check-expect (m-hit? (make-missile 50 30) (make-invader 30 30 1)) false)


;(define (m-hit? m i) false) ;stub 
(define (m-hit? m i)
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))

;; ListOfMissile -> ListOfMissile
;; updates the position of all missiles
(check-expect (move-missiles (cons (make-missile 20 20)(cons (make-missile 150 100) empty))) (cons (make-missile 20 (- 20 MISSILE-SPEED)) (cons (make-missile 150 (- 100 MISSILE-SPEED)) empty)))

;(define (move-missiles lom) lom) ;stub
(define (move-missiles lom)
  (cond [(empty? lom) empty]                          
        [else (cons (move-missile (first lom))      
                    (move-missiles (rest lom)))])) 

;; Missile -> Missile
;; updates the position of a missile
(check-expect (move-missile (make-missile 10 10)) (make-missile 10 (- 10 MISSILE-SPEED)))
(check-expect (move-missile (make-missile 100 40)) (make-missile 100 (- 40 MISSILE-SPEED)))

;(define (move-missile m) m) ;stub
(define (move-missile m) 
  (make-missile (missile-x m)  (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; updates the position of a tank
(check-expect (move-tank (make-tank 50 -1)) (make-tank (+ 50 (* TANK-SPEED -1)) -1)) 

;(define (move-tank t) t) ;stub
(define (move-tank t)
  (cond [(< (tank-x t) (/ (image-width TANK) 2)) (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) 1)]
        [(> (tank-x t) (- WIDTH (/ (image-width TANK) 2))) (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) -1)]
        [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))

;; Game -> Game
;; turns the tank to the left direction
(check-expect (change-direction-to-left (make-game empty (list M1 M2) T1))
              (make-game empty (list M1 M2) (make-tank (tank-x T1) -1)))

;(define (change-direction-to-left g) g) ;stub
(define (change-direction-to-left game) 
  (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) -1)))

;; Game -> Game 
;; turns the tank to the right
(check-expect (change-direction-to-right (make-game empty (list M1 M2) T1))
  (make-game empty (list M1 M2) (make-tank (tank-x T1) 1)))

;(define (change-direction-to-right g) g) ;stub
(define (change-direction-to-right game)
  (make-game (game-invaders game) (game-missiles game) (make-tank (tank-x (game-tank game)) 1))
)
 
;;=============================================================================================


;; Game -> Image
;; render the images of tank, missiles and invaders on a screen 

;(define (render-game g)  BACKGROUND) ;stub

(define (render-game g)
  (cond [(and (boolean? g) (not g)) GAME_OVER_BACKGROUND]
  [else (render-tank (game-tank g)
               (render-missiles (game-missiles g)
                                (render-invaders (game-invaders g))))]))


;; ListOfMissile Image -> Image
;; renders all missiles on the background
(check-expect (render-missiles empty BACKGROUND) BACKGROUND)

(check-expect (render-missiles (cons (make-missile 20 40) empty) BACKGROUND)
                (place-image MISSILE 20 40 BACKGROUND))

(check-expect (render-missiles (cons (make-missile 70 40) (cons (make-missile 20 40) empty)) BACKGROUND)
                (place-image MISSILE 70 40 (place-image MISSILE 20 40 BACKGROUND)))

;(define render-missiles lom img) img) ;stub
(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else (render-missile (first lom) (render-missiles (rest lom) img))]
        ))

;; Missile Image -> Image
;; renders a missile on the background
(check-expect (render-missile (make-missile 20 40) BACKGROUND) (place-image MISSILE 20 40 BACKGROUND))

;(define (render-missile m img) img) ;stub
(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))



;; ListOfInvader -> Image
;; renders all invaders on the background
(check-expect (render-invaders empty) BACKGROUND)

(check-expect (render-invaders (cons (make-invader 20 40 1) empty))
                (place-image INVADER 20 40 BACKGROUND))

(check-expect (render-invaders (cons (make-invader 70 40 1) (cons (make-invader 20 40 1) empty)))
                (place-image INVADER 70 40 (place-image INVADER 20 40 BACKGROUND)))

;(define render-invaders loi) img) ;stub
(define (render-invaders loi)
  (cond [(empty? loi) BACKGROUND]
        [else (render-invader (first loi) (render-invaders (rest loi)))]
        ))

;; Invader Image -> Image
;; renders an invader on the background
(check-expect (render-invader (make-invader 20 40 1) BACKGROUND) (place-image INVADER 20 40 BACKGROUND))

;(define (render-invader i img) img) ;stub
(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))


;; Tank Image -> Image
;; renders the image of a tank
(define INI (render-invaders (cons (make-invader 70 40 1) (cons (make-invader 20 70 1) empty))))

(check-expect (render-tank (make-tank 30 1) BACKGROUND)
     (place-image TANK 30 (- HEIGHT 25) BACKGROUND))

(check-expect (render-tank (make-tank 30 1) INI)
     (place-image TANK 30 (- HEIGHT 25) INI))

;(define (render-tank t img) img) ;stub
(define (render-tank g img)
  (place-image TANK (tank-x g) (- HEIGHT 25) img))


;;==============================================================================================  

;; Game KeyEvent -> Game
;; detects the press of right, left arrow keys and spacebar, then renders the required image 

;(define (handle-key g ke) g) ;stub
(define (handle-key g ke)  
  (cond [(key=? ke " ") (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) (- HEIGHT 35)) (game-missiles g)) (game-tank g))]
        [(and (key=? ke "right") (= -1 (tank-dir (game-tank g)))) (change-direction-to-right g)]
        [(and (key=? ke "left") (= 1 (tank-dir (game-tank g)))) (change-direction-to-left g)]
        [else g])) 

;;=============================================================================================


;; Game -> Boolean
;; ends the game if any of the invaders reaches the bottom edge
;; !!!

;; Game -> Boolean
;; produces true if the game is over

;(define (game-over? g) false) ;stub
(define (game-over? g)
  (hit-bottom? (game-invaders g)))

;; ListOfInvader -> Boolean
;; produces true if any of invaders reaches the bottom of the screen

;(define (hit-bottom? loi) false) ;stub
(define (hit-bottom? loi)
  (cond 
    [(empty? loi) false]
    [else 
      (or (> (invader-y (first loi)) (- HEIGHT (image-height TANK) 25))
       (hit-bottom? (rest loi)))
      ]))