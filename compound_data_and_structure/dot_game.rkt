;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname dotGame) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Design a program that moves a red dot across a canvas, and allows
; players to use the mouse to reset the dot.

; 1) Define constants for the World (e.g. "physical", "graphical")
; 2) Develop a data representation for all possible World states
; (this can be a data definition of the World)
; 3) Create functions that can render states as images, and
; end the world
;   a) get current state (State -> Image),
;   b) set next state based on current state (State -> State)
;   c) end when... (State -> Boolean)
;   d) keyboard callback (State, keyEvent -> State)
;   e) mouse callback (State, mouseX, mouseY, mouseEvent -> State)
; 4) Create a main function that takes in input args necessary
; to start running the world

; 1)
(define WORLD-WIDTH 100)
(define WORLD-HEIGHT 100)
(define DOTSIZE 3)

(define MTS (empty-scene WORLD-WIDTH WORLD-HEIGHT))
(define DOT (circle DOTSIZE "solid" "red"))

; 2) A State is a Posn representing position: the position (x and y) of
; the red dot (a Posn) in the world

; 3)

; "x+" adds a red dot to the canvas at a specified position
; State -> Image
; example: (scene+dot (make-posn 10 20)) >> (place-image DOT 10 20 MTS))
(define (scene+dot p)
  (let* ([x (posn-x p)]
         [y (posn-y p)])
    (place-image DOT x y MTS)))


; "xy+" advances the world (ball movement in x and y) by one on each clock-tick
; State -> State
; example: (x+ (make-posn 1 1)) >> (make-posn 2 2)
(define (xy+ p)
  (let* ([x (posn-x p)]
         [y (posn-y p)])
    (make-posn (+ x 1) (+ y 1))))

; "end?" ends the world when the x or y position of p > 100
; State -> Boolean
; example: (end? (make-posn 100 101)) >> true
; example: (end? (make-posn 100 100)) >> false 
(define (end? p)
  (let* ([x (posn-x p)]
         [y (posn-y p)])
    (cond
      [(or (> x 100) (> y 100))
       true]
      [else false])))

; "reset-dot" resets the dot to a specified position on a mouse-click
; State mouseX mouseY mouseEvent -> State
; example: (reset-dot (make-posn 50 50) 0 0 "left-down") >> (make-posn 25 25)
; example: (reset-dot (make-posn 50 50) 0 0 "right-down") >> (make-posn 75 75) 
(define (reset-dot p mouseX mouseY mouseEvent)
    (cond
      [(equal? mouseEvent "button-down")
        (make-posn (* WORLD-WIDTH (/ 1 4)) (* WORLD-WIDTH (/ 1 4)))]
      [(equal? mouseEvent "button-up")
        (make-posn (* WORLD-WIDTH (/ 3 4)) (* WORLD-WIDTH (/ 3 4)))]
    [else p])) ; else just return the same state

; 4)

; main launches the program, given an initial state
; State -> State (Posn -> Posn)
(define (main p0)
  (big-bang p0
    [on-tick xy+]
    [on-mouse reset-dot]
    [to-draw scene+dot]
    [stop-when end?]))


; Examples/Testing

; scene+dot

; x+

; reset-dot

; end?

; main