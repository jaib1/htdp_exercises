;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname trafficLightWorld) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A program that simulates a traffic light for two cycles.
; Starts on red, stays there for 20 clock ticks, then turns green for
; 15 clock ticks, then yellow for 5, then red, and repeats until 2
; cycles have passed, or if user presses "q". If the user mouse clicks,
; the world is reset.

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
; physical constants
(define WORLD-WIDTH 50)
(define WORLD-HEIGHT 100)
(define LIGHT-X (/ WORLD-WIDTH 2))
(define LIGHT-Y (/ WORLD-HEIGHT 2))

; graphical constants
(define BACKGROUND (empty-scene WORLD-WIDTH WORLD-HEIGHT))
(define RED-LIGHT (place-image (circle (/ WORLD-WIDTH 2) "solid" "red")
                               LIGHT-X LIGHT-Y BACKGROUND))
(define YELLOW-LIGHT (place-image (circle (/ WORLD-WIDTH 2) "solid" "yellow")
                               LIGHT-X LIGHT-Y BACKGROUND))
(define GREEN-LIGHT (place-image (circle (/ WORLD-WIDTH 2) "solid" "green")
                               LIGHT-X LIGHT-Y BACKGROUND))

; 2)
; A State is a number: the time measured in clock-ticks since world
; start.

; 3)
; a)
; "renderLight" renders the light to be displayed, 'x' ticks from
; the start of the world or from the time another light comes on
; State -> Image
; examples:
;  (renderLight 0) >> RED-LIGHT
;  (renderLight 21) >> GREEN-LIGHT
;  (renderLight 42) >> YELLOW-LIGHT
;  (renderLight 48) >> RED-LIGHT
(define (renderLight x)
  (cond
    [(or(<= 0 x 20) (<= 48 x 68) (>= x 96)) RED-LIGHT] ; red light case
    [(or(<= 21 x 41) (<= 69 x 89)) GREEN-LIGHT] ; green light case
    [(or(<= 42 x 47) (<= 90 x 95)) YELLOW-LIGHT] ; yellow light case
    ))

; b)
; "tock" advances the world by one on each tick
; State -> State
; example: (tock 0) >> 1
(define (tock x)
  (+ x 1))

; c)
; "end?" ends the world when 'x' > 100
(define (end? x)
  (cond
    [(> x 100) true]
    [else false]))

; d) mouse callback
; "reset" resets the car to the initial x-position, which is where the
; left edge of the car is at x = 0, when the mouse is clicked
; State mouseX mouseY mouseEvent -> State
(define (reset x mouseX mouseY mouseEvent)
  (cond
    [(equal? mouseEvent "button-down") 0]
    [else x])) ; else just return the same state

; e) keyboard callback
; "endKey" ends the world by changing the state so that "end?"
; returns true when the "q" key is pressed
; State keyEvent -> State
(define (endKey x keyEvent)
  (cond
    [(equal? keyEvent "q") 100]
    [else x])) ; else just return the same state

; 4)
; "main" launches the program, given an initial state
; State -> State
(define (main initState)
  (big-bang initState
    [on-tick tock]
    [to-draw renderLight]
    [on-mouse reset]
    [on-key endKey]
    [stop-when end?]
    ))

; Testing:

(check-expect(renderLight 0) RED-LIGHT)
(check-expect(renderLight 21) GREEN-LIGHT)
(check-expect(renderLight 42) YELLOW-LIGHT)
(check-expect(renderLight 96) RED-LIGHT)
(check-expect(tock 0) 1)
(check-expect(end? 101) true)
(check-expect(end? 99) false)
(check-expect(reset 50 0 0 "button-down") 0)
(check-expect(reset 50 0 0 "move") 50)
(check-expect(endKey 50 "q") 100)
(check-expect(endKey 50 " ") 50)

; run
(main 0)