#lang racket

(require racket/gui/base)
(require racket/include)
(require rsound)

; Make a frame by instantiating the frame% class
(define mainWindow (new frame% [label "RacketSynth"]
                               [style (list 'no-resize-border)]))

 
;; Panel to contain the 'click here' and 'clear' buttons
(define mainCont (new horizontal-panel% [parent mainWindow]))
(define leftPanel (new vertical-panel% [parent mainCont]))
(define rightPanel (new vertical-panel% [parent mainCont]))


(define wavePanel (new group-box-panel% [parent leftPanel]
                       [label ""]))

(define freqPanel (new group-box-panel% [parent rightPanel]
                       [label ""]
                       [min-height 80]))

(define buttonPanel (new horizontal-panel% [parent leftPanel]))

(define myGetText 
  (lambda (thisText)
  (send (send thisText get-editor) get-text)))

(define playSound (new button%
                    (parent buttonPanel)
                    (label "Play Sound")
                    [callback ( lambda (button event)
                    
                    (if(equal? "Notes" (radio-check freq-box))
                       (play-Note (radio-check note-box) 1 2)
                       (my-play (string->number (myGetText freq-field)) .5 2)))]))

(define stopSound (new button%
                    (parent buttonPanel)
                    (label "Stop Sound")
                    [callback ( lambda (check-box event)
                      (stop))]))

(define (keyCheck-State) (send keyboardCheck-Box get-value))
(define keyboardCheck-Box (new check-box%
                       (parent leftPanel)
                       (label "Use Keyboard Input")
                       [callback ( lambda (button event)
                     (if (keyCheck-State)
                        (begin(controller "Keyboard input enabled.")
                              (send keyInput show #t)
                              (send keyInput enable #t)
                              (send keyInput focus))
                        (begin(controller "Keyboard input disabled.")
                              (send keyInput show #f)
                              (send keyInput enable #f))))]
                      (value #f)))

(define keyInput (new text-field% [parent leftPanel]
                       [label ""]
                       [min-height 0]
                       [callback (lambda (text event)
                                   (begin(display (myGetText keyInput))
                                         (newline)
                                         (send keyInput set-value "")))]
                       [enabled #f]))

(define currentSounds (new group-box-panel% [parent leftPanel]
                       [label "Current Sounds"]
                       [min-height 165]))

(define notePanel (new group-box-panel% [parent rightPanel]
                       [label "Notes"]
                       [min-width 250]
                       [min-height 200]))

(define waveMsg (new message%
                     (parent wavePanel)
                     (label "Select a type of wave to generate.")))

(define (radio-check radio)
  (send radio get-item-label(send radio get-selection)))
  
(define wave-box (new radio-box%
                       (label "")
                       (parent wavePanel)
                       (choices (list "Sine Wave"
                                      "Sawtooth Wave"
                                      "Square Wave"
                                      "Pulse Wave"))  
                     [callback ( lambda (button event)
                     (controller (string-append "User selected " (radio-check wave-box))))]))

(define freq-box (new radio-box%
                       (label "")
                       (parent freqPanel)
                       (choices (list "Frequency"
                                      "Notes"))
                       [style (list 'horizontal)]
                       [callback ( lambda (button event)
                     (controller (string-append "User selected " (radio-check freq-box))))]))

(define freq-field (new text-field%
                        (label "Enter a frequency:")
                        (parent freqPanel)
                        (init-value "440")))


(define note-box (new radio-box%
                       (label "")
                       (parent notePanel)
                       (choices (list "A"
                                      "Bb"
                                      "B"
                                      "C"
                                      "Db"
                                      "D"
                                      "Eb"
                                      "E"
                                      "F"
                                      "Gb"
                                      "G"
                                      "Ab"))
                       [callback ( lambda (button event)
                     (controller (string-append "User selected " (radio-check note-box))))]))

; Show the frame by calling its show method
(send keyInput show #f)
(send mainWindow show #t)

;***********************************************************************************************

(define (controller string)
  (display string)
  (newline))

(define (play-Note note vol time)
  (define note-freq 0)
  (display note)
  
  (cond ((equal? "A" note)   (set! note-freq 220))
        ((equal? "Bb" note)  (set! note-freq 233.08))
        ((equal? "B" note)   (set! note-freq 246.94))
        ((equal? "C" note)   (set! note-freq 261.63))
        ((equal? "Db" note)  (set! note-freq 277.18))
        ((equal? "D" note)  (set! note-freq 293.66))
        ((equal? "Eb" note)   (set! note-freq 311.13))
        ((equal? "E" note)   (set! note-freq 329.63))
        ((equal? "F" note)  (set! note-freq 349.23))
        ((equal? "Gb" note)   (set! note-freq 369.99))
        ((equal? "G" note)  (set! note-freq 392))
        ((equal? "Ab" note)   (set! note-freq 415.30)))
  
  (my-play note-freq vol time))


(define (get-wave-type)
  
  (cond ((equal? (radio-check wave-box) "Sine Wave")     sine-wave)
        ((equal? (radio-check wave-box) "Sawtooth Wave") sawtooth-wave)
        ((equal? (radio-check wave-box) "Square Wave")   square-wave)
        ((equal? (radio-check wave-box) "Pulse Wave")    sine-wave)))

(define my-pulse-wave 
  (lambda (x)
  (pulse-wave 1 x)))


(define (my-play freq vol time)
  
  (if (and (< freq 1721)(< 149 freq))
  (let ([signal (network ()
                  [a <= (get-wave-type) freq]
                  [out = (* .1 vol a)])]) 
    (signal-play signal)
    ;(sleep time)
    ;(stop)
    )
    (display "Frequency out of range, must be between 150 and 1720 Hz.")
  ))
  
  

 

 


