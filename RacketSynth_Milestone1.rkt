#lang racket

(require racket/gui/base)

; Make a frame by instantiating the frame% class
(define mainWindow (new frame% [label "RacketSynth"]
                               [style (list 'no-resize-border)]))

;This will eventually be converted to a procedure that will parse
;user input and call the appropriate sound generation procedure. 
(define (controller string)
  (display string)
  (newline))
 
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
                    (controller 
                     (string-append "Pressed play button with a value of " 
                                    (myGetText freq-field))))]))

(define stopSound (new button%
                    (parent buttonPanel)
                    (label "Stop Sound")
                    [callback ( lambda (check-box event)
                      (controller "Pressed stop button."))]))

(define (keyCheck-State) (send keyboardCheck-Box get-value))
(define keyboardCheck-Box (new check-box%
                       (parent leftPanel)
                       (label "Use Keyboard Input")
                       [callback ( lambda (button event)
                     (if (keyCheck-State)
                        (controller "Keyboard input enabled.")
                        (controller "Keyboard input disabled.")))]
                       (value #t)))

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
                        (init-value "440 Hz")))


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
(send mainWindow show #t)
