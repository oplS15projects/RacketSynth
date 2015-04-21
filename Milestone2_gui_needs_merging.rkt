#lang racket

(require racket/gui/base)
(require mrlib/bitmap-label)
(require racket/draw)

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



; Code to prepare to supply image to play

(define my-play-bitmap-1 (read-bitmap "./images/play_icon3.bmp3"))


(define playSound (new button%
                    (parent buttonPanel)
                    (label my-play-bitmap-1)))


; code to prepare to have stopSound bitmap label



; 

(define my-stop-bitmap-1 (read-bitmap "./images/stop_icon2.bmp3"))


(define stopSound (new button%
                    (parent buttonPanel)
                    (label my-stop-bitmap-1)))

(define my-volume-bitmap-1 (read-bitmap "./images/volume4.bmp3"))

(define volume-message (new message% (label my-volume-bitmap-1)
                         (parent buttonPanel)
                         ))

(define volume-slider (new slider%
                           (label "")
                                  (parent buttonPanel)
                                  (min-value 0)
                                  (max-value 100)
                                  (init-value 20)))


(define keyboardCheck-Box (new check-box%
                       (parent leftPanel)
                       (label "Use Keyboard Input")
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

(define wave-box (new radio-box%
                      (parent wavePanel)
                       (label "Waves")                    
                       
                       (choices (list "Sine Wave"
                                      "Sawtooth Wave"
                                      "Square Wave"
                                      "Pulse Wave"))))

(define freq-box (new radio-box%
                       (label "")
                       (parent freqPanel)
                       (choices (list "Frequency"
                                      "Notes"))
                       [style (list 'horizontal)]))

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
                                      "Ab"))))

; Show the frame by calling its show method
(send mainWindow show #t)

