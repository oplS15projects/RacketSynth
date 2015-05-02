; RacketSynth
; by David Lordan, Kevin Ornellas, & Steven Scheffelaar
;

#lang racket
(require racket/gui/base)
(require racket/include)
(require rsound)

;;Main window to store the GUI
(define mainWindow (new frame% 
                        [label "RacketSynth"]
                        [style (list 'no-resize-border)]))


                 
;Image of keyboard map. 
(define keyBitMap (read-bitmap "./images/keymap.png"))


;;Main panel to organize GUI
(define mainCont (new horizontal-panel% 
                      [parent mainWindow]))

;Left panel to store wave types and current sounds.
(define leftPanel (new vertical-panel% 
                       [parent mainCont]))

;Right panel to store input types.
(define rightPanel (new vertical-panel% 
                        [parent mainCont]))

;Panel to store keyboard map.
(define bottomPanel (new horizontal-panel% 
                         [parent mainWindow]))

;Message field to display keyboard map. 
(define mapMsg (new message% 
                    [label keyBitMap]
                    [parent bottomPanel]))


;Used to display the radio box for selecting wave types
(define wavePanel (new group-box-panel% 
                       [parent leftPanel]
                       [label ""]))

;Used to store main input types.
(define freqPanel (new group-box-panel% 
                       [parent rightPanel]
                       [label ""]
                       [min-height 80]))

;Shows frequency constraints to user. 
(define freq-message (new message%
                          [label "Enter a frequency between 109 and"]
                          (parent rightPanel)))
                          
;Text field to accept user frequency.                          
(define freq-field (new text-field%
                        [label "1721 Hz:"]
                        (parent rightPanel)
                        (init-value "440")
                        [callback (lambda (text event)
                                    (send freq-box set-selection 0))]))

;Slider for selecting the pulse wave duty-cycle, which is mapped to a 0-1 value.
(define pulseSlide (new slider%
                        [label "Pulse Wave Duty-Cycle"]
                        [parent leftPanel]
                        [min-value 1]
                        [max-value 100]))

;GUI message to show multiple notes contraint.
(define currSounds-msg (new message%
                          [label "You may have up to 7 sounds at once."]
                          (parent leftPanel)))

;Panel to store pause and play buttons.
(define buttonPanel (new horizontal-panel% [parent rightPanel]))

;Panel to store the octave shift buttons.
(define octavePanel (new horizontal-panel%
                         (parent rightPanel)))

;Displays the current octave setting to the user.
(define octMsg (new message%
                    [label "Current octave offset: 0"]
                    [parent rightPanel]))

;Procedure to retrieve text from a text-field. 
(define myGetText
  (lambda (thisText)
    (send (send thisText get-editor) get-text)))

;Bitmap for the play button. 
(define my-play-bitmap-1 (read-bitmap "./images/play_icon3.png"))

;Button to play a sound based on the current input types and their parameters. 
;Using default parameters 1,2 or .5,2 which were used for testing the play-note procedure.
;This procedure is called by the user.
(define playSound (new button%
                       (parent buttonPanel)
                       (label my-play-bitmap-1)
                       [callback (lambda (button event)
                                    (if(equal? "Notes" (radio-check freq-box))
                                       (play-Note (radio-check note-box) 1 2)
                                       (my-play (string->number (myGetText freq-field)) .5 2)))]))

;Bit map for the stop button. 
(define my-stop-bitmap-1 (read-bitmap "./images/stop_icon2.png"))

;Button to stop all current sounds. 
(define stopSound (new button%
                       (parent buttonPanel)
                       (label  my-stop-bitmap-1)
                       [callback ( lambda (check-box event)
                                    (myStop))]))

;Used to kep track of and test keyboard input. 
(define (setKeyInput bool)
  (if bool
      (begin(controller "Keyboard input enabled.")
            (send keyInput show #t)
            (send keyInput enable #t)
            (send keyInput focus))
      (begin(controller "Keyboard input disabled.")
            (send keyInput show #f)
            (send keyInput enable #f))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;A text field used to monitor keyboard input. When the user enters a key, the value
;is retrieved and the text field is immediately set to "", clearing the field. 
(define keyInput (new text-field% [parent rightPanel]
                      [label ""]
                      [min-height 0]
                      [callback (lambda (text event)
                                  (begin (keySound (myGetText keyInput))
                                         (send keyInput set-value "")))]
                      [enabled #f]))

;Panel to store the keyboard input. 
(define keyInputPanel (new horizontal-panel%
                           (parent rightPanel)))

;Check-box to allow the user to have the option of playing multiple notes with the computer
;keyboard at once. 
(define sustainCheck-Box (new check-box%
                              (parent keyInputPanel)
                              (label "Allow multiple notes?")
                              [callback (lambda(check-box event) (updateGUI))]
                              (value #f)))

;Bitmap for the radio image. 
(define my-radio-bitmap-1 (read-bitmap "./images/radio.png"))
;Radio image, which looks nice.
(define radio-message (new message% (label my-radio-bitmap-1)
                            (parent buttonPanel)))

;Panel to show a list of all current sounds.
(define currentSounds (new group-box-panel% [parent leftPanel]
                           [label "Current Sounds"]
                           [min-height 345]))

;Panel to show a list of notes. 
(define notePanel (new group-box-panel% [parent rightPanel]
                       [label "Notes"]
                       [min-width 250]
                       [min-height 200]))

;GUI message prompting the user which type of wave to create.
(define waveMsg (new message%
                     (parent wavePanel)
                     (label "Select a type of wave to generate.")))

;Grabst the current wave-type selection.
(define (radio-check radio)
  (send radio get-item-label(send radio get-selection)))

;Radio-box allowing the user to select which type of wave they would like to generate. 
(define wave-box (new radio-box%
                      (label "")
                      (parent wavePanel)
                      (choices (list "Sine Wave"
                                     "Sawtooth Wave"
                                     "Square Wave"
                                     "Pulse Wave"))
                      [callback ( lambda (button event)
                                   (send pulseSlide show (equal? (radio-check wave-box) "Pulse Wave"))
                                   (controller (string-append "User selected " (radio-check wave-box))))]))

;Radio-box allowing the user to select which type of input they would like to use. 
(define freq-box (new radio-box%
                      (label "")
                      (parent freqPanel)
                      (choices (list "Frequency"
                                     "Keyboard"
                                     "Notes"))
                      [style (list 'vertical)]
                      [callback ( lambda (button event)
                                   (updateGUI))]))

;List of musical notes. 
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
                                   (send freq-box set-selection 2)
                                   (controller (string-append "User selected " (radio-check note-box))))]))

;Buttons to shift the current octave up or down.
(define octDown (new button%
                     [label "Octave Down"]
                     [parent octavePanel]
                     [callback (lambda (button event)
                                 (octave-shift 'down))]))
(define octUp (new button%
                   [label "Octave Up"]
                   [parent octavePanel]
                   [callback (lambda (button event)
                               (octave-shift 'up))]))

;Global variable that stores the current octave setting.
(define octave 1)
(define (octave-shift direction)
  (if(equal? direction 'up)
     (cond ((equal? octave .5) (begin(set! octave 1)
                                     (send octMsg set-label "Current octave offset: 0")))
           ((equal? octave 1) (begin(set! octave 2)
                                    (send octMsg set-label "Current octave offset: +1"))))
     (cond ((equal? octave 2) (begin(set! octave 1)
                                    (send octMsg set-label "Current octave offset: 0")))
           ((equal? octave 1) (begin(set! octave .5)
                                    (send octMsg set-label "Current octave offset: -1"))))))


;Updates the GUI, called whenever a user changes a visible setting. Allows for easy turning
;on and off various buttons and messages so that only the appropriate GUI objects are shown
;to the user for a particular setting.

;I really like this procedure. I was tempted to have the updateGUI procedure take a series of boolean
;values and to set each GUI object according. Having it unfactored like this actually makes it easier
;to read and update, which I found interesting. 
(define (updateGUI)
  (begin
    (display "update")
    (myStop)
    (set! octave 1)
    (cond ((equal? (radio-check freq-box) "Frequency") (begin
                                                         (send notePanel show #f)
                                                         (send freq-field show #t)
                                                         (setKeyInput #f)
                                                         (send sustainCheck-Box show #f)
                                                         (send buttonPanel show #t)
                                                         (send octavePanel show #f)
                                                         (send currSounds-msg show #t)
                                                         (send freq-message show #t)
                                                         (send mapMsg show #f)
                                                         (send octMsg show #f)))
          
          ((equal? (radio-check freq-box) "Notes") (begin
                                                     (send notePanel show #t)
                                                     (send freq-field show #f)
                                                     (setKeyInput #f)
                                                     (send sustainCheck-Box show #f)
                                                     (send buttonPanel show #t)
                                                     (send octavePanel show #t)
                                                     (send currSounds-msg show #t)
                                                     (send freq-message show #f)
                                                     (send mapMsg show #f)
                                                     (send octMsg show #t)))
          
          ((equal? (radio-check freq-box) "Keyboard") (begin
                                                        (send notePanel show #f)
                                                        (send freq-field show #f)
                                                        (setKeyInput #t)
                                                        (send sustainCheck-Box show #t)
                                                        (send buttonPanel show #f)
                                                        (send octavePanel show #t)
                                                        (send freq-message show #f)
                                                        (send mapMsg show #t)
                                                        (send currSounds-msg show (send sustainCheck-Box get-value))
                                                        (send octMsg show #t))))))


; Sets up the intial GUI configuration. 
(send sustainCheck-Box show #f)
(send octMsg show #f)
(send octavePanel show #f)
(send notePanel show #f)
(send pulseSlide show #f)
(send keyInput show #f)
(send mainWindow show #t)
(send mapMsg show #f)
(define numOfSounds 0)

;**************************Abstraction barrier (sort-of)*************************************************
;Used to monitor user actions, for testing. 
(define (controller string)
  (display string)
  (newline))

;If the user is playing a musical not by name, the value is translated into a 
;frequency. 
(define (play-Note note vol time)
  (define note-freq 0)
  ;(display note)
  (cond ((equal? "A" note) (set! note-freq 220))
        ((equal? "Bb" note) (set! note-freq 233.08))
        ((equal? "B" note) (set! note-freq 246.94))
        ((equal? "C" note) (set! note-freq 261.63))
        ((equal? "Db" note) (set! note-freq 277.18))
        ((equal? "D" note) (set! note-freq 293.66))
        ((equal? "Eb" note) (set! note-freq 311.13))
        ((equal? "E" note) (set! note-freq 329.63))
        ((equal? "F" note) (set! note-freq 349.23))
        ((equal? "Gb" note) (set! note-freq 369.99))
        ((equal? "G" note) (set! note-freq 392))
        ((equal? "Ab" note) (set! note-freq 415.30))
        ((equal? "A+" note) (set! note-freq 440))
        ((equal? "Bb+" note) (set! note-freq 466.16))
        ((equal? "B+" note) (set! note-freq 493.188))
        ((equal? "C+" note) (set! note-freq 523.26)))
  (my-play note-freq vol time))

;Grabs the wave-type from the wave radio-box and returns the appropriate rsound procedure. 
(define (get-wave-type)
  (cond ((equal? (radio-check wave-box) "Sine Wave") sine-wave)
        ((equal? (radio-check wave-box) "Sawtooth Wave") sawtooth-wave)
        ((equal? (radio-check wave-box) "Square Wave") square-wave)
        ((equal? (radio-check wave-box) "Pulse Wave") pulse-wave)))

;Retrieves the pulse-wave duty-cycle value that is set by the user.
(define getPulse
  (lambda ()
    (send pulseSlide get-value)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;HEART OF THE PROJECT - the procedure from whence all sounds depart 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Checks that the user input is within the program's contraints.
;Checks for the wave-type to create and generates the appropriate sounds.
;Also calls procedures to update the current sound list. 
(define (my-play relFreq vol time)
  (let ([freq (* octave relFreq)])
    (if (and (< freq 1721)(< 109 freq))
        (begin
          (if (<= numOfSounds 6)
              (begin
                (set! numOfSounds (+ 1 numOfSounds))
                (addSound freq)
                (if (equal? (radio-check wave-box) "Pulse Wave")
                    (begin
                      (let ([signal (network ()
                                             [a <= pulse-wave (/ (getPulse) 100) freq]
                                             [out = (* .7 vol a)])])
                        (signal-play signal)))
                    (begin
                      (cond((equal? (radio-check wave-box) "Sine Wave")(set! vol (* vol 10))))
                      (let ([signal (network ()
                                             [a <= (get-wave-type) freq]
                                             [out = (* .3 vol a)])])
                        (signal-play signal)))))
              (begin
                (newline)
                (display "Too many sounds!"))))
        (display "Frequency out of range, must be between 109 and 1720 Hz."))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Checks what key the user presses and translates to an appropriate call to play-Note.
(define (keySound char)
  (cond ((not(send sustainCheck-Box get-value)) (myStop)))
  ;(display char)
  (cond ((equal? "a" char) (play-Note "C" 1 2))
        ((equal? "w" char) (play-Note "Db" 1 2))
        ((equal? "s" char) (play-Note "D" 1 2))
        ((equal? "e" char) (play-Note "Eb" 1 2))
        ((equal? "d" char) (play-Note "E" 1 2))
        ((equal? "f" char) (play-Note "F" 1 2))
        ((equal? "t" char) (play-Note "Gb" 1 2))
        ((equal? "g" char) (play-Note "G" 1 2))
        ((equal? "y" char) (play-Note "Ab" 1 2))
        ((equal? "h" char) (play-Note "A+" 1 2))
        ((equal? "u" char) (play-Note "Bb+" 1 2))
        ((equal? "j" char) (play-Note "B+" 1 2))
        ((equal? "k" char) (play-Note "C+" 1 2))
        ((equal? "z" char) (octave-shift 'down))
        ((equal? "x" char) (octave-shift 'up))
        ((equal? " " char) (myStop))))

;This is weird and hacky. All message must have initalized with a string and any string that
;message is then set to may not exceed the original string in length. In other words, empty strings
;are 'permanent'. By setting temporarily empty messages to a large blank string, space in set aside
;for actual strings the message may be set to. 
(define filler "                                                                  ")

;Current sound messages.
(define currSound1 (new message%
                        [label filler]
                        [parent currentSounds]))
(define currSound2 (new message%
                        [label filler]
                        [parent currentSounds]))
(define currSound3 (new message%
                        [label filler]
                        [parent currentSounds]))
(define currSound4 (new message%
                        [label filler]
                        [parent currentSounds]))
(define currSound5 (new message%
                        [label filler]
                        [parent currentSounds]))
(define currSound6 (new message%
                        [label filler]
                        [parent currentSounds]))
(define currSound7 (new message%
                        [label filler]
                        [parent currentSounds]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; The list of current sounds is initially empty. 
(define soundList '())

;Updates the current sound list, 
(define (addSound freq)
  (begin
    (set! soundList (append soundList (list (number->string freq))))
    (display soundList)
    (newline)
    (cond ((= numOfSounds 1)(send currSound1 set-label (getSoundString freq)))
          ((= numOfSounds 2)(send currSound2 set-label (getSoundString freq)))
          ((= numOfSounds 3)(send currSound3 set-label (getSoundString freq)))
          ((= numOfSounds 4)(send currSound4 set-label (getSoundString freq)))
          ((= numOfSounds 5)(send currSound5 set-label (getSoundString freq)))
          ((= numOfSounds 6)(send currSound6 set-label (getSoundString freq)))
          ((= numOfSounds 7)(send currSound7 set-label (getSoundString freq))))))

; Displays the last series of generated sounds, their
; frequency and wave type. 
(define (getSoundString freq)
  (string-append "Now playing a " (radio-check wave-box) " at " (number->string freq) " hertz."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Custom stop procedure which not only stops all current sounds, but clears the
;current sound messages; 
(define (myStop)
  (begin
    (send currSound1 set-label filler)
    (send currSound2 set-label filler)
    (send currSound3 set-label filler)
    (send currSound4 set-label filler)
    (send currSound5 set-label filler)
    (send currSound6 set-label filler)
    (send currSound7 set-label filler)
    (set! soundList '())
    (set! numOfSounds 0)
    (stop)))

;Party's over. 