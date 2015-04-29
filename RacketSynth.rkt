#lang racket

(require racket/gui/base)
(require racket/include)
(require rsound)

(define currHz 'freq)

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


(define freq-field (new text-field%
                        (label "Enter a frequency:")
                        (parent rightPanel)
                        (init-value "440")
                        [callback (lambda (text event) 
                                    ;(setKeyInput #f)
                                    (send freq-box set-selection 0))]))


;(define buttonMsg1 (new message% 
 ;                      [label "Notes will continue until the"]
  ;                     [parent leftPanel]))
;(define buttonMsg2 (new message% 
 ;                      [label "Stop-Sound button is pressed."]
  ;                     [parent leftPanel]))
;(define buttonMsg3 (new message% 
 ;                      [label "Build some chords!"]
  ;                     [parent leftPanel]))


(define pulseSlide (new slider%
                        [label "Pulse Wave Duty-Cycle"]
                        [parent leftPanel]
                        [min-value 1]
                        [max-value 100]))


(define buttonPanel (new horizontal-panel% [parent rightPanel]))
(define octavePanel (new horizontal-panel%
                         (parent rightPanel)))

(define octMsg (new message%
                   [label "Current octave offset: 0"]
                   [parent rightPanel]))



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
                      (myStop))]))

;(define (keyCheck-State) (send keyboardCheck-Box get-value))


(define (setKeyInput bool)
 (if bool
                        (begin(controller "Keyboard input enabled.")
                              (send keyInput show #t)
                              (send keyInput enable #t)
                              (send keyInput focus))
                        (begin(controller "Keyboard input disabled.")
                              (send keyInput show #f)
                              (send keyInput enable #f)
                              ;(send keyboardCheck-Box set-value #f) 
                              )))
  
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define keyInput (new text-field% [parent rightPanel]
                       [label ""]
                       [min-height 0]
                       [callback (lambda (text event)
                                   (begin (keySound (myGetText keyInput))
                                          (send keyInput set-value "")))]
                       [enabled #f]))

(define keyInputPanel (new horizontal-panel%
                           (parent rightPanel)))

;(define keyboardCheck-Box (new check-box%
 ;                      (parent keyInputPanel)
  ;                     (label "Use Keyboard Input")
   ;                    [callback ( lambda (button event)
    ;                             (setKeyInput (keyCheck-State))
     ;                            (send sustainCheck-Box show (keyCheck-State))  
      ;                           (cond ( (not(keyCheck-State)) (myStop)))
                                    
       ;                             )]
        ;              (value #f)))

(define sustainCheck-Box (new check-box%
                              (parent keyInputPanel)
                              (label "Allow multiple notes?")
                              (value #f)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define currentSounds (new group-box-panel% [parent leftPanel]
                       [label "Current Sounds"]
                       [min-height 345]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




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
                                  (send pulseSlide show (equal? (radio-check wave-box) "Pulse Wave"))
                                  (controller (string-append "User selected " (radio-check wave-box))))]))

(define freq-box (new radio-box%
                       (label "")
                       (parent freqPanel)
                       (choices (list "Frequency"
                                      "Keyboard"
                                      "Notes"))
                       [style (list 'vertical)]
                       [callback ( lambda (button event)
                                   ; (setKeyInput #f)
                                    (updateGUI)
                                    ;(controller (string-append "User selected " (radio-check freq-box)))
                                    
                                    )]))








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
                                   ;(setKeyInput #f)
                                   (controller (string-append "User selected " (radio-check note-box))))]))



(define octDown (new button%
                   [label "Octave Down"]
                   [parent octavePanel]
                   [callback (lambda (button event)
                               (octave-shift 'down)
                               
                               )]))

(define octUp (new button%
                   [label "Octave Up"]
                   [parent octavePanel]
                   [callback (lambda (button event)
                               (octave-shift 'up)
                               
                               )]))

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
                                                                (send octMsg set-label "Current octave offset: -1"))))
     ))



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
                                                       (send octMsg show #f)
                                                      
                                                      ))
        ((equal? (radio-check freq-box) "Notes") (begin
                                                       (send notePanel show #t)
                                                       (send freq-field show #f)
                                                       (setKeyInput #f)
                                                       (send sustainCheck-Box show #f) 
                                                       (send buttonPanel show #t)
                                                       (send octavePanel show #t)
                                                       (send octMsg show #t)
                                                       
                                                      
                                                      )
         
         
         )
        ((equal? (radio-check freq-box) "Keyboard") (begin
                                                       (send notePanel show #f)
                                                       (send freq-field show #f)
                                                       (setKeyInput #t)
                                                       (send sustainCheck-Box show #t)
                                                       (send buttonPanel show #f)
                                                       (send octavePanel show #t)
                                                       (send octMsg show #t)
                                                                                                           
                                 
                                                       
                                                      ))
        
        
        
        
        
        
        
        ))
  
  
  
  )


; Show the frame by calling its show method
(send sustainCheck-Box show #f)
(send octMsg show #f)
(send octavePanel show #f)
(send notePanel show #f)
(send pulseSlide show #f)
(send keyInput show #f)
(send mainWindow show #t)
(define numOfSounds 0)


;***********************************************************************************************

(define (controller string)
  (display string)
  (newline))

(define (play-Note note vol time)
  (define note-freq 0)
  ;(display note)
  
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
        ((equal? "Ab" note)   (set! note-freq 415.30))
        ((equal? "A+" note)   (set! note-freq 440))
        ((equal? "Bb+" note)   (set! note-freq 466.16))
        ((equal? "B+" note)   (set! note-freq 493.188))
        ((equal? "C+" note)   (set! note-freq 523.26))
        
        
        
        
        )
  
 ; (cond (   (equal? )     ()   ) )
 ; (set! note-freq (* note-freq 2))
  
  (my-play note-freq vol time))


(define (get-wave-type)
  (cond ((equal? (radio-check wave-box) "Sine Wave")     sine-wave)
        ((equal? (radio-check wave-box) "Sawtooth Wave") sawtooth-wave)
        ((equal? (radio-check wave-box) "Square Wave")   square-wave)
        ((equal? (radio-check wave-box) "Pulse Wave")    pulse-wave)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define getPulse
  (lambda ()
    
    
  (send pulseSlide get-value))
  
  )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;HOME BASE
(define (my-play relFreq vol time)

(let ([freq   (* octave relFreq)])  
  
  
  (if (and (< freq 1721)(< 109 freq))
      (begin
        (if (<= numOfSounds 4)
            (begin
              (set! numOfSounds (+ 1 numOfSounds))
              (addSound freq)
              (if (equal? (radio-check wave-box) "Pulse Wave") 
                  (begin
                    (let ([signal (network ()
                                     [a <= pulse-wave (/ (getPulse) 100) freq]
                                     [out = (* .1 vol a)])]) 
                    (signal-play signal)))
              (begin    
                  (cond((equal? (radio-check wave-box) "Sine Wave")(set! vol (* vol 5))))
                  (let ([signal (network ()
                                     [a <= (get-wave-type) freq]
                                     [out = (* .1 vol a)])]) 
                  (signal-play signal)))))
            (begin 
              (newline)
              (display "Too many sounds!"))))
(display "Frequency out of range, must be between 109 and 1720 Hz.")))
  
  
  
  
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
   

(define filler "                                                                     ")

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define soundList '())
; Needs to list sounds

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
           ((= numOfSounds 6)(send currSound6 set-label (getSoundString freq))))))


(define (getSoundString freq)
  (string-append "Now playing a " (radio-check wave-box) " at " (number->string freq) " hertz."))
  
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 (define (myStop)
   (begin 
    (send currSound1 set-label filler)
    (send currSound2 set-label filler)
    (send currSound3 set-label filler)
    (send currSound4 set-label filler)
    (send currSound5 set-label filler)
    (send currSound6 set-label filler)
     
     (set! soundList '())
     (set! numOfSounds 0)
     (stop)))

 

 


