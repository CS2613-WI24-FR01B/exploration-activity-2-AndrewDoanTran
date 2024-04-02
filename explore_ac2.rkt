#lang racket/gui

; Make a frame by instantiating the frame% class
(define frame (new frame% [label "Employee salary calculator"]
                   [width 350] ; set the width of window in R
                   [height 300] ; set the height of window in R
                   ))

; This line creates a message such as the text in Java
(define welcome (new message% [parent frame]
                          [label "Welcome to program!"]))

; Create text fields for employee information
(define text-field-name (new text-field%
                        [parent frame]
                        [label "\n***Name employee: "]
                        [init-value ""]
                        [style '(single)]
                        [font normal-control-font]
                        [enabled #t]))

(define text-field-ID (new text-field%
                        [parent frame]
                        [label "\n***ID employee: "]
                        [init-value ""]
                        [style '(single)]
                        [font normal-control-font]
                        [enabled #t]))

(define text-field-position (new text-field%
                        [parent frame]
                        [label "\n***Position employee (Manager, Supervisor, Empoyee): "]
                        [init-value ""]
                        [style '(single)]
                        [font normal-control-font]
                        [enabled #t]))

(define text-field-hours (new text-field%
                        [parent frame]
                        [label "\n***Hours for the last two weeks: "]
                        [init-value ""]
                        [style '(single)]
                        [font normal-control-font]
                        [enabled #t]))

(define text-field-late-times (new text-field%
                        [parent frame]
                        [label "\n***Number of late: "]
                        [init-value ""]
                        [style '(single)]
                        [font normal-control-font]
                        [enabled #t]))

;output
(define output-label (new message% [parent frame]
                                    [label "\t\tThe result will be printed here"]
                                    [min-width 400]))

; Create a button to calculate the salary
(define calculate (new button%
                       [parent frame]
                       [label "Calculate"]
                       [callback (lambda (button event)
                                   (let* ([name (send text-field-name get-value)]
                                          [ID (send text-field-ID get-value)]
                                          [position (send text-field-position get-value)]
                                          [hours (string->number (send text-field-hours get-value))]
                                          [late-times (string->number (send text-field-late-times get-value))])
                                     (send output-label set-label (format "~a, ID: ~a has $~a for this month." name ID (number->string (salary-cal hours position late-times)))))
                                     )]))

; Create a button to clear all input fields and output label
(define clear-button (new button%
                       [parent frame]
                       [label "Clear"]
                       [callback (lambda (button event)
                                   (send text-field-name set-value "")
                                   (send text-field-ID set-value "")
                                   (send text-field-position set-value "")
                                   (send text-field-hours set-value "")
                                   (send text-field-late-times set-value "")
                                   (send output-label set-label "\t\tThe result will be printed here")
                                   )]))

; Function to calculate the salary of an employee
(define (salary-cal total-hours position late_times)
  (define punishment 0)
  (define position-salary 0)

  ; Calculate the punishment based on the number of times late
  (cond
    [(< late_times 4) (set! punishment 50)]
    [else (set! punishment 100)]
    )

  ; Determine the position salary based on the employee's position
  (cond
    ; Using string-downcase to lower case the string
    [(string=? (string-downcase position) "manager") (set! position-salary 25)]
    [(string=? (string-downcase position) "supervisor") (set! position-salary 17)]
    [else (set! position-salary 14.75)]
    )
  
  ; Calculate the salary based on the total hours and position salary
  (define salary (- (* total-hours position-salary) punishment))
  
  ; Ensure that salary is not negative
  (cond
    [(< salary 0) (set! salary 0)])
  
  salary) ; Return the calculated salary

; Show the frame by calling its show method
(send frame show #t)
