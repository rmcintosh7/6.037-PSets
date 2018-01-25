#lang racket

;Rachel McIntosh
;Project 2, due 1/18

;; By default, Racket doesn't have set-car! and set-cdr! functions.  The
;; following line allows us to use those:
(require r5rs)
;; Unfortunately, it does this by making cons return a "mutable pair"
;; instead of a "pair", which means that many built-ins may fail
;; mysteriously because they expect a pair, but get a mutable pair.
;; Re-define a few common functions in terms of car and friends, which
;; the above line make work with mutable pairs.
(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
;; We also tell DrRacket to print mutable pairs using the compact syntax
;; for ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)


;Problem 1: create an abstraction for a simple key-value table

;Constructor; tag as table and add space for front and rear pointers
(define (make-table)
  (cons 'table (cons '() '())))

;Check that value is tagged as a table; check for pair first to prevent errors
(define (table? table)
  (and (pair? table) (eq? 'table (car table))))

;Set the element after 'table as the front pointer
(define (front-ptr table)
  (cadr table))

;Set the last element as the rear pointer
(define (rear-ptr table)
  (cddr table))

;Make the front pointer point to the new item passed in
(define (set-front-ptr! table item)
  (set-car! (cdr table) item))

;Make the rear pointer point to the new item passed in
(define (set-rear-ptr! table item)
  (set-cdr! (cdr table) item))

;Add a new key-value pairing to the table by setting it as both the front and rear pointer if the table is empty,
;or by tacking the pairing on to the end of the table and setting it as the rear pointer
(define (table-put! table key value)
  (if (table? table)
      (let ((new-pair (cons (list key value) '())))
        (cond ((null? (front-ptr table)) (set-front-ptr! table new-pair)
              (set-rear-ptr! table new-pair))
              (else (set-cdr! (rear-ptr table) new-pair)
                    (set-rear-ptr! table new-pair))))
      (error "Object not a table:" table)))

;Send the associative list section of the table to the helper function if a table is passed in;
;using car cdr on table before passing it ensures that only the list of key-value pairs (and not the table tagging) is passed so as to prevent errors
(define (table-has-key? table key)
  (if (table? table)
      (has-key-helper (car (cdr table)) key)
      (error "Object not a table:" table)))

;Works similarly to assoc, but was written because errors were given when previously attempting to call assoc
;Iterates through a list of key-value pairs, returning true if any of the keys are found or false upon reaching the end of the list
(define (has-key-helper table key)
  (if (null? table)
      #f
      (if (equal? key (car (car table)))
          #t
          (has-key-helper (cdr table) key))))

;Send the associative list section of the table to the helper function if a table is passed in and this table contains the key
;using car cdr on table before passing it ensures that only the list of key-value pairs (and not the table tagging) is passed so as to prevent errors
(define (table-get table key)
  (if (table-has-key? table key)
      (table-get-helper (car (cdr table)) key)
      (error "Key not in table:" table)))

;Works similarly to assoc, but was written because errors were given when previously attempting to call assoc
;Iterates through a list of key-value pairs, returning the appropriate value when the key is found
;Uses the assumption that table-get has checked that the table contains the key
(define (table-get-helper table key)
   (if (equal? key (car (car table)))
          (cdr (car table))
          (table-get-helper (cdr table) key)))

;Interactions pasted below:
;> (define my-table (make-table))
;> (table? my-table)
;#t
;> (table-put! my-table 'ben-bitdiddle 'chocolate)
;> (table-put! my-table 'alyssa-p-hacker 'cake)
;> (table-has-key? my-table 'ben-bitdiddle)
;#t
;> (table-has-key? my-table 'louis-reasoner)
;#f
;> (table-get my-table 'ben-bitdiddle)
;(chocolate)
;> (table-get my-table 'louis-reasoner)
;key not in table: (table ((ben-bitdiddle chocolate) (alyssa-p-hacker cake)) (alyssa-p-hacker cake))
;Note: implementation also passed formal test suite

;Problem 2: Create the procedure make-monitored, which returns a procedure and tracks the number of calls to fib
;The counter can also be reset

;given code for fib
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

;Takes in a procedure func and drops a frame to store num-calls using 'let', which allows for the tracking feature.
;Cheks for special symbols passed in and proceeds with their indicated action if found (ie. returns the number of calls or resets the counter)
;Otherwise, increments the number of calls and calls the procedure passed in
(define make-monitored
  (lambda (func)
    (let ((num-calls 0))
      (lambda (x)
          (cond
            ((equal? x 'how-many-calls?)
             num-calls)
            ((equal? x 'reset-call-count)
             (set! num-calls 0))
            (else (set! num-calls (+ num-calls 1))
                  (func x)))))))

;Interactions pasted below followed by the statements printed upon runtime                         
;(fib 8) ;; => 21
;(set! fib (make-monitored fib))
;(fib 8) ;; => 21
;(fib 'how-many-calls?) ;; => 67
;(fib 8) ;; => 21
;(fib 'how-many-calls?) ;; => 134
;(fib 'reset-call-count)
;(fib 'how-many-calls?) ;; => 0
;(fib 0) ;;0
;(fib 'how-many-calls?) ;; 1
;(fib 25) ;; big number
;(fib 'how-many-calls?) ;; an even bigger number
;(fib 'reset-call-count)
;(fib 'how-many-calls?) ;; 0
;21
;21
;67
;21
;134
;0
;0
;1
;75025
;242786
;0

;Problem 3: create a procedure that makes a table (implemented earlier) of the input (key) and number of calls (value) between inputs 1 and max

;Creates a new table and calls the helper function
(define make-num-calls-table
  (lambda (func max)
    (define my-table (make-table))
    (make-num-calls-table-helper func max 0 my-table)))

;Iterates through the numbers n to max, returning the completed table upon reaching the max
;For each iteration, the key is the current iteration and the value is the result of 'how-many-calls? after calling the function on the current iteration
;The call count is reset after adding the new pairing to the table to ensure the values do not sum over iteration (ie. (2 3) will be added, not (2 4))
(define make-num-calls-table-helper
  (lambda (func max n my-table)
    (if (equal? n max)
        my-table
        (begin
          (func (+ n 1))
          (table-put! my-table (+ n 1) (func 'how-many-calls?))
          (func 'reset-call-count)
          (make-num-calls-table-helper func max (+ n 1) my-table))))) 

;Interactions pasted below;
;(make-num-calls-table fib 10)
;(make-num-calls-table fib 20)
;(make-num-calls-table fib 30)
;statements printed upon running:
;(table ((1 1) (2 3) (3 5) (4 9) (5 15) (6 25) (7 41) (8 67) (9 109) (10 177)) (10 177))
;(table
; ((1 1)
;  (2 3)
;  (3 5)
;  (4 9)
;  (5 15)
;  (6 25)
;  (7 41)
;  (8 67)
;  (9 109)
;  (10 177)
;  (11 287)
;  (12 465)
;  (13 753)
;  (14 1219)
;  (15 1973)
;  (16 3193)
;  (17 5167)
;  (18 8361)
;  (19 13529)
;  (20 21891))
; (20 21891))
;(table
; ((1 1)
;  (2 3)
;  (3 5)
;  (4 9)
;  (5 15)
;  (6 25)
;  (7 41)
;  (8 67)
;  (9 109)
;  (10 177)
;  (11 287)
;  (12 465)
;  (13 753)
;  (14 1219)
;  (15 1973)
;  (16 3193)
;  (17 5167)
;  (18 8361)
;  (19 13529)
;  (20 21891)
;  (21 35421)
;  (22 57313)
;  (23 92735)
;  (24 150049)
;  (25 242785)
;  (26 392835)
;  (27 635621)
;  (28 1028457)
;  (29 1664079)
;  (30 2692537))
; (30 2692537))
;Note: repetition of the last call (the call to max) is due to my use of a rear pointer in my implementation of table

;Problem 4: Create a procedure that caches results of new calls to the function in a table, and retrieves the cached result if this input is used again
;on the function as opposed to recalculating the result
;Note: code currently not working; error occurs at calling (func x); error message is + contract violation, expected number, given (1)
;Will attempt to fix over the next couple of days

;Drop a table into memory using let, which remains in memory and can be referenced later
;Check this table for the key passed in, returning the cached value if found
;Otherwise, call the function originally passed in and store the result
(define memorize
  (lambda (func)
    (let ((my-table (make-table)))
      (lambda (x)
        (if (table-has-key? my-table x)
            (table-get my-table x)
            (begin
              (let ((answer (func x)))
              (table-put! my-table x answer)
              answer)))))))
              

                         

(fib 3)
(set! fib (memorize fib))
(fib 2) ;; => 21

  
  

 

