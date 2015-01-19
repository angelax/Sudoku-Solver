;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname sudoku) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
;;
;; ***************************************************
;;  Yi Xu (20576746)
;;  CS 135 Fall 2014
;;  Assignment 10, Problem 3 (sudoku)
;; ***************************************************
;;

;; The following line is REQUIRED (do not remove)
(require "a10lib.rkt")

;; Place your Personal Identification here


;; NOTE: Do NOT leave top-level expressions in your code.
;;       In other words, when your code is run, only the
;;       check-expect message "All X tests passed!"
;;       should appear in the interactions window

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A SudokuDigit is one of:
;; * '?
;; * 1 <= Nat <= 9

;; A Puzzle is a (listof (listof SudokuDigit))
;; requires: the list and all sublists have a length of 9

;; A Solution is a Puzzle
;; requires: none of the SudokuDigits are '?
;;           the puzzle satisfies the number placement 
;;             rules of sudoku

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Here are some sample sudoku puzzles

;; From the basic test shown in the assignment:
(define veryeasy
  '((? 4 5 8 9 3 7 1 6)
    (8 1 3 5 7 6 9 2 4)
    (7 6 9 2 1 4 5 3 8)
    (5 3 6 9 8 7 1 4 2)
    (4 9 2 1 6 5 8 7 3)
    (1 7 8 4 3 2 6 5 9)
    (6 8 4 7 2 1 3 9 5)
    (3 2 1 6 5 9 4 8 7)
    (9 5 7 3 4 8 2 6 1)))

;; the above puzzle with more blanks:
(define easy
  '((? 4 5 8 ? 3 7 1 ?)
    (8 1 ? ? ? ? ? 2 4)
    (7 ? 9 ? ? ? 5 ? 8)
    (? ? ? 9 ? 7 ? ? ?)
    (? ? ? ? 6 ? ? ? ?)
    (? ? ? 4 ? 2 ? ? ?)
    (6 ? 4 ? ? ? 3 ? 5)
    (3 2 ? ? ? ? ? 8 7)
    (? 5 7 3 ? 8 2 6 ?)))

;; the puzzle listed on wikipedia
(define wikipedia '((5 3 ? ? 7 ? ? ? ?)
                    (6 ? ? 1 9 5 ? ? ?)
                    (? 9 8 ? ? ? ? 6 ?)
                    (8 ? ? ? 6 ? ? ? 3)
                    (4 ? ? 8 ? 3 ? ? 1)
                    (7 ? ? ? 2 ? ? ? 6)
                    (? 6 ? ? ? ? 2 8 ?)
                    (? ? ? 4 1 9 ? ? 5)
                    (? ? ? ? 8 ? ? 7 9)))

;; A blank puzzle template for you to use:
(define blank '((? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)
                (? ? ? ? ? ? ? ? ?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; HAVE FUN, good luck with your final exams, and have a Merry Christmas!
;; -- with love, the CS 135 team


;; make-solved: produces a function that consumes a puzzle and checks if
;; the puzzle is a Solution 
(define make-solved (lambda (puzzle)
  (cond [(empty? (foldr (lambda (x y) 
                          (cond [(member? '? x) (cons x y)]
                                [else y])) empty puzzle)) puzzle]
        [else false])))


;; (neighbours puzzle) consumes a puzzle and produces a list with the 
;; possible puzzles where the first ? in the puzzle in filled
;; neighbours: Puzzle -> (listof Puzzle)
;; Examples:
(check-expect (neighbours veryeasy) 
              '(((2 4 5 8 9 3 7 1 6)
                 (8 1 3 5 7 6 9 2 4)
                 (7 6 9 2 1 4 5 3 8)
                 (5 3 6 9 8 7 1 4 2)
                 (4 9 2 1 6 5 8 7 3)
                 (1 7 8 4 3 2 6 5 9)
                 (6 8 4 7 2 1 3 9 5)
                 (3 2 1 6 5 9 4 8 7)
                 (9 5 7 3 4 8 2 6 1))))
(check-expect (neighbours easy) (list
                                 (list
                                  (list 2 4 5 8 '? 3 7 1 '?)
                                  (list 8 1 '? '? '? '? '? 2 4)
                                  (list 7 '? 9 '? '? '? 5 '? 8)
                                  (list '? '? '? 9 '? 7 '? '? '?)
                                  (list '? '? '? '? 6 '? '? '? '?)
                                  (list '? '? '? 4 '? 2 '? '? '?)
                                  (list 6 '? 4 '? '? '? 3 '? 5)
                                  (list 3 2 '? '? '? '? '? 8 7)
                                  (list '? 5 7 3 '? 8 2 6 '?))))

(define (neighbours puzzle)
  (local 
    [(define digits '(1 2 3 4 5 6 7 8 9))
     
     ;; (find-blank puzzle row) consumes a puzzle and a row and
     ;; goes through the rows in the puzzle to find the first '?
     ;; find-blank: Puzzle Nat -> Puzzle (listof SudokuDigit))
     (define (find-blank puzzle row)
       (local [(define col (find-col (first puzzle) 1))]
         (cond [(integer? col) (list (list row col) (first puzzle))]
               [else (find-blank (rest puzzle) (add1 row))])))
     
     ;; (find-col digits-lst col) consumes a digits-lst and a col and
     ;; goes through digits-lst to find the col the first '? is in
     ;; find-col: (listof SudokuDigit) Nat -> SudokuDigit
     (define (find-col digits-lst col)
       (cond [(empty? digits-lst) empty]
             [(equal? (first digits-lst) '?) col]
             [else (find-col (rest digits-lst) (add1 col))]))
     
     (define blank-lst (find-blank puzzle 1))
     
     ;; (get-col puzzle col) consumes a puzzle and a col and goes through
     ;; the puzzle to produce a list of SudokuDigit in the col
     ;; get-col: Puzzle Nat -> SudokuDigit
     (define (get-col puzzle col)
       (cond [(empty? puzzle) empty]
             [else (cons (find-values (first puzzle) col 1) 
                         (get-col (rest puzzle) col))]))
     
     ;; (find-values digits-lst col current-col) consumes a digits-lst, a col
     ;; and the current-col to produce the SudokuDigit in the col
     ;; find-values: 
     ;; (listof SudokuDigit) Nat Nat -> SudokuDigit
     (define (find-values digits-lst col current-col)
       (cond [(= col current-col) (first digits-lst)]
             [else (find-values 
                    (rest digits-lst) col (add1 current-col))]))
     
     ;; (get-box puzzle row-set col-set row) consumes puzzle, row-set, col-set
     ;; and row to produce the SudoduDigits in the box of row-set and col-set
     ;; get-box: Puzzle Nat Nat Nat -> (listof SudokuDigit)
     (define (get-box puzzle row-set col-set row)
       (cond [(= row (* row-set 3)) 
              (find-col-values (first puzzle) col-set 1)]
             [(and (< row (* row-set 3))
                   (>= row (- (* row-set 3) 2)))
              (append (find-col-values (first puzzle) col-set 1) 
                      (get-box (rest puzzle) row-set col-set (add1 row)))]
             [else (get-box (rest puzzle) row-set col-set (add1 row))]))
     
     ;; (find-col-values digit-lst col-set col) consumes digit-lst, col-set
     ;; and col to produce the SudoduDigits in the box of col-set
     ;; find-col-values: (listof SudokuDigit) Nat Nat -> (listof SudokuDigit)
     (define (find-col-values digit-lst col-set col)
       (cond [(= col (* col-set 3)) (cons (first digit-lst) empty)]
             [(and (< col (* col-set 3))
                   (>= col (- (* col-set 3) 2)))
              (cons (first digit-lst) 
                    (find-col-values (rest digit-lst) col-set (add1 col)))] 
             [else (find-col-values (rest digit-lst) col-set (add1 col))]))
     
     ;; (possible-values position row) consumes a position and row to produce
     ;; a list of possible SudoduDigits for the first '? in the puzzle
     ;; possible-values: 
     ;; (list Nat Nat) (listof SudokuDigit) -> (listof SudokuDigit) 
     (define (possible-values position row)
       (local [(define col (get-col puzzle (second position)))
               (define box (get-box puzzle (quotient (+ (first position) 2) 3)
                                    (quotient (+ (second position) 2) 3) 1))]       
         (filter (lambda (x) (and (not (member? x row))
                                  (not (member? x col))
                                  (not (member? x box)))) digits)))
     
     ;; (?-row puzzle position row value) consumes a puzzle, position, row, 
     ;; value to produce a puzzle with the first '? replaced by value
     ;; ?-row: Puzzle (list Nat Nat) Nat SudokuDigit -> Puzzle 
     (define (?-row puzzle position row value)
       (cond [(= (first position) row) 
              (cons (?-col (first puzzle) (second position) 1 value)
                    (rest puzzle))]
             [else (cons (first puzzle) 
                         (?-row (rest puzzle) position (add1 row) value))]))
     
     ;; (?-col digits-lst col-position col value) 
     ;; consumes a digits-lst, col-position, col, value to produce a  
     ;; list of SudokuDigit with the first '? replaced by value
     ;; ?-col: (list SudokuDigit) Nat Nat SudokuDigit -> (list SudokuDigit)
     (define (?-col digits-lst col-position col value)
       (cond [(= col-position col) 
              (cons value (rest digits-lst))]
             [else (cons (first digits-lst) 
                         (?-col (rest digits-lst) col-position 
                                (add1 col) value))]))
     
     ;; (replace-digit position value) consumes a position and value to produce
     ;; a list of puzzles with the first '? replaced by an element in value
     ;; ?-row: Puzzle (list Nat Nat) Nat SudokuDigit -> Puzzle 
     (define (replace-digit position values)
       (cond [(empty? values) empty]
             [else (cons (?-row puzzle position 1 (first values))
                         (replace-digit position (rest values)))]))]
    
    (replace-digit (first blank-lst)
                   (possible-values (first blank-lst) (second blank-lst)))))

;; Tests
(check-expect (neighbours wikipedia)
              (list
               (list
                (list 5 3 1 '? 7 '? '? '? '?)
                (list 6 '? '? 1 9 5 '? '? '?)
                (list '? 9 8 '? '? '? '? 6 '?)
                (list 8 '? '? '? 6 '? '? '? 3)
                (list 4 '? '? 8 '? 3 '? '? 1)
                (list 7 '? '? '? 2 '? '? '? 6)
                (list '? 6 '? '? '? '? 2 8 '?)
                (list '? '? '? 4 1 9 '? '? 5)
                (list '? '? '? '? 8 '? '? 7 9))
               (list
                (list 5 3 2 '? 7 '? '? '? '?)
                (list 6 '? '? 1 9 5 '? '? '?)
                (list '? 9 8 '? '? '? '? 6 '?)
                (list 8 '? '? '? 6 '? '? '? 3)
                (list 4 '? '? 8 '? 3 '? '? 1)
                (list 7 '? '? '? 2 '? '? '? 6)
                (list '? 6 '? '? '? '? 2 8 '?)
                (list '? '? '? 4 1 9 '? '? 5)
                (list '? '? '? '? 8 '? '? 7 9))
               (list
                (list 5 3 4 '? 7 '? '? '? '?)
                (list 6 '? '? 1 9 5 '? '? '?)
                (list '? 9 8 '? '? '? '? 6 '?)
                (list 8 '? '? '? 6 '? '? '? 3)
                (list 4 '? '? 8 '? 3 '? '? 1)
                (list 7 '? '? '? 2 '? '? '? 6)
                (list '? 6 '? '? '? '? 2 8 '?)
                (list '? '? '? 4 1 9 '? '? 5)
                (list '? '? '? '? 8 '? '? 7 9))))
(check-expect (neighbours blank)
              (list
               (list
                (list 1 '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?))
               (list
                (list 2 '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?))
               (list
                (list 3 '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?))
               (list
                (list 4 '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?))
               (list
                (list 5 '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?))
               (list
                (list 6 '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?))
               (list
                (list 7 '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?))
               (list
                (list 8 '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?))
               (list
                (list 9 '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?)
                (list '? '? '? '? '? '? '? '? '?))))


;; make-neighbours: produces a function that consumes a puzzle and passes it
;; into the neighbours function to get a list of possible neighbour puzzles 
(define make-neighbours (lambda (puzzle) (neighbours puzzle)))


;; (sudoku puzzle) consumes a puzzle and produces the completed 
;; sudoku puzzle
;; sudoku: Puzzle -> Puzzle
;; Examples:
(check-expect (sudoku veryeasy)
              '((2 4 5 8 9 3 7 1 6)
                (8 1 3 5 7 6 9 2 4)
                (7 6 9 2 1 4 5 3 8)
                (5 3 6 9 8 7 1 4 2)
                (4 9 2 1 6 5 8 7 3)
                (1 7 8 4 3 2 6 5 9)
                (6 8 4 7 2 1 3 9 5)
                (3 2 1 6 5 9 4 8 7)
                (9 5 7 3 4 8 2 6 1)))
(check-expect (sudoku easy)
              '((2 4 5 8 9 3 7 1 6)
                (8 1 3 5 7 6 9 2 4)
                (7 6 9 2 1 4 5 3 8)
                (5 3 6 9 8 7 1 4 2)
                (4 9 2 1 6 5 8 7 3)
                (1 7 8 4 3 2 6 5 9)
                (6 8 4 7 2 1 3 9 5)
                (3 2 1 6 5 9 4 8 7)
                (9 5 7 3 4 8 2 6 1)))
              
(define (sudoku puzzle)
  (find-final puzzle make-neighbours make-solved))

;; Tests
(check-expect (sudoku wikipedia)
              (list
               (list 5 3 4 6 7 8 9 1 2)
               (list 6 7 2 1 9 5 3 4 8)
               (list 1 9 8 3 4 2 5 6 7)
               (list 8 5 9 7 6 1 4 2 3)
               (list 4 2 6 8 5 3 7 9 1)
               (list 7 1 3 9 2 4 8 5 6)
               (list 9 6 1 5 3 7 2 8 4)
               (list 2 8 7 4 1 9 6 3 5)
               (list 3 4 5 2 8 6 1 7 9)))
(check-expect (sudoku blank)
              (list
               (list 1 2 3 4 5 6 7 8 9)
               (list 4 5 6 7 8 9 1 2 3)
               (list 7 8 9 1 2 3 4 5 6)
               (list 2 1 4 3 6 5 8 9 7)
               (list 3 6 5 8 9 7 2 1 4)
               (list 8 9 7 2 1 4 3 6 5)
               (list 5 3 1 6 4 2 9 7 8)
               (list 6 4 2 9 7 8 5 3 1)
               (list 9 7 8 5 3 1 6 4 2)))
              

