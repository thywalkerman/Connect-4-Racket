#lang racket
;; Connect 4 Game

;===========================;;
; Personal Helper Functions ;;
;===========================;;

;============================================;;
;  helper functions that work with matrices  ;;
;  along with some general purpose functions ;;
;============================================;;
;; return item at index of list
(define (JRHget-item lst index)
  (if (null? lst)
    `()
    (if (= index 1)
      (car lst)
      (JRHget-item (cdr lst) (- index 1))
    )
  )
)

;; count number of elements equal to a given number
(define (JRHcount-equal-to lst num)
  (if (null? lst)
    0
    (if (= (car lst) num)
      (+ 1 (JRHcount-equal-to (cdr lst) num))
      (+ 0 (JRHcount-equal-to (cdr lst) num))
    )
  )
)

;; edit cell from row-list with given item
(define (JRHedit-cell row-list col item)
  (if (null? row-list)
    `()
    (if (= col 1)
      (cons item (cdr row-list))
      (cons (car row-list)
        (JRHedit-cell (cdr row-list) (- col 1) item)
      )
    )
  )
)

;; return an item from given cell in matrix
(define (JRHget-cell matrix row col)
  (JRHget-item (JRHget-item matrix row) col)
)

;; return a new matrix with the given grid location with new given item
(define (JRHset-cell matrix row col item)
  (if (null? matrix)
    `()
    (if (= row 1)
      (cons (JRHedit-cell (car matrix) col item)
        (cdr matrix)
      )
      (cons (car matrix)
        (JRHset-cell (cdr matrix) (- row 1) col item)
      )
    )
  )
)

; CONNECT 4 GAME FUNCTIONS ;;
;==========================;;
;  these helper functions are geared towards the process of playing connect 4 ;;
;=============================================================================;;
;; return the game board
(define (JRHget-game-board)
  (cdr JRHGame)
)

;; return given move and update board state
(define (JRHupdate-game-board move)
  (set! JRHGame (cons (JRHnext-player)
		      (JRHfind-next-empty-row (JRHget-game-board) move (car JRHGame)))
  )
  move
)

;; find the next empty row from given column in board and place marker
(define (JRHfind-next-empty-row board col marker)
  (if (null? board)
    `()
    (if (> (JRHget-item (car board) col) 0)
      (cons (car board) (JRHfind-next-empty-row (cdr board) col marker))
      (cons (JRHedit-cell (car board) col marker) (cdr board))
    )
  )
)

;; get row index from last move
(define (JRHget-row-of-last-move col)
  (JRHget-row-of-last-move-helper (JRHget-game-board) 1 col)
)

(define (JRHget-row-of-last-move-helper board row col)
  (if (or (null? board) (= (JRHget-item (car board) col) 0))
    (- row 1)
    (JRHget-row-of-last-move-helper (cdr board) (+ row 1) col)
  )
)

;; determine which player goes next
(define (JRHnext-player)
  (if (= (car JRHGame) 1)
    2
    1
  )
)

; GROUPING FUNCTIONS ;;
; Helps group together pieces in order to check for win conditions          ;;
; All groupings (except vertical grouping) will assume given move (col)     ;;
; is the beginning index of a 4-in-a-row                                    ;;
; (i.e col=4, grouping={col=4, col=5, col=6, col=7}                         ;;
;===========================================================================;;
;; group a section to check for vertical win
;; row: n-1  col: n
(define (JRHgroup-vertical col)
  (JRHgroup-vertical-accumulate '() (JRHget-row-of-last-move col) col 4)
)

(define (JRHgroup-vertical-accumulate group row col group-of)
  (if (or (null? (JRHget-cell (JRHget-game-board) row col))
          (= group-of 0))
    group
    (JRHgroup-vertical-accumulate (cons (JRHget-cell (JRHget-game-board) row col) group)
                               (- row 1) col (- group-of 1))
  )
)

;; group a section to check for horizontal win
;; row: n   col: n+1
(define (JRHgroup-horizontal row col)
  (JRHgroup-horizontal-accumulate `() row col 4)
)

(define (JRHgroup-horizontal-accumulate group row col group-of)
  (if (or (null? (JRHget-cell (JRHget-game-board) row col))
          (= group-of 0))
    group
    (JRHgroup-horizontal-accumulate (cons (JRHget-cell (JRHget-game-board) row col) group)
                                  row (+ col 1) (- group-of 1))
  )
)

;; group a section to check for diagonal left win
;; row: n-1   col: n+1
(define (JRHgroup-diagonal-left row col)
  (JRHgroup-diagonal-left-accumulate `() row col 4)
)

(define(JRHgroup-diagonal-left-accumulate group row col group-of)
  (if (or (null? (JRHget-cell (JRHget-game-board) row col))
          (= group-of 0))
    group
    (JRHgroup-diagonal-left-accumulate (cons (JRHget-cell (JRHget-game-board) row col) group)
                                 (- row 1) (+ col 1) (- group-of 1))
  )
)

;; group a section to check for diagonal right win
;; row: n+1   col: n+1
(define (JRHgroup-diagonal-right row col)
  (JRHgroup-diagonal-right-accumulate `() row col 4)
)

(define(JRHgroup-diagonal-right-accumulate group row col group-of)
  (if (or (null? (JRHget-cell (JRHget-game-board) row col))
          (= group-of 0))
    group
    (JRHgroup-diagonal-right-accumulate (cons (JRHget-cell (JRHget-game-board) row col) group)
                                 (+ row 1) (+ col 1) (- group-of 1))
  )
)

; WINNING CONDITIONS ;;
; Check for specific combinations of wins
; All win conditions (except vertical win) will assume that the given move (col)  
; is the beginning index of a 4-in-a-row.
;
; If this fails, we see if the given move (col) is used as an index for any
; 4-in-a-row by shifting left and checking for its respective win.
; If a win has yet to be found after 3 shifts, then the win condition will fail
;================================================================================;;
;; check to see if a group is 4-in-a-row
(define (JRHcheck-win group col)
  (if (= (JRHcount-equal-to group (JRHnext-player)) 4)
    #t
    #f
  )
)

;; return true if vertical win
(define (JRHvertical-win? col)
  (if (JRHcheck-win (JRHgroup-vertical col) col)
    #t
    #f
  )
)

;; return true if horizontal win
(define (JRHhorizontal-win? row col checks)
  (if (and (> checks 0) (JRHcheck-win (JRHgroup-horizontal row col) col))
    #t
    (if (null? (JRHget-cell (JRHget-game-board) row (- col 1)))
      #f
      (if (= (JRHget-cell (JRHget-game-board) row (- col 1)) (JRHnext-player))
        (JRHhorizontal-win? row (- col 1) (- checks 1))
        #f
      )
    )
  )
)

;; return true if diagonal left win
(define (JRHdiagonal-left-win? row col checks)
  (if (and (> checks 0) (JRHcheck-win (JRHgroup-diagonal-left row col) col))
    #t
    (if (null? (JRHget-cell (JRHget-game-board) (+ row 1) (- col 1)))
      #f
      (if (= (JRHget-cell (JRHget-game-board) (+ row 1) (- col 1)) (JRHnext-player))
        (JRHdiagonal-left-win? (+ row 1) (- col 1) (- checks 1))
        #f
      )
    )
  )
)

;; return true if diagonal right win
(define (JRHdiagonal-right-win? row col checks)
  (if (and (> checks 0) (JRHcheck-win (JRHgroup-diagonal-right row col) col))
    #t
    (if (null? (JRHget-cell (JRHget-game-board) (- row 1) (- col 1)))
      #f
      (if (= (JRHget-cell (JRHget-game-board) (- row 1) (- col 1)) (JRHnext-player))
        (JRHdiagonal-right-win? (- row 1) (- col 1) (- checks 1))
        #f
      )
    )
  )
)

;======================;;
;  Required Functions  ;;
;======================;;
;; global game state
(define JRHGame 0)

;; initialize game and display a message
(define (JRHStartGame)
  (begin
    (set! JRHGame `(1 (0 0 0 0 0 0 0)
                      (0 0 0 0 0 0 0)
		      (0 0 0 0 0 0 0)
		      (0 0 0 0 0 0 0)
		      (0 0 0 0 0 0 0)
		      (0 0 0 0 0 0 0))
    )
    (display "I'm finally finished with the assignment...\n")
    #t
  )
)

;; mark move and return a given column by player
(define (JRHMarkMove col)
  (if (JRHLegalMoveP col)
    (JRHupdate-game-board col)
    ; if given illegal move, randomly generate a legal move
    (JRHMarkMove (random 8))
  )
)

;; display the current state of the game and returns #t
(define (JRHShowGame)
  (begin
    (display (car JRHGame))
    (newline)
    (display (car (cdr (cdr (cdr (cdr (cdr (JRHget-game-board))))))))
    (newline)
    (display (car (cdr (cdr (cdr (cdr (JRHget-game-board)))))))
    (newline)
    (display (car (cdr (cdr (cdr (JRHget-game-board))))))
    (newline)
    (display (car (cdr (cdr (JRHget-game-board)))))
    (newline)
    (display (car (cdr (JRHget-game-board))))
    (newline)
    (display (car (JRHget-game-board)))
    (newline)
  )
  #t
)

;; make move chosen by computer and returns chosen column for next move
(define (JRHMakeMove)
  (random 8)
)

;; determine if given move (column) is legal
;; return #t or #f
(define (JRHLegalMoveP move)
  ;; illegal move if column is full
  (if (and (not (= 0 move))
           (= (JRHget-item (JRHget-item (JRHget-game-board) 6) move) 0))
    #t
    #f
  )
)

;; determine if the last move (column) given results in a win
;; returns #t or #f
(define (JRHWinP move)
  (if (JRHvertical-win? move)
    #t
    (if (JRHhorizontal-win? (JRHget-row-of-last-move move) move 4)
      #t
      (if (JRHdiagonal-left-win? (JRHget-row-of-last-move move) move 4)
        #t
        (if (JRHdiagonal-right-win? (JRHget-row-of-last-move move) move 4)
          #t
          #f
        )
      )
    )
  )
)

;; determine if the given move (column) will result in a win
;; return #t or #f
(define (JRHWillWinP move)
  (if (JRHWinP (JRHMarkMove move))
    #t
    #f
  )
  ; reset board state to previous state
  (set! JRHGame (cons (JRHnext-player) (JRHset-cell (JRHget-game-board) (JRHget-row-of-last-move move) move 0)))
)