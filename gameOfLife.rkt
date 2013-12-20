#lang racket
(define (make-cell x y s) (list x y s))
(define (same-position cell1 cell2) (and (= (car cell1) (car cell2)) (= (cadr cell1) (cadr cell2))))

(define state caddr)
(define (alive? cell) (equal? 'alive (caddr cell)))
(define (dead? cell) (equal? 'dead (caddr cell)))
(define row car)
(define column cadr)
(define make-board list)

(define (are-neighbours cell1 cell2)
  (and 
   (and (< (abs (- (row cell1) (row cell2))) 2) (< (abs (- (column cell1) (column cell2))) 2))
   (not (and (= (row cell1) (row cell2)) (= (column cell1) (column cell2)) ))
   )
  )

(define (neighbours cell board)
  (define (neighbours-iter cell board acc)
    (cond 
      [(null? board) acc]
      [else
       (cond 
         [(are-neighbours cell (car board)) (neighbours-iter cell (cdr board) (cons (car board) acc))]
         [else  (neighbours-iter cell (cdr board) acc)])]
      )
    )
  (neighbours-iter cell board '())
  )


(define (neighbours-alive cell board)
  (define (neighbours-alive-iter cell board acc)
    (cond [(null? board) acc]
          [else
           (cond 
             [(and (are-neighbours cell (car board)) (alive? (car board))) (neighbours-alive-iter cell (cdr board) (cons (car board) acc))]
             [else  (neighbours-alive-iter cell (cdr board) acc)]
             )
           ]
          )
    )
  (neighbours-alive-iter cell board '())
  )

(define (num-neighbours-alive cell board)
  (length (neighbours-alive cell board))
  )

(define (next-generation board)  
  (define (next-generation-iter examined-cells board iter)
    (cond 
      [(null? examined-cells) iter]
      [else 
       (define cell (car examined-cells))   
       (define num-alive-neighbors (num-neighbours-alive cell board))
       (cond 
         [(alive? cell)                                                
          (cond                                                            
            [(= 2 num-alive-neighbors) (next-generation-iter (cdr examined-cells) board (append iter (list (make-cell (row cell) (column cell) 'alive))))]
            [(= 3 num-alive-neighbors) (next-generation-iter (cdr examined-cells) board (append iter (list (make-cell (row cell) (column cell) 'alive))))]
            [else (next-generation-iter (cdr examined-cells) board (append iter (list (make-cell (row cell) (column cell) 'dead))))]                               
            )]
         [(dead? cell)
          (cond                                                            
            [(= 3 num-alive-neighbors) (next-generation-iter (cdr examined-cells) board (append iter (list (make-cell (row cell) (column cell) 'alive))))]
            [else (next-generation-iter (cdr examined-cells) board (append iter (list (make-cell (row cell) (column cell) 'dead))))] 
            )
          ]
         )
       ]
      )
    )
  (next-generation-iter board board '())
  )




(require rackunit)

(define blink-start (list (make-cell 0 0 'dead) (make-cell 0 1 'alive) (make-cell 0 2 'dead)
                          (make-cell 1 0 'dead) (make-cell 1 1 'alive) (make-cell 1 2 'dead)
                          (make-cell 2 0 'dead) (make-cell 2 1 'alive) (make-cell 2 2 'dead)))


(define blink-end (list (make-cell 0 0 'dead) (make-cell 0 1 'dead) (make-cell 0 2 'dead)
                        (make-cell 1 0 'alive) (make-cell 1 1 'alive) (make-cell 1 2 'alive)
                        (make-cell 2 0 'dead) (make-cell 2 1 'dead) (make-cell 2 2 'dead))) 


(define tests
  
  (test-suite
   "Sample tests for Assignment 5"
   (check-equal? 1 1)
   
   (check-equal?  (make-cell 1 2 3) (make-cell 1 2 3))
   
   (check-equal? #t (same-position (make-cell 0 0 'alive) (make-cell 0 0 'alive)))
   
   (check-equal? #t (are-neighbours (make-cell 0 0 'alive) (make-cell 0 1 'dead)))
   
   (check-equal? #t (are-neighbours (make-cell 0 0 'alive) (make-cell 1 1 'dead)))
   
   (check-equal? #f (are-neighbours (make-cell 0 0 'alive) (make-cell 1 2 'dead)))
   
   ;; cell is not neighbors with itself
   (check-equal? #f (are-neighbours (make-cell 0 0 'alive) (make-cell 0 0 'alive)))
   
   ;; filter negihbors of a cell against a list of empty cells
   (check-equal? '() (neighbours (make-cell 0 0 'alive) '()))
   
   ;; filter negihbors of a cell against a list of empty cells
   (check-equal?   (neighbours (make-cell 1 0 'alive) (list(make-cell 0 0 'alive)) ) (list(make-cell 0 0 'alive)) )
   
   
   ;; filter negihbors of a cell against a list of empty cells
   (check-equal?   (neighbours (make-cell 1 0 'alive) (make-board (make-cell 0 0 'alive) (make-cell 0 1 'alive) )) (list(make-cell 0 1 'alive) (make-cell 0 0 'alive)))
   
   (check-equal?   (neighbours (make-cell 1 0 'alive) (make-board (make-cell 0 0 'alive) (make-cell 0 1 'alive) (make-cell 0 3 'dead)) ) (list(make-cell 0 1 'alive) (make-cell 0 0 'alive)))
   
   (check-equal?   (neighbours-alive (make-cell 1 0 'alive) (list (make-cell 0 0 'dead) (make-cell 0 1 'alive) (make-cell 0 3 'dead))) (list(make-cell 0 1 'alive)))
   
   (check-equal?   (neighbours-alive (make-cell 1 0 'alive) (list (make-cell 0 0 'dead) (make-cell 0 1 'alive) (make-cell 0 3 'dead) (make-cell 1 1 'alive))) (list (make-cell 1 1 'alive) (make-cell 0 1 'alive) ))
   
   
   ;; next generation of an empty board is empty board
   (check-equal?  (next-generation '()) '())
   
   ;; next generation of a one cell alive board is board with dead cell
   (check-equal?  (next-generation (make-board (make-cell 0 0 'alive))) (make-board (make-cell 0 0 'dead)))
   
   ;; next genration of a two cells board all alive end up in a two dead
   
   (check-equal?  (next-generation (make-board (make-cell 0 0 'alive) (make-cell 0 1 'alive)))   (make-board (make-cell 0 0 'dead) (make-cell 0 1 'dead)))
   
   ;; next generation of a three dead cell board
   (check-equal?  (next-generation (make-board (make-cell 0 0 'dead) (make-cell 0 1 'dead) (make-cell 0 2 'dead))) (make-board (make-cell 0 0 'dead) (make-cell 0 1 'dead) (make-cell 0 2 'dead)))
   
   ;; next generation of three alive cells
   ;   (check-equal? (num-neighbours-alive (make-cell 0 1 'alive)  (make-board (make-cell 0 0 'alive) (make-cell 0 1 'alive) (make-cell 0 2 'alive)) ) 2)
   
   (check-equal? (num-neighbours-alive (make-cell 0 1 'alive)  (make-board (make-cell 0 0 'alive) (make-cell 0 1 'alive) (make-cell 0 2 'alive)) ) 2)
   
   
   (check-equal?  (next-generation (make-board (make-cell 0 0 'alive) (make-cell 0 1 'alive) (make-cell 0 2 'alive))) (make-board (make-cell 0 0 'dead) (make-cell 0 1 'alive) (make-cell 0 2 'dead)))
   
   ;; check the neighbours alive of the blinker start configuration
   (check-equal? (num-neighbours-alive (make-cell 1 0 'dead) blink-start) 3)
   
   (check-equal? (neighbours-alive (make-cell 1 0 'dead) blink-start) (list (make-cell 2 1 'alive) (make-cell 1 1 'alive) (make-cell 0 1 'alive))) 
      
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
