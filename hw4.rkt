
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file



;; helper function to make a stream

(define (make-stream f start) (cons start (lambda () (make-stream f (f start)))))

;; put your code below

(define (sequence low high stride) (
  if (<= low high) 
     (cons low (sequence (+ low stride) high stride))
     null
     )
)

(define (string-append-map xs suffix) 
  (map (lambda (str) (string-append str suffix)) xs)                     
)

(define (list-nth-mod xs n) (cond [(< n 0) (error "list-nth-mod: negative number")]
                                  [(null? xs) (error "list-nth-mod: empty list")]
                                  [(>= n (length xs))  (list-nth-mod xs (remainder n (length xs)))]
                                  [#t (car (list-tail xs n))]
                             ))

(define (stream-for-n-steps s n) (if (= n 0) null
                                 (let ([pr (s)])
                                   (cons (car pr) (stream-for-n-steps (cdr pr) (- n 1)))
                                 )))
                               
(define (funny-number-stream) (make-stream (lambda (x) (cond [(= (remainder x 5) 0) (+ (- x) 1)]
                                                             [(= (remainder x 5) 4) (- (+ x 1))]
                                                             [#t (+ x 1)]
                                                             )) 1 ))
                                                           
(define (dan-then-dog) (letrec ([dan (lambda() (cons "dan.jpg" dog))]
                                [dog (lambda() (cons "dog.jpg" dan))]
                                )
                         (dan)
                         ))

(define (stream-add-zero s) (let ([pr (s)])
                            (lambda () (cons (cons 0 (car pr)) (stream-add-zero (cdr pr))))
                            ))

(define (cycle-lists xs ys) (letrec ([helper
                                      (lambda (n)
                                        (cons 
                                         (cons (list-nth-mod xs n) (list-nth-mod ys n))
                                         (lambda () (helper (+ n 1)))
                                        )
                                      )
                                     ]) (lambda () (helper 0))))



(define (vector-assoc v vec) (letrec ([helper (lambda (begin end) 
                                                (if (< begin end) 
                                                    (let ([elem (vector-ref vec begin)]) 
                                                      (if (pair? elem)
                                                          (if (equal? (car elem) v) elem (helper (+ begin 1) end))
                                                          (helper (+ begin 1) end)
                                                      )
                                                    ) #f
                                                )
                                              )]) (helper 0  (vector-length vec) )))
                                                
(define (cached-assoc xs n) (let ([vec (make-vector n #f)]
                                  [pos 0]
                                  [next (lambda (x) (if (= x (- n 1)) 0 (+ x 1)))] 
                                  )
                             (lambda (v)
                               (let ([elem (vector-assoc v vec)])
                                 (if 
                                  (equal? elem #f)
                                     (begin 
                                       (set! elem (assoc v xs))
                                       (if (equal? elem #f) #f
                                           (begin 
                                             (vector-set! vec pos elem)
                                             (set! pos (next pos))
                                             elem
                                           )
                                       )
                                     )
                                     elem
                                 )
                               )
                              )))

                                                          
                                      
                                        



