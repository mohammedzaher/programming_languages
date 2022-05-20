
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence x y z)
  (if (> x y)
      null
      (cons x (sequence (+ x z) y z))))

(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))

(define (stream-for-n-steps s n)
  (if (= n 0)
      null
      (cons (car (s)) (stream-for-n-steps (cdr(s)) (- n 1)))))

(define funny-number-stream
  (letrec ([f (lambda (x) (if (= (remainder x 5) 0)
                              (cons (- x) (lambda () (f (+ x 1))))
                              (cons x (lambda () (f (+ x 1))))))])
    (lambda () (f 1))))

(define dan-then-dog
  (letrec ([f (lambda (x) (if (string=? x "dog.jpg")
                              (cons "dan.jpg" (lambda () (f "dan.jpg")))
                              (cons "dog.jpg" (lambda () (f "dog.jpg")))))])
    (lambda () (f "dog.jpg"))))

(define (stream-add-zero s)
  (lambda () (cons
              (cons 0 (car (s)))
              (stream-add-zero (cdr (s))))))

(define (cycle-lists xs ys)
  (letrec ([f (lambda (n) (cons
                           (cons (list-nth-mod xs n)
                                 (list-nth-mod ys n))
                           (lambda () (f (+ n 1)))))])
    (lambda () (f 0))))

(define (vector-assoc v vec)
  (letrec ([f (lambda (n) (cond
                            [(equal? n (vector-length vec)) #f]
                            [(pair? (vector-ref vec n)) (if (equal? (car (vector-ref vec n)) v)
                                                           (vector-ref vec n)
                                                           (f (+ n 1)))]
                            [#t (f (+ n 1))]))])
    (f 0)))

(define (cached-assoc xs n)
  (letrec ([memo (make-vector n #f)]
           [i 0]
           [f (lambda (v)
                (let ([ans (vector-assoc v memo)])
                  (if ans
                      ans
                      (let ([new-ans (assoc v xs)])
                        (begin
                          (vector-set! memo i new-ans)
                          (if (= i (- n 1))
                              (set! i 0)
                              (set! i (+ i 1)))
                          new-ans)))))])
    f))