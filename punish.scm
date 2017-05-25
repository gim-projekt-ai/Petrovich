;;;Punish commands definition
;;; PETROVICH
#lang racket
(require racket/format)
(define ns (make-base-namespace))

(define weights-path "petrovich/config/weights/")
(define default-weights-path "petrovich/config/default-weights/")
(define history-path "petrovich/config/history")


;;The global, fallback, default weight for commands
(define (set-global-weight comm w)
  (let ((p (open-output-file (string-append default-weights-path comm) #:exists 'replace)))
    (display w p)
    (newline p)
    (close-output-port p)))

(define (get-global-weight comm)
  (parameterize ((current-namespace ns))
    (load (string-append default-weights-path (~a comm)))))

;;the proper weights
(define (get-weights command)
  (parameterize ((current-namespace ns))
               (load (string-append weights-path command))))
;;finding last one doesnt work!!!
(define (get-weight command what)
      (letrec ((find-the-weight (lambda (weights)
                                  (if (or (equal? weights '()) (void? weights))
                                      (get-global-weight command)
                                      (if (equal? (cdr weights) '())
                                          (get-global-weight command)
                                          ;fallback weight previous line, proper one below:
                                          (if (equal? (caar weights) what)
                                              (cadar weights)
                                              (find-the-weight (cdr weights))))))))
        (find-the-weight (get-weights command))))
                
                
;;These lambdas get command history in diferrent, convenient ways
(define (get-history)
  (let ((p (open-input-file history-path)))
    (let f ((x (read p)))
      (if (eof-object? x)
          (begin
            (close-input-port p)
            '())
          (cons x (f (read p)))))))

(define (current-command)
  (let ((h (reverse (get-history))))
    (~a (car h))))

(define (get-last-3)
  (let ((h (reverse (get-history))))
    (list (cadr h) (caddr h) (cadddr h))))

(define (get-last-2)
  (let ((h (reverse (get-history))))
    (list (cadr h) (caddr h))))

(define (get-last)
  (let ((h (reverse(get-history))))
    (list (cadr h))))

;;Add to history - move elswhere
(define (add-to-history comm)
  (let ((h (open-output-file history-path #:exists 'append)))
    (display comm h)
    (newline h)
    (close-output-port h)))
;;Rewrite weights to file
(define (write-to-weight comm contents)
  (let ((p (open-output-file (string-append weights-path comm) 'replace)))
    (display "'" p)
    (write contents p)
    (close-output-port p)))

;;Lots of fun: manages weights
(define (set-weight comm where new-weight)
  (let ((weights (get-weights comm)))
    (letrec ((find-and-write (lambda (li)
                               (cond
                                 ((equal? li '())
                                  (display "In equal-new")
                                  (write-to-weight
                                   comm
                                   (append weights (list (list where new-weight)))))
                                 ((equal? (caar li) where)
                                  (display "Appendin")
                                  (letrec ((find-insert (lambda (li newli)
                                                          (cond
                                                            ((equal? li '()) newli)
                                                            ((equal? (caar li) where)
                                                             (find-insert (cdr li)
                                                                          (append newli
                                                                                  (list (list where new-weight)))))
                                                            (else (find-insert (cdr li)
                                                                               (append newli
                                                                                       (list (car li)))))))))
                                    (write-to-weight comm (find-insert weights '()))))
                                 (else (find-and-write (cdr li)))))))
      (find-and-write weights))))
  
;;PUNISH and REWARD!!! :-) :D
(define (punish)
  (let ((c (current-command))
        (l1 (get-last))
        (l2 (get-last-2))
        (l3 (get-last-3)))
    (set-global-weight c (- (get-global-weight c) 1))
    (set-weight c l3 (- (get-weight c l3) 10))
    (set-weight c l2 (- (get-weight c l2) 6))
    (set-weight c l1 (- (get-weight c l1) 3))))
  
;(set-global-weight "k53" 40)
;(set-weight "k3" '(k3 k5 k8) 15)
;(get-global-weight "k53")
;(add-to-history "ham2")

;(punish)
(get-global-weight "ham2")
(get-history)
(get-last-3)
(get-last-2)
(get-last)
(current-command)

(get-weights "k3")
(get-global-weight "k3")
(get-weight "k3" '(k1 k2))