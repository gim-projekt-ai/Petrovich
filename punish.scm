;;;Punish commands definition
;;; PETROVICH


(define weights-path "petrovich/config/weights/")
(define default-weights-path "petrovich/config/default-weights/")
(define history-path "petrovich/config/history")

(define (get-weights command)
  (load (string-append weights-path command)))
;(cadar (get-weights "k3"))

;;These lambdas get command history in diferrent, convenient ways
(define (get-history)
  (let ((p (open-input-file history-path)))
  (let f ((x (read p)))
    (if (eof-object? x)
        (begin
          (close-input-port p)
          '())
        (cons x (f (read p)))))))

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
  (let ((h (open-output-file history-path 'append)))
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

;;The global, fallback, default weight for commands
(define (set-global-weight comm w)
  (let ((p (open-output-file (string-append default-weights-path comm) 'replace)))
    (display w p)
    (newline p)
    (close-output-port p)))

(define (get-global-weight comm)
  (load (string-append default-weights-path comm)))
  
;;PUNISH and REWARD!!! :-) :D
;(define (punish)
  
;(set-global-weight "k53" 40)
;(set-weight "k3" '(k3 k5 k8) 15)
;(get-global-weight "k53")
(get-history)
(get-last-3)
(get-last-2)
(get-last)
