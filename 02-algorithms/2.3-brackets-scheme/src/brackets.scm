(set-port-encoding! (current-output-port) "UTF-8")
(set-port-encoding! (current-error-port) "UTF-8")

(define (fail msg)
  (display (string-append "Ошибка: " msg) (current-error-port))
  (newline (current-error-port))
  (exit 1))

(define (usage)
  (display "Использование:" (current-error-port))
  (newline (current-error-port))
  (display "  guile brackets.scm gen <n> [-c]" (current-error-port))
  (newline (current-error-port))
  (display "  guile brackets.scm check <строка>" (current-error-port))
  (newline (current-error-port))
  (exit 1))

(define (gen open close prefix n)
  (if (= (+ open close) (* 2 n))
      (list (list->string (reverse prefix)))
      (append
       (if (< open n) (gen (+ open 1) close (cons #\( prefix) n) '())
       (if (< close open) (gen open (+ close 1) (cons #\) prefix) n) '()))))

(define (count-seq open close n)
  (if (= (+ open close) (* 2 n))
      1
      (+ (if (< open n) (count-seq (+ open 1) close n) 0)
         (if (< close open) (count-seq open (+ close 1) n) 0))))

(define (factorial n)
  (if (= n 0) 1 (* n (factorial (- n 1)))))

(define (catalan n)
  (/ (factorial (* 2 n)) (* (factorial (+ n 1)) (factorial n))))

(define (check s i stack)
  (cond
   ((= i (string-length s))
    (if (null? stack)
        "правильная"
        (string-append "неправильная: скобка в позиции "
                       (number->string (car (last-pair stack)))
                       " не закрыта")))
   ((char=? (string-ref s i) #\()
    (check s (+ i 1) (cons (+ i 1) stack)))
   ((char=? (string-ref s i) #\))
    (if (null? stack)
        (string-append "неправильная: лишняя закрывающая скобка в позиции "
                       (number->string (+ i 1)))
        (check s (+ i 1) (cdr stack))))
   (else
    (fail (string-append "недопустимый символ '"
                         (string (string-ref s i))
                         "' в позиции "
                         (number->string (+ i 1)))))))

(define (to-number text)
  (let ((n (string->number text)))
    (if (and n (integer? n) (exact? n) (>= n 0))
        n
        (fail "n должно быть неотрицательным целым числом"))))

(define (run-gen n only-count)
  (let ((total (if only-count
                   (count-seq 0 0 n)
                   (let ((result (gen 0 0 '() n)))
                     (for-each
                      (lambda (s)
                        (display (if (string-null? s) "(пустая последовательность)" s))
                        (newline))
                      result)
                     (length result)))))
    (display (string-append "всего: " (number->string total)))
    (newline)
    (display (string-append "число Каталана C(" (number->string n) ") = "
                            (number->string (catalan n))))
    (newline)))

(define (main args)
  (cond
   ((and (>= (length args) 2) (string=? (car args) "gen"))
    (let ((n (to-number (cadr args)))
          (only-count (and (= (length args) 3) (string=? (caddr args) "-c"))))
      (if (and (= (length args) 3) (not only-count))
          (usage))
      (if (> (length args) 3)
          (usage))
      (run-gen n only-count)))
   ((and (= (length args) 2) (string=? (car args) "check"))
    (display (string-append "\"" (cadr args) "\": " (check (cadr args) 0 '())))
    (newline))
   (else (usage))))

(main (cdr (command-line)))
