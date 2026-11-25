(defun fail (msg)
  (format *error-output* "Ошибка: ~a~%" msg)
  (finish-output *error-output*)
  (sb-ext:exit :code 1))

(defun usage ()
  (format *error-output* "Использование: sbcl --script dedup.lisp [--adjacent] [--ignore-case] [--trim] [--count] [-o выходной_файл] <файл>~%")
  (finish-output *error-output*)
  (sb-ext:exit :code 1))

(defun read-lines (path)
  (handler-case
      (with-open-file (in path :external-format :utf-8 :if-does-not-exist nil)
        (unless in
          (fail (format nil "не удалось открыть файл ~a" path)))
        (loop for line = (read-line in nil)
              while line
              collect (string-right-trim '(#\Return) line)))
    (file-error ()
      (fail (format nil "не удалось открыть файл ~a" path)))
    (sb-int:stream-decoding-error ()
      (fail (format nil "файл ~a не в кодировке UTF-8" path)))))

(defun make-key (line ignore-case trim)
  (let ((s (if trim (string-trim '(#\Space #\Tab) line) line)))
    (if ignore-case (string-downcase s) s)))

(defun group-lines (lines adjacent ignore-case trim)
  (let ((seen (make-hash-table :test 'equal))
        (groups '())
        (prev nil))
    (dolist (line lines)
      (let ((k (make-key line ignore-case trim)))
        (if adjacent
            (if (and groups (string= k prev))
                (incf (cdr (first groups)))
                (push (cons line 1) groups))
            (let ((g (gethash k seen)))
              (if g
                  (incf (cdr g))
                  (let ((new (cons line 1)))
                    (setf (gethash k seen) new)
                    (push new groups)))))
        (setf prev k)))
    (nreverse groups)))

(defun write-groups (stream groups count)
  (dolist (g groups)
    (if count
        (format stream "~7d ~a~%" (cdr g) (car g))
        (format stream "~a~%" (car g)))))

(defun main (args)
  (let ((adjacent nil) (ignore-case nil) (trim nil) (count nil) (out nil) (files '()))
    (loop while args
          do (let ((a (pop args)))
               (cond ((string= a "--adjacent") (setf adjacent t))
                     ((string= a "--ignore-case") (setf ignore-case t))
                     ((string= a "--trim") (setf trim t))
                     ((string= a "--count") (setf count t))
                     ((string= a "-o")
                      (unless args (usage))
                      (setf out (pop args)))
                     ((and (> (length a) 0) (char= (char a 0) #\-))
                      (fail (format nil "неизвестный параметр ~a" a)))
                     (t (push a files)))))
    (unless (= (length files) 1)
      (usage))
    (let* ((path (first files))
           (lines (read-lines path))
           (groups (group-lines lines adjacent ignore-case trim)))
      (when (and out (string= out path))
        (fail "выходной файл совпадает с входным"))
      (if out
          (handler-case
              (with-open-file (stream out :direction :output :if-exists :supersede
                                          :external-format :utf-8)
                (write-groups stream groups count))
            (file-error ()
              (fail (format nil "не удалось записать файл ~a" out))))
          (write-groups *standard-output* groups count))
      (when out
        (format t "результат записан в ~a~%" out))
      (finish-output *standard-output*)
      (format *error-output* "строк: ~d, осталось: ~d, удалено: ~d~%"
              (length lines) (length groups) (- (length lines) (length groups))))))

(main (rest sb-ext:*posix-argv*))
