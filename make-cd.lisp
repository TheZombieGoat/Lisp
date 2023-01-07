(defun make-cd (title artist rating)
  (list :title title :artist artist :rating rating))

(defun add-record (cd) (push cd *db*))

(defun dumb ()
    (dolist (cd *db*)
      (format t "~{~a:~10t~a~%~}~%" cd)))
  
(defun prompt-read (prompt)
    (format *query-io* "~a: " prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun prompt-for-cd ()
    (make-cd
     (prompt-read "Title")
     (prompt-read "Artist")
     (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)))

(defun add-cds ()
    (loop (add-record (prompt-for-cd))
          (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

(defun s-artist (artist)
  (remove-if-not
   #'(lambda (cd) (equal (getf cd :artist) artist))
   *db*))

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun where (&key title artist rating)
  #'(lambda (cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t))))
