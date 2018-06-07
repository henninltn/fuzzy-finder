(defpackage fuzzy-finder
  (:use :cl
        :alexandria))
(in-package :fuzzy-finder)

;; TEST
(let ((mask (make-hash-table)))
  (setf (gethash :a mask) 0)
  (setf (gethash :b mask) 0)
  (setf (gethash :c mask) 0)
  (setf (gethash :d mask) 0)
  (setf (gethash :e mask) 0)
  (setf (gethash :f mask) 0)
  (setf (gethash :g mask) 0)
  (loop for char in (list :c :d :e)
        do (loop for mk in (list :a :b :c :d :e :f :g)
                 do (format t "~c~a, ~a:~c" #\Newline char mk #\Newline)
                 do (setf (gethash mk mask)
                          (ash (gethash mk mask) -1))
                 do (if (eq char mk)
                        (setf (gethash mk mask)
                              8))
                 do (maphash (lambda (key val) (format t "~a: ~a~c" key val #\Newline))
                             mask))))

(defun char-to-keyword (char)
  (intern (string-upcase (string char)) :keyword))

;; FAIL
(defun shift-and (text pattern)
  (let* ((text (concatenate 'list text))
         (pattern (concatenate 'list pattern))
         (finish (ash 1 (- (length pattern) 1)))
         (mask (alist-hash-table
                (loop for char in (remove-duplicates text)
                      collect (cons (char-to-keyword char)
                                    0)))))
    (loop for p-char in pattern
          do (loop for mask-key being the hash-keys of mask
                   for mask-val being the hash-values of mask
                   do (setf (gethash mask-key mask)
                            (ash mask-val -1))
                   do    (if (equal (char-to-keyword p-char) mask-key)
                             (setf (gethash mask-key mask)
                                   (logior mask-val finish)))))
    (maphash (lambda (key val) (format t "~a: ~a~c" key val #\Newline))
             mask)
    (let ((state 0))
      (loop for t-char in text
            for i from 0 below (length text)
            do (setf state (logand (logior (ash state 1) 1)
                                   (gethash (char-to-keyword t-char) mask)))
            if (>= state finish)
              collect i))))
