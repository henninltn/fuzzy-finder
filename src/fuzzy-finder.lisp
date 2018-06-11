(defpackage fuzzy-finder
  (:use :cl
        :alexandria))
(in-package :fuzzy-finder)

(defun shift-and (text pattern)
  (let* ((text (concatenate 'list text))
         (pattern (concatenate 'list pattern))
         (finish (ash 1 (- (length pattern) 1)))
         (mask (alist-hash-table (loop for c in (remove-duplicates text)
                                       collect (cons c 0))
                                 :test 'equal)))
    (loop for char in pattern
          do (loop for mk in (hash-table-keys mask)
                   do (setf (gethash mk mask)
                            (ash (gethash mk mask) -1))
                   do (if (eq char mk)
                          (setf (gethash mk mask)
                                (logior (gethash mk mask)
                                        finish)))))
    (let ((state 0))
      (loop for char in text
            for i from 0 below (length text)
            do (setf state (logand (logior (ash state 1)
                                           1)
                                   (gethash char mask)))
            if (>= state finish)
              collect i))))

