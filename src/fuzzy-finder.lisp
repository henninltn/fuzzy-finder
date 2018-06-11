(defpackage fuzzy-finder
  (:use :cl
        :alexandria))
(in-package :fuzzy-finder)

(defun bitap-base (text pattern lambda)
  (let* ((text (concatenate 'list text))
         (pattern (concatenate 'list pattern))
         (finish (ash 1 (1- (length pattern))))
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
    (funcall lambda text pattern finish mask)))

(defun shift-and (text pattern)
  (bitap-base text pattern
              (lambda (text pattern finish mask)
                (let ((state 0))
                  (loop for char in text
                        for i from 0 below (length text)
                        do (setf state (logand (logior (ash state 1)
                                                       1)
                                               (gethash char mask)))
                        if (>= state finish)
                          collect i)))))

(defun shift-or (text pattern)
  (bitap-base text pattern
              (lambda (text pattern finish mask)
                (let* ((max (1- (expt 2 (length pattern))))
                      (state max))
                  (loop for char in text
                        for i from 0 below (length text)
                        do (setf state (logior (logand max
                                                       (ash state 1))
                                               (logxor max
                                                       (gethash char mask))))
                        if (<= state finish)
                          collect i)))))


