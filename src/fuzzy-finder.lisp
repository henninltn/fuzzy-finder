(defpackage fuzzy-finder
  (:use :cl
        :alexandria)
  (:export :shift-and
           :shift-or
           :levenshtein-distance
           :fuzzy-finder))
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
  (bitap-base
    text pattern
    (lambda (text pattern finish mask)
      (let ((state 0))
        (loop for char in text
              for i from 0 below (length text)
              do (setf state
                       (logand (logior (ash state 1)
                                       1)
                               (gethash char mask)))
              if (>= state finish)
                collect i)))))

(defun shift-or (text pattern)
  (bitap-base
    text pattern
    (lambda (text pattern finish mask)
      (let* ((max (1- (expt 2 (length pattern))))
             (state max))
        (loop for char in text
              for i from 0 below (length text)
              do (setf state
                       (logior (logand max
                                       (ash state 1))
                               (logxor max
                                       (gethash char mask))))
              if (<= state finish)
                collect i)))))

(defun levenshtein-distance (str1 str2)
  (let* ((len-str1 (length str1))
         (len-str2 (length str2))
         (d (make-array (list (1+ len-str1) (1+ len-str2)))))
    (loop for i from 0 to len-str1
          do (setf (aref d i 0) i))
    (loop for i from 0 to len-str2
          do (setf (aref d 0 i) i))
    (loop for i1 from 1 to len-str1
          do (loop for i2 from 1 to len-str2
                   do (setf (aref d i1 i2)
                            (min (1+ (aref d (1- i1) i2))
                                 (1+ (aref d i1 (1- i2)))
                                 (+ (aref d (1- i1) (1- i2))
                                    (if (equal (char str1 (1- i1)) (char str2 (1- i2))) 0 1))))))
    (aref d len-str1 len-str2)))

(defun fuzzy-finder (text pattern &key (distance 5))
  (bitap-base
    text pattern
    (lambda (text pattern finish mask)
      (format *DEBUG-IO* "T: ~a~cP: ~a~c" (concatenate 'string text) #\Newline (concatenate 'string pattern) #\Newline)
      (format *DEBUG-IO* "finish: ~3,'0b~c" finish #\Newline)
      (format *DEBUG-IO* "mask:~c" #\Newline)
      (maphash (lambda (k v) (format *DEBUG-IO* "  ~a -> ~3,'0b~c" k v #\Newline))
               mask)
      (let ((state-array (make-array (list distance)))
            (save-state-array (make-array (list distance))))
        (format *DEBUG-IO* ";; 初期化~c" #\Newline)
        (format *DEBUG-IO* "state:~c" #\Newline)
        (loop for i from 0 below distance
              do (setf (aref state-array i) (1- (ash 1 i)))
              do (format *DEBUG-IO* "  ~3,'0b~c" (aref state-array i) #\Newline))
        (loop for char in text
              for char-i from 0 below (length text)
              do (format *DEBUG-IO* "T[~a]: ~a~c" char-i char #\Newline)
              do (format *DEBUG-IO* ";; shift動作~cstate:~c" #\Newline #\Newline)
              do (loop for i from 0 below distance
                       do (setf (aref state-array i)
                                (logior (ash (aref state-array i) 1) 1))
                       do (format *DEBUG-IO* "  ~3,'0b~c" (aref state-array i) #\Newline))
              do (format *DEBUG-IO* ";; save動作~csave-state:~c" #\Newline #\Newline)
              do (loop for i from 0 below distance
                       do (setf (aref save-state-array i)
                                (logior (aref state-array i)
                                        (ash (aref state-array i) 1)
                                        (ash (aref state-array i) -1)))
                       do (format *DEBUG-IO* "  ~3,'0b~c" (aref save-state-array i) #\Newline))
              do (format *DEBUG-IO* ";; and動作~cstate:~c" #\Newline #\Newline)
              do (loop for i from 0 below distance
                       do (setf (aref state-array i)
                                (logand (aref state-array i)
                                        (gethash char mask)))
                       do (format *DEBUG-IO* "  ~3,'0b~c" (aref state-array i) #\Newline))
              do (format *DEBUG-IO* ";; 合成~cstate:~c" #\Newline #\Newline)
              do (format *DEBUG-IO* "  ~3,'0b~c" (aref state-array 0) #\Newline)
              do (loop for i from 1 below distance
                       do (setf (aref state-array i)
                                (logior (aref state-array i)
                                        (aref save-state-array (1- i))))
                       do (format *DEBUG-IO* "  ~3,'0b~c" (aref state-array i) #\Newline))
              append (loop for state across state-array
                           for i from 0 below (length state-array)
                           do (if (>= state finish)
                                  (format t "collect ~d~c" i #\Newline))
                           if (>= state finish)
                             collect (cons i char-i)))))))
