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

(defun fuzzy-finder (text pattern &key (distance 5))
  (bitap-base
    text pattern
    (lambda (text pattern finish mask)
      (format *DEBUG-IO* "T: ~a~cP: ~a~c" text #\Newline pattern #\Newline)
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
              do (format *DEBUG-IO* ";; shift動作~cstate:~c" #\Newline #\Newline)
              do (loop for i from 0 below distance
                       do (setf (aref state-array i)
                                (logior (ash (aref state-array i) 1) 1))
                       do (format *DEBUG-IO* "  ~3,'0b~c" (aref state-array i) #\Newline))
              do (format *DEBUG-IO* ";; save動作~cstate:~c" #\Newline #\Newline)
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

;; T: abc
;; P: abb
;;
;; mask:
;;   a -> 001
;;   b -> 110
;;
;; ;; 初期化
;; state:
;;   000  0
;;   001  1
;;
;; ;; shift動作
;; state:
;;   001  1
;;   011  3
;;
;; ;; save
;; save-state:
;;   011 <- 001 | 010 | 000
;;   111 <- 011 | 110 | 001
;;
;; ;; and動作
;; state:
;;   001 1
;;   001 1
;;
;; ;; 合成
;; state:
;;   001 1
;;   011 3
;;
;; ;; shift動作
;; state:
;;   011  3
;;   111  7
;;
;; ;; save
;; save-state:
;;   111 <- 011 | 110 | 001
;;   111 <- 111 | 111 | 011
;;
;; ;; and動作
;; state:
;;   011 3
;;   011 3
;;
;; ;; 合成
;; state:
;;   011 1
;;   111 7
;;
;; ;; shift動作
;; state:
;;   111  7
;;  1111 15
;;
;; ;; save
;; save-state:
;;   111 <- 111 | 111 | 110
;;   111 <- 111 | 111 | 110
;;
;; ;; and動作
;; state:
;;   110 6
;;   110 6
;;
;; ;; 合成
;; state:
;;   110 6
;;   111 7
;;

(defun fuzzy-finder (text pattern &key (distance 5))
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
    (let ((state-array (make-array (list distance) :initial-element 0))
          (save-state-array (make-array (list distance))))
      (loop for char in text
            do (loop for state across state-array
                     for i from 0 below (length state-array)
                     do (setf (aref state-array i)
                              (1- (ash 1 i)))
                     do (setf (aref state-array i)
                              (logior (ash state 1)))
                     do (setf (aref save-state-array i)
                              (logior state
                                      (ash state 1)
                                      (ash state -1)))
                     do (setf (aref state-array i)
                              (logand state (gethash char mask))))
            do (loop for i from 1 below (length state-array)
                     do (setf (aref state-array i)
                              (aref save-state-array (1- i))))
            collect (loop for state across state-array
                          for i from 0 below (length state-array)
                          if (>= state finish)
                            collect i)))))
