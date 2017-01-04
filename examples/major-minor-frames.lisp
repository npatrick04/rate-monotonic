(in-package #:rate-monotonic.examples)

;;; Values in milliseconds.
(defparameter *frame-resolution* 20)
(defparameter *major-period* 1000)
(defparameter *minor-period* 200)

(defun major-minor-frame-example (&optional (major-frames 5))
  (let ((major (make-timer-period))
	(minor (make-timer-period))
	(minor-per-major (floor *major-period* *minor-period*)))
    (with-timer-period (*frame-resolution*)
      (dotimes (major-frame major-frames)
	(period major :ms *major-period*)
	(format t "Major frame ~A~%" major-frame)
	(dotimes (minor-frame minor-per-major)
	  (period minor :ms *minor-period*)
	  (format t "~TMinor frame ~A~%" minor-frame))))))
