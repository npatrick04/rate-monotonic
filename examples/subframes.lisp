(in-package #:rate-monotonic.examples)

;;; Values in milliseconds.
(defparameter *sf-frame-resolution* 20)
(defparameter *sf-major-period* 1000)
(defparameter *sf-first-period* 200)
(defparameter *sf-second-period* 400)

(defun subframes (&optional (major-frames 5))
  (let ((major (make-timer-period))
	(inner (make-timer-period)))
    (with-timer-period (*sf-frame-resolution*)
      (dotimes (major-frame major-frames)
	(period major :ms *major-period*)
	(format t "Major frame ~A~%" major-frame)
	;; Start first period
	(period inner :ms *sf-first-period*)
	;; Do some work
	(format t "~TInner period 1~%")

	;; Start the second period
	(period inner :ms *sf-second-period*)
	;; Do some more work
	(format t "~TInner period 2~%")
	
	;; Don't let the period expire (we don't want to bias the statistics)
	(cancel inner))
      (list (period-statistics major)
	    (period-statistics inner)))))
