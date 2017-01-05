(in-package #:rate-monotonic.examples)

;;; Values in milliseconds.
(defparameter *mm-frame-resolution* 20)
(defparameter *mm-major-period* 1000)
(defparameter *mm-minor-period* 200)

(defun major-minor-frame-example (&optional (major-frames 5))
  (let ((major (make-timer-period))
	(minor (make-timer-period))
	(minor-per-major (floor *mm-major-period* *mm-minor-period*)))
    (with-timer-period (*mm-frame-resolution*)
      (dotimes (major-frame major-frames)
	(period major :ms *mm-major-period*)
	(format t "Major frame ~A~%" major-frame)
	(dotimes (minor-frame minor-per-major)
	  (period minor :ms *mm-minor-period*)
	  (format t "~TMinor frame ~A~%" minor-frame))

	;; Don't bias the statistics
	(finish-period minor))
      (finish-period major)
      (list (period-statistics major)
	    (period-statistics minor)))))
