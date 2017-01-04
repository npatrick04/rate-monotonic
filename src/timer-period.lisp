;;; rate-monotonic is a Common Lisp thread scheduling library.
;;; Copyright 2017 Nicholas Patrick
;;; 
;;; This file is part of rate-monotonic.
;;; 
;;; rate-monotonic is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; rate-monotonic is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with rate-monotonic.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:rate-monotonic)

;;; The timer-period class provides an implementation of the rate-monotonic API
;;; using the timer-wheel library.  It must be initialized with
;;; TIMER-PERIOD-INIT and shutdown with TIMER-PERIOD-STOP (or using the helper
;;; macro WITH-TIMER-PERIOD.

(defparameter *timer-wheel* nil)

(defun timer-period-init (&optional (resolution-ms 100) (wheel-size 100))
  "Initialize the underlying *TIMER-WHEEL* with provided RESOLUTION-MS and WHEEL-SIZE."
  (setf *timer-wheel* (tw:make-wheel wheel-size resolution-ms))
  (tw:initialize-timer-wheel *timer-wheel*))

(defun timer-period-stop ()
  "Shut down the underlying *TIMER-WHEEL*."
  (tw:shutdown-timer-wheel *timer-wheel*)
  (setf *timer-wheel* nil))

(defmacro with-timer-period ((&optional (resolution-ms 100) (wheel-size 100)) &body body)
  "Execute BODY with the *TIMER-WHEEL* initialized and running."
  `(progn
     (setf *timer-wheel* (tw:make-wheel ,wheel-size ,resolution-ms))
     (tw:with-timer-wheel *timer-wheel*
       ,@body)
     (setf *timer-wheel* nil)))

(defclass timer-period (period)
  ((timer :accessor timer-period-timer
	  :initarg :timer)
   (lock :accessor timer-lock
	 :initform (bt:make-lock))
   (cv   :accessor timer-cv
	 :initform (bt:make-condition-variable))))

(defun make-timer-period ()
  "A helper function to make a timer period object."
  (make-instance 'timer-period))

(defun make-timer-period-timeout (timer-period)
  "An internal function that returns a timeout lambda for the given TIMER-PERIOD."
  (lambda (wheel timer)
    (with-accessors ((lock     timer-lock)
		     (cv       timer-cv)
		     (state    period-state)
		     (interval period-interval)
		     (stats    period-statistics))
	timer-period
      (bt:with-lock-held (lock)
	(case state
	  (:ready
	   ;; The thread finished the period on time!  Time to retrigger it. 
	   (tw:schedule-timer wheel timer :ticks interval)
	   (setf state :running)
	   (bt:condition-notify cv))
	  (:running
	   ;; The thread is late!
	   (incf (stat-missed-count stats))
	   (setf state :expired))
	  (:expired
	   (error "Why is the timeout called on an expired period?"))
	  (:inactive
	   (error "Why is the timeout called on an inactive period?")))))))

;;; Implement the rate monotonic interface!

(defmethod cancel ((p timer-period))
  (tw:uninstall-timer *timer-wheel*
		      (timer-period-timer p))
  (setf (period-state p) :inactive)
  :successful)

(defmethod period ((p timer-period) (type (eql :ticks)) interval)
  (with-accessors
	((timer           timer-period-timer)
	 (lock            timer-lock)
	 (cv              timer-cv)
	 (state           period-state)
	 (stats           period-statistics))
      p

    (let* ((now (get-internal-real-time))
	   (dt (- now (stat-last-start stats))))
      ;; TODO: Consider reducing the work performed in the lock
      (bt:with-lock-held (lock)
	(prog1
	    (case state
	      (:inactive
	       (setf timer (tw:make-timer (make-timer-period-timeout p)))
	       (tw:schedule-timer *timer-wheel* timer :ticks interval)
	       (setf state :running)
	       :successful)
	      (:ready
	       (error "PERIOD called on a READY period???"))
	      (:running
	       (update-statistics stats dt)
	       (setf state :ready
		     (period-interval p) interval)
	       (bt:condition-wait cv lock)
	       :successful)
	      (:expired
	       (update-statistics stats dt)
	       (tw:schedule-timer *timer-wheel* timer :ticks interval)
	       (setf state :running)
	       :timeout))
	  (setf (stat-last-start stats) (get-internal-real-time)))))))

(defmethod period ((p timer-period) (type (eql :seconds)) interval)
  (period p :ms (round (* interval 1000))))

(defmethod period ((p timer-period) (type (eql :ms)) interval)
  (multiple-value-bind (ticks remainder)
      (ceiling interval (tw:wheel-resolution *timer-wheel*))
    (unless (zerop remainder)
      (warn "An interval of ~A milliseconds doesn't match resolution of ~A ticks."
	    interval
	    (tw:wheel-resolution *timer-wheel*)))
    (period p :ticks ticks)))

(defmethod status ((p timer-period))
  (values
   (period-state p)
   (- (get-internal-real-time)
      (stat-last-start (period-statistics p)))))


