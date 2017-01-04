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

(deftype state ()
  `(member :inactive :running :ready :expired)
  "A period starts off in the :INACTIVE state.
When the first PERIOD call is made, it goes to :RUNNING.
When :RUNNING
  - if the thread calls PERIOD before the timeout, the
    period state will be :READY.  
  - if the interval elapses prior to the thread calling PERIOD,
    the period state changes to :EXPIRED.
When :READY, the period state will change to :RUNNING after
    the interval elapses.
When :EXPIRED, the period state will change to :RUNNING after
    the thread calls PERIOD, and the period will be reinitialized
    with the provided interval.")

(defstruct stat 
  (count  0)
  (missed-count 0)
  (max-runtime 0)
  (total-runtime 0)
  (min-runtime 0)
  (last-start 0))

(defun reset-statistics (stat)
  "Reset the values in a STAT struct."
  (setf (stat-count stat)         0
	(stat-missed-count stat)  0
	(stat-max-runtime stat)   0
	(stat-total-runtime stat) 0
	(stat-min-runtime stat)   0))

(defun update-statistics (stat this-time)
  "Update STAT object with internal-real-time deltat THIS-TIME for a period."
  (if (zerop (stat-count stat))
      (setf (stat-min-runtime stat) this-time)
      (incf (stat-total-runtime stat) this-time))
  (when (> this-time (stat-max-runtime stat))
    (setf (stat-max-runtime stat) this-time))
  (incf (stat-count stat)))

(defmethod print-object ((stat stat) stream)
  (print-unreadable-object (stat stream :type t)
    (format stream ":COUNT ~D :MISSED ~D :MIN ~D :AVG ~D :MAX ~D"
	    (stat-count stat)
	    (stat-missed-count stat)
	    (stat-min-runtime stat)
	    (round (stat-total-runtime stat) (stat-count stat))
	    (stat-max-runtime stat))))

(defclass period ()
  ((name :accessor period-name
	 :initform ""
	 :initarg :name)
   (state :accessor period-state
	  :initform :inactive
	  :type 'state)
   (statistics :accessor period-statistics
	       :initform (make-stat))
   (interval :accessor period-interval
	     :initarg :interval)))

(defgeneric cancel (period)
  (:documentation "Cancel a period."))

(defgeneric period (period type interval)
  (:documentation "Initiate a PERIOD with the provided INTERVAL.  If a PERIOD is
  already running, then the thread will block for the remainder of the interval
  at which point the thread is unblocked for the new duration.  The return value
  is :SUCCESSFUL when initiating or calling prior to the previous interval.

  If a running period interval has already elapsed, the period is re-initiated
  with the new interval, and the call returns :TIMEOUT.

  TYPE can be one of the following:
  - :TICKS - INTERVAL is an unsigned integer indicating an interval in the
              underlying resolution.
  - :SECONDS - INTERVAL is a real value.
  - :MS - INTERVAL is an integral millisecond interval."))

(defgeneric status (period)
  (:documentation "Return the status of the period in 2 values.
 - State - :EXPIRED, :READY, :INACTIVE, or :RUNNING
 - Time since last period."))


