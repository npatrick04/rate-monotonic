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
  (:documentation "Cancel a period, putting it into the inactive state."))

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

(defgeneric finish-period (period)
  (:documentation "Complete a running PERIOD without restarting it.  The period
  will not block on this call.  The return value is :SUCCESSFUL when calling
  prior to the previous interval.

  If a running period interval has already elapsed, the call returns :TIMEOUT.

  This does essentially the same thing as CANCEL, but this captures statistics."))

(defgeneric status (period)
  (:documentation "Return the status of the period in 2 values.
 - State - :EXPIRED, :READY, :INACTIVE, or :RUNNING
 - Time since last period."))


