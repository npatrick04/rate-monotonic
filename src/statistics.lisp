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

