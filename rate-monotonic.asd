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

(asdf:defsystem #:rate-monotonic
  :description "A rate monotonic period scheduler inspired by RTEMS."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "Specify license here"
  :depends-on (#:bordeaux-threads #:timer-wheel)
  :serial t
  :components ((:module "src"
			:components
			((:file "package")
			 (:file "rate-monotonic")
			 (:file "timer-period")))))

