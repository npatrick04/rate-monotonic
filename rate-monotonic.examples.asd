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

(asdf:defsystem #:rate-monotonic.examples
  :description "Examples for the rate monotonic period scheduler."
  :author "Nick Patrick <npatrick04@gmail.com>"
  :license "GPL-v3"
  :depends-on (#:bordeaux-threads #:rate-monotonic)
  :serial t
  :components ((:module "examples"
			:components
			((:file "package")
			 (:file "major-minor-frames")
			 (:file "subframes")))))

