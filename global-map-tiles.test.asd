;; global-map-tiles.test.asd 
;;
;; Copyright (c) 2021 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>
;;
;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.

(in-package :cl-user)
(defpackage :global-map-tiles.test-asd
  (:use :cl :asdf))
(in-package :global-map-tiles.test-asd)

(asdf:defsystem #:global-map-tiles.test
  :description "Test global-map-tiles"
  :author "Jeremiah LaRocco <jeremiah_larocco@fastmail.com>"
  :license  "ISC"
  :version "0.0.1"
  :serial t
  :depends-on (#:fiveam #:global-map-tiles)
  :components ((:module "t"
                :components
                ((:file "package"))))
  :perform (test-op
            :after (op c)
            (eval (read-from-string
                   "(every #'fiveam::TEST-PASSED-P (5am:run :global-map-tiles))"))))
