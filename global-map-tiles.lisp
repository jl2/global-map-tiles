;; global-map-tiles.lisp
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

(in-package :global-map-tiles)

(defclass global-mercator ()
  ((tile-size :initform 256 :initarg :tile-size :accessor tile-size)))

(defgeneric initial-resolution (coord-system))
(defgeneric origin-shift (coord-system))
(defgeneric lat-lon-to-meters (cs lat lon))
(defgeneric meters-to-lat-lon (cs mx my))
(defgeneric resolution (cs zoom))
(defgeneric pixels-to-meters (cs px py zoom))
(defgeneric meters-to-pixels (cs mx my zoom))
(defgeneric pixels-to-tile (cs px py))
(defgeneric pixels-to-raster (cs px py zoom))
(defgeneric meters-to-tile (cs mx my zoom))
(defgeneric tile-bounds (cs tx ty zoom))
(defgeneric tile-lat-lon-bounds (cs tx ty zoom))
(defgeneric zoom-for-pixel-size (cs pixel-size))
(defgeneric google-tile (cs tx ty zoom))
(defgeneric quad-tree (cs tx ty zoom))


(defmethod initial-resolution ((coord-system global-mercator))
  (with-slots (tile-size) coord-system
    (/ (* 2 pi 6378137) tile-size)))

(defmethod origin-shift ((coord-system global-mercator))
  (* pi 6378137))

(defmethod lat-lon-to-meters ((cs global-mercator) lat lon)
  (declare (ignorable cs))
  (let ((mx (/ (* lon (origin-shift cs)) 180))
        (my (/ (log (tan (* (+ 90 lat) (/ pi 360)))) (/ pi 180))))
    (values mx (/ (* my (origin-shift cs)) 180))))


(defmethod meters-to-lat-lon ((cs global-mercator) mx my)
  (declare (ignorable cs))
  (let ((lon (* 180 (/ mx (origin-shift cs))))
        (lat (* 180 (/ my (origin-shift cs)))))
    (values
     (* (/ 180 pi)
        (- (* 2
              (atan (exp (* lat
                            (/ pi 180)))))
           (/ pi 2)))
     lon)))


(defmethod resolution ((cs global-mercator) zoom)
  (declare (ignorable cs))
  (/ (initial-resolution cs) (expt 2 zoom)))


(defmethod pixels-to-meters ((cs global-mercator) px py zoom)
  (declare (ignorable cs))
  (let* ((res (resolution cs zoom))
         (mx (- (* px res) (origin-shift cs)))
         (my (- (* py res) (origin-shift cs))))
    (values mx my)))


(defmethod meters-to-pixels ((cs global-mercator) mx my zoom)
  (declare (ignorable cs))
  (let* ((res (resolution cs zoom))
         (px (/ (+ mx (origin-shift cs)) res))
         (py (/ (+ my (origin-shift cs)) res)))
    (values px py)))


(defmethod pixels-to-tile ((cs global-mercator) px py)
  (declare (ignorable cs))
  (let ((tx (1- (ceiling px (tile-size cs))))
        (ty (1- (ceiling py (tile-size cs)))))
    (values tx ty)))

(defmethod pixels-to-raster ((cs global-mercator) px py zoom)
  (declare (ignorable cs))
  (let ((map-size (ash (tile-size cs) zoom)))
    (values px (- map-size py))))

(defmethod meters-to-tile ((cs global-mercator) mx my zoom)
  (multiple-value-call #'pixels-to-tile cs (meters-to-pixels cs mx my zoom)))

(defmethod tile-bounds ((cs global-mercator) tx ty zoom)
  (with-slots (tile-size) cs
    (multiple-value-bind (minx miny)
        (pixels-to-meters cs
                          (* tx tile-size)
                          (* ty tile-size)
                          zoom)
      (multiple-value-bind (maxx maxy)
          (pixels-to-meters cs
                            (* (1+ tx) tile-size)
                            (* (1+ ty) tile-size)
                            zoom)
        (values minx miny maxx maxy)))))

(defmethod tile-lat-lon-bounds ((cs global-mercator) tx ty zoom)
  (multiple-value-bind (minx miny maxx maxy) (tile-bounds cs tx ty zoom)
    (multiple-value-bind (min-lat min-lon) (meters-to-lat-lon cs minx miny)
      (multiple-value-bind (max-lat max-lon) (meters-to-lat-lon cs maxx maxy)
        (values min-lat min-lon max-lat max-lon)))))

(defmethod zoom-for-pixel-size ((cs global-mercator) pixel-size)
  (loop for i below 30
        if (> pixel-size (resolution cs i)) do
          (return-from zoom-for-pixel-size
            (if (not (zerop i))
                (1- i)
                0))))
(defmethod google-tile ((cs global-mercator) tx ty zoom)
  (values tx (- (1- (expt 2 zoom)) ty)) zoom)

(defmethod quad-tree ((cs global-mercator) tx ty zoom)
  (concatenate
   'string
   (loop with real-ty = (- (expt 2 zoom) 1 ty)
         for i from zoom above 0
         for mask = (ash 1 (1- i))
         collect (let ((digit 0))
                   (when (not (zerop (logand tx mask)))
                     (incf digit))
                   (when (not (zerop (logand real-ty mask)))
                     (incf digit 2))
                   (code-char (+ digit (char-code #\0)))))))
