;; global-map-tiles.lisp
;;
;; Copyright (c) 2022 Jeremiah LaRocco <jeremiah_larocco@fastmail.com>
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
  ((tile-size :initform 256 :type fixnum :initarg :tile-size :accessor tile-size)))

(defparameter *default-coordinate-system* (make-instance 'global-mercator))

(declaim (optimize (speed 1) (space 3) (debug 0) (compilation-speed 0) (safety 0))
         (inline #:global-mercator
                 #:initial-resolution
                 #:origin-shift
                 #:lat-lon-to-meters
                 #:meters-to-lat-lon
                 #:resolution
                 #:pixels-to-meters
                 #:meters-to-pixels
                 #:pixels-to-tile
                 #:pixels-to-raster
                 #:meters-to-tile
                 #:tile-bounds
                 #:tile-lat-lon-bounds
                 #:zoom-for-pixel-size
                 #:google-tile
                 #:quad-tree))

;; (defgeneric initial-resolution (coord-system))
;; (defgeneric origin-shift (coord-system))
;; (defgeneric lat-lon-to-meters (cs lat lon))
;; (defgeneric meters-to-lat-lon (cs mx my))
;; (defgeneric resolution (cs zoom))
;; (defgeneric pixels-to-meters (cs px py zoom))
;; (defgeneric meters-to-pixels (cs mx my zoom))
;; (defgeneric pixels-to-tile (cs px py))
;; (defgeneric pixels-to-raster (cs px py zoom))
;; (defgeneric meters-to-tile (cs mx my zoom))
;; (defgeneric tile-bounds (cs tx ty zoom))
;; (defgeneric tile-lat-lon-bounds (cs tx ty zoom))
;; (defgeneric zoom-for-pixel-size (cs pixel-size))
;; (defgeneric google-tile (cs tx ty zoom))
;; (defgeneric quad-tree (cs tx ty zoom))

(defun lat-lon-tile-boundary (zoom min-lat min-lon max-lat max-lon
                              &key (coord-system  *default-coordinate-system*))
  (multiple-value-call
      #'values
    (multiple-value-call #'meters-to-tile zoom (lat-lon-to-meters min-lat min-lon :coord-system coord-system) :coord-system coord-system)
    (multiple-value-call #'meters-to-tile zoom (lat-lon-to-meters max-lat max-lon :coord-system coord-system) :coord-system coord-system)))

(defun initial-resolution (coord-system)
  (with-slots (tile-size) coord-system
    (/ (* 2.0 pi 6378137.0) tile-size)))

(defun origin-shift (coord-system)
  (declare (ignorable coord-system))
  (* pi 6378137))

(defun lat-lon-to-meters (lat lon
                          &key (coord-system *default-coordinate-system*))
  (let ((mx (/ (* lon (origin-shift coord-system))
               180.0))

        (my (/ (log
                (tan (*
                      (+ 90.0 lat)
                        (/ pi 360.0))))
               (/ pi
                  180.0))))
    (values mx (/ (* my (origin-shift coord-system)) 180.0))))


(defun meters-to-lat-lon (mx my &key (coord-system *default-coordinate-system*))
  (let ((lon (* 180.0 (/ mx (origin-shift coord-system))))
        (lat (* 180.0 (/ my (origin-shift coord-system)))))
    (values
     (* (/ 180.0 pi)
        (- (* 2.0
              (atan (exp (* lat
                            (/ pi 180.0)))))
           (/ pi 2.0)))
     lon)))


(defun resolution (zoom &key (coord-system *default-coordinate-system*))
  (/ (initial-resolution coord-system) (expt 2 zoom)))


(defun pixels-to-meters (zoom px py &key (coord-system *default-coordinate-system*))
  (let* ((res (resolution zoom :coord-system coord-system))
         (mx (- (* px res) (origin-shift coord-system)))
         (my (- (* py res) (origin-shift coord-system))))
    (values mx my)))


(defun meters-to-pixels (zoom mx my &key (coord-system *default-coordinate-system*))
  (let* ((res (resolution zoom :coord-system coord-system))
         (px (/ (+ mx (origin-shift coord-system)) res))
         (py (/ (+ my (origin-shift coord-system)) res)))
    (values px py)))


(defun pixels-to-tile (px py &key (coord-system *default-coordinate-system*))
  (let ((tx (1- (ceiling px (tile-size coord-system))))
        (ty (1- (ceiling py (tile-size coord-system)))))
    (values tx ty)))

(defun pixels-to-raster (zoom px py &key (coord-system *default-coordinate-system*))
  (let ((map-size (ash (tile-size coord-system) zoom)))
    (values px (- map-size py))))

(defun meters-to-tile (zoom mx my &key (coord-system *default-coordinate-system*))
  (multiple-value-call
      #'pixels-to-tile
    (meters-to-pixels zoom mx my :coord-system coord-system)
    :coord-system coord-system))

(defun tile-bounds (zoom tx ty &key (coord-system *default-coordinate-system*))
  (with-slots (tile-size) coord-system
    (multiple-value-bind (minx miny)
        (pixels-to-meters zoom
                          (* tx tile-size)
                          (* ty tile-size)
                          :coord-system coord-system)
      (multiple-value-bind (maxx maxy)
          (pixels-to-meters zoom
                            (* (1+ tx) tile-size)
                            (* (1+ ty) tile-size)
                            :coord-system coord-system)
        (values minx miny maxx maxy)))))

(defun tile-lat-lon-bounds (zoom tx ty &key (coord-system *default-coordinate-system*))
  (multiple-value-bind (minx miny maxx maxy) (tile-bounds zoom tx ty :coord-system coord-system)
    (multiple-value-bind (min-lat min-lon) (meters-to-lat-lon minx miny :coord-system coord-system)
      (multiple-value-bind (max-lat max-lon) (meters-to-lat-lon maxx maxy :coord-system coord-system)
        (values min-lat min-lon max-lat max-lon)))))

(defun zoom-for-pixel-size (pixel-size &key (coord-system *default-coordinate-system*))
  (loop for i below 30
        if (> pixel-size (resolution i :coord-system coord-system)) do
          (return-from zoom-for-pixel-size
            (if (not (zerop i))
                (1- i)
                0))))

(defun google-tile (zoom tx ty)
  (values tx (- (1- (expt 2 zoom)) ty)) zoom)

(defun quad-tree (zoom tx ty)
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
