;; package.lisp
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
(defpackage :global-map-tiles.test
  (:use :cl
        :5am
        :alexandria
        :global-map-tiles))

(in-package :global-map-tiles.test)

(5am:def-suite :global-map-tiles)
(5am:in-suite :global-map-tiles)

(defun near (a b &optional (eps 0.000001d0))
  (< (abs (- b a)) eps))

(5am:test global-mercator-test-cases
 (let (
        (cs (make-instance 'gmt:global-mercator))
          (lat-lons '((10 40.0d0 -105.0d0)
                      ))
        (meters '((-11688546.533293726d0 4865942.279503177d0)
                  ))
        (xyz '((213 636)
               ))
        (goog '((213 387)
                ))
        (lat-lon-bounds '((39.90973623453719d0 -105.1171875d0 40.17887331434698d0 -104.765625d0)
                          ))
        (epsg-bounds '((-11701591.786121063d0 4852834.0517692715d0 -11662456.027639052d0 4891969.810251281d0)
                        ))
        (quadkey '("0231010123"
                   )))
    (loop
      for (zoom lat lon) in lat-lons
      for (expected-mx expected-my) in meters
      for (expected-tx expected-ty) in xyz
      for (expected-gx expected-gy) in goog
      for (expected-min-lat expected-min-lon expected-max-lat expected-max-lon) in lat-lon-bounds
      for (expected-min-mx expected-min-my expected-max-mx expected-max-my) in epsg-bounds
      for expected-qk in quadkey
      do
         ;; convert to meters
         (multiple-value-bind (mx my) (gmt:lat-lon-to-meters cs lat lon)
           (is-true (near mx expected-mx))
           (is-true (near my expected-my))
           ;; meters to lat lon
           (multiple-value-bind (exp-lat exp-lon) (gmt:meters-to-lat-lon cs mx my)
             (is-true (near lat exp-lat))
             (is-true (near lon exp-lon)))

           ;; convert to tiles
           (multiple-value-bind (tx ty) (gmt:meters-to-tile cs expected-mx expected-my zoom)
             (is-true (= tx expected-tx))
             (is-true (= ty expected-ty))

             (is-true (string= expected-qk (gmt:quad-tree cs tx ty zoom)))

             ;; Convert tiles to pi
             (multiple-value-bind (min-mx min-my max-mx max-my) (gmt:tile-bounds cs tx ty zoom)
               (is-true (near min-mx expected-min-mx))
               (is-true (near min-my expected-min-my))
               (is-true (near max-mx expected-max-mx))
               (is-true (near max-my expected-max-my)))
             (multiple-value-bind (min-lat min-lon max-lat max-lon) (gmt:tile-lat-lon-bounds cs tx ty zoom)
               (is-true (near min-lat expected-min-lat))
               (is-true (near min-lon expected-min-lon))
               (is-true (near max-lat expected-max-lat))
               (is-true (near max-lon expected-max-lon))))))))
