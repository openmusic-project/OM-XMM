;============================================================================
; OM-XMM
; A bridge between o7 and the XMM library.
;
; XMM is a C++ library that implements Gaussian Mixture Models and Hidden Markov Models for recognition and regression. 
; The library implements an interactive machine learning workflow with fast training and continuous, real-time inference. 
; See http://ircam-rnd.github.io/xmm/
;============================================================================
;
;   This program is free software. For information on usage 
;   and redistribution, see the "LICENSE" file in this distribution.
;
;   This program is distributed in the hope that it will be useful,
;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. 
;
;============================================================================

;;;=====================================
;;; OM-XMM lib loader for OM6
;;;=====================================
;;; Author J. Bresson
;;;=====================================

(in-package :om)

(om::set-lib-release 1.0 (find-library "om-xmm"))

(compile&load (merge-pathnames "sources/xmm-init" *load-pathname*))
(compile&load (merge-pathnames "sources/xmm-bindings" *load-pathname*))
(compile&load (merge-pathnames "sources/xmm-om-objects" *load-pathname*))

(om::fill-library 
 '((nil nil (xmm::xmmobj) (xmm::run xmm::get-errors xmm::test) nil)))

(print "
 ==============================
 OM-XMM
 ==============================
 A bridge between OM and the XMM library
 ==============================
")
