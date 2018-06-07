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


;;;===================================================
;;;
;;; Author: Paul Best
;;;
;;;===================================================

(in-package :om)

(defpackage :xmm 
  (:use :common-lisp :cl-user))


(defun load-xmm-lib ()
  (let ((libpath (merge-pathnames 
                  "lib/mac/libxmm-om.dylib" 
                  (om::mypathname (om::find-library "om-xmm")))))
    (om-fi::om-load-foreign-library
           "LIBXMM"
           `((:macosx ,libpath)
             (t (:default "libxmm-om"))))
    ))

;; load now
(load-xmm-lib)

;; load at OM startup
;; #+macosx(om-fi::add-foreign-loader 'load-iae-lib)

(push :xmm *features*)







