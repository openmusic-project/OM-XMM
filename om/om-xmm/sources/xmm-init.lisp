
;;;===================================================
;;;
;;; OM-XMM
;;; Author: Paul Best
;;;
;;;===================================================

(in-package :om)

(defpackage :xmm 
  (:use :common-lisp :cl-user))


(defun load-xmm-lib ()
  (let ((libpath (merge-pathnames 
                  "lib/mac/libxmm.dylib" 
                  (om::mypathname (om::find-om-library "om-xmm")))))
    (om-fi::om-load-foreign-library
           "LIBXMM"
           `((:macosx ,libpath)
             (t (:default "libxmm"))))
    ))

;; load now
(load-xmm-lib)

;; load at OM startup
;; #+macosx(om-fi::add-foreign-loader 'load-iae-lib)

(push :xmm *features*)


#|


|#






