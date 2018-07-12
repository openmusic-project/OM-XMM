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


(in-package :xmm)

(cffi::defcfun ("initDataset" xmm-initData) :pointer (numcolumns :int))
(cffi::defcfun ("fillDataset" xmm-filldata) :int (descrs :pointer) (sample_num :int) (sample_sizes :pointer) (laabels :pointer) (dataptr :pointer))
(cffi::defcfun ("trainXMM" xmm-train) :int (dataptr :pointer) (modelptr :pointer))
(cffi::defcfun ("runXMM" xmm-run) :float (descrs :pointer) (sample_size :int) (columnum :int) (modelptr :pointer) (reset :int) (out :pointer))
(cffi::defcfun ("initXMM" xmm-initmodel) :pointer (relat :float) (abs :float) (states :int) (gaussians :int))
(cffi::defcfun ("save_model_JSON" xmm-save) :int (path :pointer) (modelptr :pointer))
(cffi::defcfun ("importJson" xmm-import) :int (path :pointer) (model-ptr :pointer) (lablptr :pointer))
(cffi::defcfun ("free_model" xmm-free) :void (modelptr :pointer) (dataptr :pointer))
(cffi::defcfun ("getclassAvrg" xmm-classavrg) :int (dataptr :pointer) (label :pointer) (out :pointer))

;; (listen *terminal-io*)
;; (setq test (openxmm))
;; (closexmm test)