(in-package :xmm)

(cffi::defcfun ("trainXMM" trainXMM) :int (descrs :pointer) (sample_num :int) (sample_sizes :pointer) (laabels :pointer) (modelptr :pointer))
(cffi::defcfun ("initXMM" initXMM) :pointer)
(cffi::defcfun ("runXMM" runXMM) :int (descrs :pointer) (sample_size :int) (modelptr :pointer))


;; (listen *terminal-io*)
;; (setq test (openxmm))
;; (closexmm test)