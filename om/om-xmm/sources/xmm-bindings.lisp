(in-package :xmm)

(cffi::defcfun ("trainXMM" xmm-train) :int (descrs :pointer) (sample_num :int) (sample_sizes :pointer) (laabels :pointer) (modelptr :pointer))
(cffi::defcfun ("initXMM" xmm-init) :pointer)
(cffi::defcfun ("runXMM" xmm-run) :int (descrs :pointer) (sample_size :int) (modelptr :pointer))
(cffi:defcfun ("save_model_JSON" xmm-save) :int (path :pointer) (modelptr :pointer))
(cffi:defcfun ("free_model" xmm-free) :void (modelptr :pointer))


;; (listen *terminal-io*)
;; (setq test (openxmm))
;; (closexmm test)