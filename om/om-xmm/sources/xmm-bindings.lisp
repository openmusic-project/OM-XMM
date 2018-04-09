(in-package :xmm)

(cffi::defcfun ("initDataset" xmm-initData) :pointer (numcolumns :int))
(cffi::defcfun ("fillDataset" xmm-filldata) :int (descrs :pointer) (sample_num :int) (sample_sizes :pointer) (laabels :pointer) (dataptr :pointer))
(cffi::defcfun ("trainXMM" xmm-train) :int (dataptr :pointer) (modelptr :pointer))
(cffi::defcfun ("runXMM" xmm-run) :int (descrs :pointer) (sample_size :int) (columnum :int) (modelptr :pointer) (reset :boolean))
(cffi::defcfun ("initXMM" xmm-initmodel) :pointer)
(cffi::defcfun ("save_model_JSON" xmm-save) :int (path :pointer) (modelptr :pointer))
(cffi::defcfun ("importJson" xmm-import) :pointer (path :pointer) (model-ptr :pointer) (lablptr :pointer))
(cffi::defcfun ("free_model" xmm-free) :void (modelptr :pointer) (dataptr :pointer))


;; (listen *terminal-io*)
;; (setq test (openxmm))
;; (closexmm test)