(in-package :xmm)

(cffi::defcfun ("opennXMM" opennXMM) :char)
(cffi::defcfun ("trainXMM" trainXMM) :int (descrs :pointer) (sample_num :int) (sample_sizes :pointer) (laabels :pointer))


;; (listen *terminal-io*)
;; (setq test (openxmm))
;; (closexmm test)