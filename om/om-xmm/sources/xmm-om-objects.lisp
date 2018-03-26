;; Lisp code for XMM / OM objects

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         XMM OBJECT       ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :xmm)


;;; DATASET = LIST de ((DATA1 LABEL1) (DATA2 LABEL2) ...)
(defclass xmmobj (om::om-cleanup-mixin)
  ((dataset :accessor dataset :form nil :initarg :dataset)
   (model-ptr :accessor model-ptr :initform nil)
   ))

(defmethod om::om-cleanup ((self xmmobj))
  (when (model-ptr self)
    (om::om-print (format nil "deleting model of ~A [~A]" self (model-ptr self)) "GC")
    (xmm-free (model-ptr self))
  ))

(defmethod initialize-instance ((self xmmobj) &rest initargs)
  (call-next-method)
  (setf (model-ptr self) (xmm-init))
  (print "ready to learn !")
  self)


(defmethod om::om-init-instance ((self xmmobj) &optional args)

  (call-next-method)
  (print (dataset self))
  
  ;;;; LEARN
  (learn self (dataset self))

  self)




;;run-model(list) :
;;list is a matrix of descriptors for 1 sample 
;;descriptor matrix is a list of descriptor vectors

(defmethod run-model ((self xmmobj) data)
  (let ((descr (fli:allocate-foreign-object :type :pointer :nelems (length list)))
        (size (length (car list))))
    ;;Loop for each descriptor
    (loop for i from 0 to (1- (length list))  
          do (setf (fli:dereference descr :type :pointer :index i) 
                   (fli:allocate-foreign-object :type :float :nelems size :initial-contents (nth i list))))
    (code-char (xmm-run descr (length (car list)) (model-ptr self))))
)



;;learn(list) :
;;list is a list of samples. 
;;each sample is a list of size 2 : the descriptor matrix, and the label
;;descriptor matrix is a list of descriptor vectors
;;each descriptor vector is a list of values 

(defmethod learn ((self xmmobj) list)
  (let ((laabels (fli:allocate-foreign-object :type :char :nelems (length list)))
        (descrs (fli:allocate-foreign-object :type :pointer :nelems (length list)))
        (sizes (fli:allocate-foreign-object :type :int :nelems (length list))))       

    ;;Loop for each sample
    (loop for j from 0 to (1- (length list))
          do (let ((size (length (car (car (nth j list))))))
                 (setf (fli:dereference laabels :type :char :index j) (char (cadr (nth j list)) 0))
                 (setf (fli:dereference descrs :type :pointer :index j) (fli:allocate-foreign-object :type :pointer :nelems (length (car (nth j list)))))
                 (setf (fli:dereference sizes :type :int :index j) size)
               ;;Loop for each descriptor
               (loop for i from 0 to (1- (length (car (nth j list))))  
                     do (setf (fli:dereference (fli:dereference descrs :type :pointer :index j) :type :pointer :index i) 
                              (fli:allocate-foreign-object :type :float :nelems size :initial-contents (loop for x in (nth i (car (nth j list))) do (coerce x 'single-float)))))))
    
    
    (code-char (xmm-train descrs (length list) sizes laabels (model-ptr self)))
;;FREE MEMORY
))




