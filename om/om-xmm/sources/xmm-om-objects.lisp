;; Lisp code for XMM / OM objects

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         XMM OBJECT       ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :xmm)


;;; DATASET = LIST de ((DATA1 LABEL1) (DATA2 LABEL2) ...)
(defclass xmmobj (om::om-cleanup-mixin)
  ((dataset :accessor dataset :initform nil :initarg :dataset :type :list)
   (model-ptr :accessor model-ptr :initform nil)
   ))

(defmethod om::om-cleanup ((self xmmobj))
  (when (model-ptr self)
    (om::om-print (format nil "deleting model of ~A [~A]" self (model-ptr self)) "GC")
    (xmm::xmm-free (model-ptr self))
  ))

(defmethod initialize-instance ((self xmmobj) &rest initargs)
  (call-next-method)
  (setf (model-ptr self) (xmm-init))
  (print "ready to learn !")
  self)


(defmethod om::om-init-instance ((self xmmobj) &optional args)

  (call-next-method)
  
  ;;;; LEARN
  (learn self)
  self)



;;test : outputs the accuracy for prediction on labelled data
;;data is a list of samples
;;each sample is a list of 2 (desc-matrix , label) 
;;descriptor matrix is a list of descriptor vectors
(defmethod test((self xmmobj) data)
  (let ((accuracy 0)
        (num-samples (length data)))
    (loop for sample in data
          do (let ((pred (make-string 1 :initial-element (run self (car sample))))
                   (real (cadr sample)))
               (if (string= pred real) (setf accuracy (1+ accuracy)))
              (print "pred: " )
              (print pred )
              (print " actual: ")
              (print real)))
    (/ accuracy num-samples))
)

;;run : output a predicted label for unlabelled data
;;data is a matrix of descriptors for 1 sample 
;;descriptor matrix is a list of descriptor vectors

(defmethod run((self xmmobj) data)
  (let ((descr (fli:allocate-foreign-object :type :pointer :nelems (length data)))
        (size (length (car data))))
    ;;Loop for each descriptor
    (loop for i from 0 to (1- (length data))  
          do (setf (fli:dereference descr :type :pointer :index i) 
                   (fli:allocate-foreign-object :type :float :nelems size :initial-contents (nth i data))))
    (code-char (xmm-run descr size (model-ptr self))))
)



;;learn : trains the model with the dataset 
;;data is a list of samples. 
;;each sample is a list of size 2  (descriptor matrix, label)
;;descriptor matrix is a list of descriptor vectors

(defmethod learn ((self xmmobj))
  (let* ((data (dataset self))
        (laabels (fli:allocate-foreign-object :type :char :nelems (length data)))
        (descrs (fli:allocate-foreign-object :type :pointer :nelems (length data)))
        (sizes (fli:allocate-foreign-object :type :int :nelems (length data))))       
    (if (null data) (return-from learn "empty data"))
    ;;Loop for each sample
    (loop for j from 0 to (1- (length data))
          do (let ((size (length (car (car (nth j data))))))
                 (setf (fli:dereference laabels :type :char :index j) (char (cadr (nth j data)) 0))
                 (setf (fli:dereference descrs :type :pointer :index j) (fli:allocate-foreign-object :type :pointer :nelems (length (car (nth j data)))))
                 (setf (fli:dereference sizes :type :int :index j) size)
               ;;Loop for each descriptor
               (loop for i from 0 to (1- (length (car (nth j data))))  
                     do (setf (fli:dereference (fli:dereference descrs :type :pointer :index j) :type :pointer :index i) 
                              (fli:allocate-foreign-object :type :float :nelems size :initial-contents (nth i (car (nth j data))))))))
    
    
    (code-char (xmm-train descrs (length (dataset self)) sizes laabels (model-ptr self)))
))




