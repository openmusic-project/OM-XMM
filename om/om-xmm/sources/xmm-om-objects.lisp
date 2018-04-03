;; Lisp code for XMM / OM objects

(in-package :xmm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         XMM OBJECT       ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; DATASET = LIST de ((DATA1 LABEL1) (DATA2 LABEL2) ...)
(defclass xmmobj (om::om-cleanup-mixin)
  ((labls :accessor labls :initform nil :type :list)
   (dataset :accessor dataset :initform nil :initarg :dataset :type :list) ;;;;;;list like : ( ((descr-matrix1) , (label1)) , ((descr-matrix2) , (label2)), ...)
   (column-names :accessor columns :initform nil :initarg :column-names :type :list)
   (data-ptr :accessor data-ptr :initform nil) ; PTR on c++ Trainingset object 
   (model-ptr :accessor model-ptr :initform nil )
   (errors :accessor errors :initform nil :type :list)
   ))


(defmethod om::om-cleanup ((self xmmobj))
  (when (model-ptr self)
    (om::om-print (format nil "deleting model of ~A [~A] and dataset [~A]" self (model-ptr self) (data-ptr self)) "GC")
    (if (and (not (fli::null-pointer-p (model-ptr self))) (not (fli::null-pointer-p (data-ptr self))))
        (xmm-free (model-ptr self) (data-ptr self)))
  ))

(defmethod initialize-instance ((self xmmobj) &rest initargs)
  (call-next-method)
  (setf (model-ptr self) (xmm-initmodel))
  self)

(defmethod om::om-init-instance ((self xmmobj) &optional args)
  (call-next-method)
  (if (and (columns self) (dataset self))
      (progn 
        (om::om-print "init and train......" "XMM")
        (setf (data-ptr self) (xmm-initdata (length (columns self))))
        (fill_data self)
        (xmm-train (data-ptr self) (model-ptr self))
        (om::om-print ".... done training !" "XMM"))
    (om::om-print "missing some argument to learn.." "XMM"))
  self)



;;test : outputs the accuracy for prediction on labelled data
;;data is a list of samples
;;each sample is a list of 2 (desc-matrix , label) 
;;descriptor matrix is a list of descriptor vectors
(defmethod test((self xmmobj) data)
  (let ((accuracy 0)
        (num-samples (length data)))
    (setf (errors self) (make-list (length (labls self)) :initial-element 0))
    (loop for sample in data
          do (let* ((pred (make-string 1 :initial-element (run self (car sample))))
                   (real (cadr sample))
                   (pos (position real (labls self) :test #'equal)))
               ;(print (format nil "pred: ~A " pred ))
               ;(print (format nil " actual: ~A ~d" real pos))
               (if (not pos) (om::om-print (format nil "Label ~a was not in training..." real) "XMM")  
                 (if (string= pred real) 
                     (setf accuracy (1+ accuracy)) 
                   (setf (nth pos (errors self)) (1+ (nth pos (errors self))))))))
               
    (om::om-print (format nil "Accuracy : ~d/~d" accuracy num-samples) "XMM")
))

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
    (code-char (xmm-run descr size (length (columns self)) (model-ptr self))))
)



;;learn : trains the model with the dataset 
;;data is a list of samples. 
;;each sample is a list of size 2  (descriptor matrix, label)
;;descriptor matrix is a list of descriptor vectors

(defmethod fill_data((self xmmobj))
  (let* ((data (dataset self))
        (laabels (fli:allocate-foreign-object :type :char :nelems (length data)))
        (descrs (fli:allocate-foreign-object :type :pointer :nelems (length data)))
        (sizes (fli:allocate-foreign-object :type :int :nelems (length data))))       
    (if (null data) (return-from fill_data "empty data"))
    ;;Loop for each sample
    (loop for j from 0 to (1- (length data))
          do (let ((size (length (car (car (nth j data))))))
                 (setf (fli:dereference laabels :type :char :index j) (char (cadr (nth j data)) 0))
                 ;maj list labels
                 (if (not (find (cadr (nth j data)) (labls self) :test #'equal))  
                     (setf (labls self) (append (labls self) (list (cadr (nth j data))))))
                 (setf (fli:dereference descrs :type :pointer :index j) 
                       (fli:allocate-foreign-object :type :pointer :nelems (length (car (nth j data)))))
                 (setf (fli:dereference sizes :type :int :index j) size)
               ;;Loop for each descriptor
               (loop for i from 0 to (1- (length (car (nth j data))))  
                     do(setf (fli:dereference (fli:dereference descrs :type :pointer :index j) :type :pointer :index i) 
                             (fli:allocate-foreign-object :type :float :nelems size :initial-contents (nth i (car (nth j data))))))))
    (code-char (xmm-filldata descrs (length (dataset self)) sizes laabels (data-ptr self)))
    ;free pointers
    (fli:free-foreign-object laabels)
    (fli:free-foreign-object sizes)
    (loop for i from 0 to (1- (length data))
          do (progn (loop for j from 0 to (1- (length (car (nth i data))))
                          do (fli:free-foreign-object (fli:dereference (fli:dereference descrs :type :pointer :index i) :type :pointer :index j)))
               (fli:free-foreign-object (fli:dereference descrs :type :pointer :index i))))
    (fli:free-foreign-object descrs)
))

(defmethod export-json((self xmmobj) path)
  (let ((path-ptr (fli:allocate-foreign-object :type :char :nelems (length path) :initial-contents (coerce path 'list))))
  (xmm-save path-ptr (model-ptr self))
  (fli::free-foreign-object path-ptr))
)

(defmethod import-json((self xmmobj) path)
  (let* ((path-ptr (fli:allocate-foreign-object :type :char :nelems (length path) :initial-contents (coerce path 'list)))
         (labptr (xmm-import path-ptr (model-ptr self)))
         (cur (fli::dereference labptr :type :char :index 0))
         (id 0))
    (setf (labls self) (list))
    (loop until (char= cur #\0 ) do
          (progn (setf (labls self) (append (labls self) (list cur)))
            (setf id (1+ id))
            (setf cur (fli::dereference labptr :type :char :index id))))
   ; (xmm-free labptr nil)
    (fli:free-foreign-object path-ptr)
)) 

(defmethod get-labels((self xmmobj))
  (labls self)
)

(defmethod get-model((self xmmobj))
  (model-ptr self)
)

(defmethod get-columns((self xmmobj))
  (columns self)
)

(defmethod get-errors((self xmmobj))
  (loop for i from 0 to (1- (length (labls self)))
       do (print (format nil "~A ~d" (nth i (labls self)) (nth i (errors self))))
))

;;; VIEW 

(defmethod om::display-modes-for-object ((self xmmobj))
  '(:hidden :text :mini-view))


(defmethod om::draw-mini-view ((self xmmobj) (box t) x y w h &optional time)
    (om::om-with-font 
     (om::om-def-font :font1 :size 10)
     (om::om-draw-string (+ x 10) (+ y 20) (concatenate 'string "Labels : " (format nil "~{~a~} " (labls self))) :wrap (om::box-w box))))

    ;(om::om-with-font 
     ;(om::om-def-font :font1 :size 10)
     ;(om::om-draw-string (+ x 10) (+ y 20) (concatenate 'string "Dataset of " (length (dataset self)) " samples") :wrap (om::box-w box)))))






