;; Lisp code for XMM / OM objects

(in-package :xmm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;         XMM OBJECT       ;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass xmmobj (om::om-cleanup-mixin)
  ((labls :accessor labls :initform nil :type :list)
   (dataset :accessor dataset :initform nil :initarg :dataset :type :list) ; DATASET = LIST de ((DATA1 LABEL1) (DATA2 LABEL2) ...)      
   (column-names :accessor column-names :initform nil :initarg :column-names :type :list) ;list of attributes
   (data-ptr :accessor data-ptr :initform nil) ; PTR on c++ Trainingset object 
   (model-ptr :accessor model-ptr :initform nil ) ; PTR on c++ HHMM object
   (errors :accessor errors :initform nil :type :list)
   (name :accessor name :initform (gensym "xmmobj"))
   (regularization :accessor regularization :initform (list 0.05 0.01) :initarg :regularization  :type list)
   (states :accessor states :initform '10 :initarg :states :type :int)
   ))


(defmethod om::om-cleanup ((self xmmobj))
  (when (model-ptr self)
    (om::om-print (format nil "deleting model of ~A (~A) [~A] and dataset [~A]" self (name self) (model-ptr self) (data-ptr self)) "GC")
    (if (and (not (fli::null-pointer-p (model-ptr self))) (not (fli::null-pointer-p (data-ptr self))))
        (xmm-free (model-ptr self) (data-ptr self)))
  ))

(defmethod initialize-instance ((self xmmobj) &rest initargs)
  (call-next-method)
  (setf (model-ptr self) (xmm-initmodel (first (regularization self)) (second (regularization self)) (states self)))
  self)

(defmethod om::om-init-instance ((self xmmobj) &optional args)
  (call-next-method)
  (if (dataset self)
      (progn 
        (if (not (column-names self)) (setf (column-names self) (make-list (length (caar (dataset self))) :initial-element 1)))
        (om::om-print "init and train......" "XMM")
        (setf (data-ptr self) (xmm-initdata (length (column-names self))))
        (fill_data self)
        (xmm-train (data-ptr self) (model-ptr self))
        (om::om-print ".... done training !" "XMM"))
    (om::om-print "missing some data to learn.." "XMM"))
  self)



;;test : outputs the accuracy for prediction on labelled data
;;fills errors list with number of error for each actual label
;;data is a list of samples
;;each sample is a list of 2 (desc-matrix , label) 
;;descriptor matrix is a list of descriptor vectors
(defmethod test((self xmmobj) data)
  (let ((accuracy 0)
        (num-samples (length data)))
    (setf (errors self) (make-list (length (labls self)) :initial-element 0))
    (loop for sample in data
          do (let* ((pred (run self (car sample)))
                   (real (cadr sample))
                   (pos (position real (labls self) :test #'equal)))
               ;(print (format nil "pred: ~A " pred))
               ;(print (format nil " actual: ~A " real))
               (if (not pos) (progn (om::om-print (format nil "Label ~a was not in training..." real) "XMM")  
                               (decf num-samples))
                 (if (string= pred real) 
                     (incf accuracy) 
                   (setf (nth pos (errors self)) (1+ (nth pos (errors self))))))))
               
    (om::om-print (format nil "Accuracy : ~d/~d" accuracy num-samples) "XMM")
    (/ accuracy num-samples)
))



;;run : output a predicted label for unlabelled data
;;data is a matrix of descriptors for 1 sample 
;;descriptor matrix is a list of descriptor vectors
;;reset : if nil : the model's passed results will be kept, influencing the next results. else, model's results will be reset
(defmethod run((self xmmobj) data &optional (reset 1))
  (let ((descr (fli:allocate-foreign-object :type :pointer :nelems (length data)))
        (size (length (car data)))
        (resultptr (fli:allocate-foreign-object :type :char :nelems 20))
        (likelihood 0)
        (cur)
        (i 0)
        (result (make-string 20)))
    (if (= 0 size) (om::om-print "Data size is null, frame might be too small" "XMM") 
      (if (not (column-names self)) (om::om-print "Please set column names to enable running" "XMM")
        (progn
          ;;Loop for each descriptor, and build data in pointer to send to xmm
          (loop for i from 0 to (1- (length data))  
                do (setf (fli:dereference descr :type :pointer :index i) 
                         (fli:allocate-foreign-object :type :float :nelems size :initial-contents (to-float (nth i data)))))
          (setf likelihood (xmm-run descr size (length (column-names self)) (model-ptr self) reset resultptr))
          ;fetch result from pointer
          (setf cur (fli:dereference resultptr :type :char :index 0))
          (loop until (char= cur #\0) do
                (progn 
                  (setf (char result i) cur)
                  (incf i)
                  (setf cur (fli:dereference resultptr :type :char :index i))))
          (setf result (subseq result 0 i))
        ;free pointer
          (loop for i from 0 to (1- (length data))
                do (fli:free-foreign-object (fli:dereference descr :type :pointer :index i)))
          (fli:free-foreign-object descr)
          (fli:free-foreign-object resultptr)
          ;(om::om-print (format nil "~a with ~f likelihood" result likelihood) "XMM")
          result))))
)



(defun to-float (list)
  (loop for elem in list collect
        (float elem)
))
(defun to-chars (str)
  (loop for i from 0 to (1- (length str)) collect
        (char str i)
))




;;Builds the xmm trainingset object with the attibute (dataset) and stores it in data-ptr
(defmethod fill_data((self xmmobj))
  (let* ((data (dataset self))
        (laabels (fli:allocate-foreign-object :type :pointer :nelems (length data)))
        (descrs (fli:allocate-foreign-object :type :pointer :nelems (length data)))
        (sizes (fli:allocate-foreign-object :type :int :nelems (length data))))       
    (if (null data) (return-from fill_data "empty data"))
    ;;store data in pointers to pass to xmm library
    ;;Loop for each sample
    (loop for j from 0 to (1- (length data))
          do (let ((size (length (car (car (nth j data))))))
               (setf (fli:dereference laabels :type :pointer :index j) 
                     (fli:allocate-foreign-object :type :char :nelems (length (cadr (nth j data))) :initial-contents  (to-chars (cadr (nth j data)))))
               ;maj list labels
               (if (not (find (cadr (nth j data)) (labls self) :test #'string-equal))  
                   (setf (labls self) (append (labls self) (cdr (nth j data)))))
               ;alloc
               (setf (fli:dereference descrs :type :pointer :index j) 
                       (fli:allocate-foreign-object :type :pointer :nelems (length (car (nth j data)))))
               (setf (fli:dereference sizes :type :int :index j) size)
               ;;Loop for each descriptor
               (loop for i from 0 to (1- (length (car (nth j data))))  
                     do(setf (fli:dereference (fli:dereference descrs :type :pointer :index j) :type :pointer :index i) 
                             (fli:allocate-foreign-object :type :float :nelems size :initial-contents (to-float (nth i (car (nth j data)))))))))
    ;send to xmm library
    (code-char (xmm-filldata descrs (length (dataset self)) sizes laabels (data-ptr self)))
    ;free pointers
    (fli:free-foreign-object sizes)
    (loop for i from 0 to (1- (length data))
          do (progn (loop for j from 0 to (1- (length (car (nth i data))))
                          do (fli:free-foreign-object (fli:dereference (fli:dereference descrs :type :pointer :index i) :type :pointer :index j)))
               (fli:free-foreign-object (fli:dereference descrs :type :pointer :index i))
               (fli:free-foreign-object (fli:dereference laabels :type :pointer :index i))))
    (fli:free-foreign-object descrs)
    (fli:free-foreign-object laabels)
))





(defmethod export-json((self xmmobj) path)
  (let ((path-ptr (fli:allocate-foreign-object :type :char :nelems (length path) :initial-contents (coerce path 'list))))
  (xmm-save path-ptr  (model-ptr self))
  (fli::free-foreign-object path-ptr)
  (om::om-print (format nil "Saved model at ~A " path) "XMM")
))



(defmethod import-json((self xmmobj) path)
  (when path
    (let* ((path-ptr (fli:allocate-foreign-object :type :char :nelems (length path) :initial-contents (coerce path 'list)))
           (lablptr (fli:allocate-foreign-object :type :pointer :nelems 30))   ;;; /!\ 30 labels maximum
           (size (xmm-import path-ptr (model-ptr self) lablptr))
           (temp (list))
           (id 1)
           (i 0)
           (ptr)
           (cur #\r))
      (setf (labls self) (list))
      ;read list of labels in lablptr
      (loop until (= id 0) do 
            (progn 
              (setf temp (list))
              (setf id 0)
              (setf ptr (fli:dereference lablptr :type :pointer :index i))
              (setf cur (fli:dereference ptr :type :char :index 0))
              (loop until (char= cur #\0) do
                    (progn
                      (setf temp (append temp (list cur)))
                      (setf id (1+ id))
                      (setf cur (fli::dereference ptr :type :char :index id))
                      ))
              (if temp
                  (setf (labls self) (append (labls self) (list (concatenate 'string temp)))))
              (setf i (1+ i))
              (fli:free-foreign-object ptr)
              ))
      (fli:free-foreign-object lablptr)
      (fli:free-foreign-object path-ptr)
size)))


(defmethod get-class-avrg((self xmmobj) label)
  (let* ((dimsize (length (column-names self)))
         (result (fli:allocate-foreign-object :type :pointer :nelems dimsize))
         (labelptr (fli:allocate-foreign-object :type :char :nelems (length label) :initial-contents (coerce label 'list))) 
         (size  (xmm-classavrg (data-ptr self) labelptr result))
         (ret (make-list dimsize))
         (ptr))
    (loop for dim from 0 to (1- dimsize) do
          (progn 
            (setf (nth dim ret)  (make-list size))
            (setf ptr (fli:dereference result :type :pointer :index dim))
            (loop for id from 0 to (1- size) do
                  (setf (nth id (nth dim ret)) (fli:dereference ptr :type :float :index id))
                  )
            (fli:free-foreign-object ptr)
            )
          )
    (fli:free-foreign-object labelptr)
    (fli:free-foreign-object  result)
    ret)
)



(defmethod get-labels((self xmmobj))
  (labls self)
)

(defmethod get-model((self xmmobj))
  (model-ptr self)
)

(defmethod get-columns((self xmmobj))
  (column-names self)
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
     (om::om-draw-string (+ x 10) (+ y 20) (concatenate 'string "Labels : " (format nil "~{~A~^,~}" (labls self))) :wrap (om::box-w box))))

    ;(om::om-with-font 
     ;(om::om-def-font :font1 :size 10)
     ;(om::om-draw-string (+ x 10) (+ y 20) (concatenate 'string "Dataset of " (length (dataset self)) " samples") :wrap (om::box-w box)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GENETIC ALGO FOR HYPER-PARAMETER OPTIMIZATION
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun testaccu (item1 item2)
  (if  item1
      (if (> (car (second item1)) (car (second item2)))
          item1
        item2)
   item2)
)


(defun find4best (results)
  (let ((ret (make-list 4))
        (mlist results))
    (loop for i from 0 to 3 do 
          (let ((max nil))
            (loop for item in mlist do 
                  (setf max (testaccu max item))
            )
            (setf mlist (remove max mlist))
            (setf (nth i ret) max)
          ))
ret
))

(defun reproduce (results) 
  (let* ((ret results)
        (parents (car (om:mat-trans ret))))
   
    (loop for parent in parents do 
          (setf ret 
                (append ret 
                        ;Create new child whith random variation on number of states and regularization
                        (list (list  (car parent) (+ (1- (random 3)) (second parent)) 
                                     (list (+ (/ (1- (random 3)) 200) (car (third parent))) (+ (/ (1- (random 3)) 1000) (cadr (third parent))))))
                        )
                ))
    ret)
)



(defun gene-algo (fun firstparams) 
  (let* ((params firstparams)
         (condition T)) 
    (loop while condition do
          (progn (setf params (reproduce
                        ;Get 4 best results (( "descr" 10 (0.05 0.01)) (0.3457 0.7575)) 
                               (print (find4best
                                ;;collect results
                                (loop for param in params collect 
                                      (if (= (length param) 3) 
                                          (funcall fun (print param))
                                        (print param))
                                )))
                       ))
            
            (if (< 0.9 (car (second (car params)))) (setf condition nil))
                       
               )))
  ) 

