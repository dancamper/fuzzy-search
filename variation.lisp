;;;; variation.lisp

(in-package #:net.bti.fuzzy-search)

;;; ----------------------------------------------------------------------------

(defclass variation-class ()
  ((value :initarg :value :initform nil :reader value)
   (type :initarg :type :initform nil :accessor variation-type)
   (metadata :initform nil :accessor metadata)
   (searchablep :initarg :searchablep :initform nil :accessor searchablep)
   (confidence :initarg :confidence :initform 1.0 :accessor confidence :type short-float)
   (parent :initarg :parent :initform nil :accessor parent)
   (derived :initform nil :accessor derived)))

(defmethod pretty-print-object ((obj variation-class) stream)
  (labels ((print-metadata-to-string (data)
             (with-output-to-string (s)
               (format s "(")
               (loop :for (k v) :on data :by #'cddr
                     :for first-inner = t :then nil
                     :do (unless first-inner (format s " "))
                         (format s "~S ~S" k v))
               (format s ")")))
           (print-tree (node indent)
             (with-slots (value searchablep derived) node
               ;; indent, then dash + class + info
               (format stream "~&~V@T- ~A (~S searchable=~S e-confidence=~S e-metadata={~S})~%"
                       indent (class-name (class-of node))
                       value
                       (truthify searchablep)
                       (effective-confidence node)
                       (print-metadata-to-string (effective-metadata node)))
               ;; recurse into derived children
               (dolist (child derived)
                 (print-tree child (+ indent 4))))))
    (print-tree obj 0)))

(defmethod effective-confidence ((obj variation-class))
  (* (confidence obj)
     (if (parent obj)
         (effective-confidence (parent obj))
         1.0)))

(defmethod effective-metadata ((obj variation-class))
  (append (list :type (variation-type obj)) (metadata obj)))

(defmethod add-metadata ((obj variation-class) &rest args &key &allow-other-keys)
  (loop :for (k v) :on args :by #'cddr
        :do (case k
              (:searchablep (setf (searchablep obj) v))
              (:confidence (setf (confidence obj) v))
              (otherwise (setf (metadata obj) (plist-insert k v (metadata obj))))))
  obj)

(defmethod copy-metadata ((source variation-class) (target variation-class))
  (setf (metadata target) (merge-plists (metadata source) (metadata target))))

(defmethod has-metadata-p ((obj variation-class) key &key (value-test #'identity))
  (let ((existing (assoc key (metadata obj))))
    (and existing (funcall value-test (cdr existing)))))

(defmethod add-derived ((parent-obj variation-class) (obj variation-class))
  (setf (parent obj) parent-obj
        (metadata obj) (append (metadata obj) (metadata parent-obj)))
  (if (variation-type obj)
      (setf (variation-type obj) (str:concat (variation-type parent-obj) "/" (variation-type obj)))
      (setf (variation-type obj) (variation-type parent-obj)))
  (a:appendf (derived parent-obj) (list obj))
  obj)

(defmethod gather-by-type ((obj variation-class) type)
  "Examine the given object and all derived objects, return a new flat
list containing only those objects that exactly match the given type."
  (let ((result nil))
    (when (eql type (variation-type obj))
      (push obj result))
    (loop :for item :in (derived obj)
          :do (setf result (append result (gather-by-type item type))))
    result))

(defmethod gather-by-searchable ((obj variation-class))
  "Examine the given object and all derived objects, return a new flat
list containing only those objects that are searchable."
  (let ((result nil))
    (when (searchablep obj)
      (push obj result))
    (loop :for item :in (derived obj)
          :do (a:appendf result (gather-by-searchable item)))
    result))

(defmethod hash-searchable ((entity-id fixnum) (obj variation-class))
  (let* ((result nil)
         (searchable (gather-by-searchable obj))
         (seen (make-hash-table :size (length searchable) :test #'equalp)))
    ;; Deduplicate based on value; if there is a collision, keep the one with
    ;; the highest effective confidence
    (loop :for item :in searchable
          :do (let ((found-obj (gethash (value item) seen)))
                (if found-obj
                    (when (> (effective-confidence item) (effective-confidence found-obj))
                      (setf (gethash (value item) seen) item))
                    (setf (gethash (value item) seen) item))))
    (loop :for v :being :the :hash-values :in seen
          :do (let ((hash-obj (make-instance 'variation-hash-class
                                             :hash (hash-string (value v))
                                             :id entity-id
                                             :confidence (effective-confidence v)
                                             :metadata (effective-metadata v))))
                (push hash-obj result)))
    result))

;;; ------------------------------------

(defclass variation-hash-class ()
  ((hash-value :initarg :hash :reader hash-value :type fixnum)
   (entity-id :initarg :id :reader entity-id :type fixnum)
   (confidence :initarg :confidence :reader confidence :type short-float)
   (metadata :initarg :metadata :reader metadata)))

(defmethod metadata-for-index ((obj variation-hash-class))
  (append (list :entity-id (entity-id obj) :confidence (confidence obj)) (metadata obj)))

;;; ------------------------------------

(defun make-variation (v type &rest metadata &key (searchablep nil) &allow-other-keys)
  "Create and return an instance of variation-class referencing V as its value and TYPE as its variation type.
Add all matching key/value argument pairs to the new instance as metadata before returning it."
  (let ((obj (make-instance 'variation-class :value v :type type :searchablep searchablep)))
    (apply 'add-metadata obj metadata)
    obj))

