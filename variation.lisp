;;;; variation.lisp

(in-package #:net.bti.fuzzy-search)

;;; ----------------------------------------------------------------------------

(defclass variation-class ()
  ((value :initarg :value :initform nil :reader value)
   (meta :initform (make-metadata) :accessor meta)
   (searchablep :initarg :searchablep :initform nil :accessor searchablep)
   (parent :initarg :parent :initform nil :accessor parent)
   (derived :initform nil :accessor derived)))

(defmethod variation-type ((obj variation-class))
  (get-type (meta obj)))

(defmethod pretty-print-object ((obj variation-class) stream)
  (labels ((print-tree (node indent)
             (with-slots (value searchablep derived) node
               ;; indent, then dash + class + info
               (format stream "~&~V@T- ~A (~S searchable=~S "
                       indent (class-name (class-of node))
                       value
                       (truthify searchablep))
               (pretty-print-object (meta node) stream)
               (format stream ")~%")
               ;; recurse into derived children
               (dolist (child derived)
                 (print-tree child (+ indent 4))))))
    (print-tree obj 0)))

(defmethod add-metadata ((obj variation-class) &rest args)
  (loop :for (k v) :on args :by #'cddr
        :do (case k
              (:searchablep (setf (searchablep obj) v))
              (otherwise (set-kv (meta obj) k v))))
  obj)

(defmethod add-derived ((parent-obj variation-class) (obj variation-class))
  "Adds OBJ as a child/derived object to PARENT-OBJ. Metadata in child is adjusted
to include some of parent's metadata."
  (inherit-from (meta obj) (meta parent-obj))
  (setf (parent obj) parent-obj)
  (a:appendf (derived parent-obj) (list obj))
  obj)

(defmethod get-confidence ((obj variation-class) &optional default-value)
  (get-confidence (meta obj) default-value))

(defmethod gather-by-type ((obj variation-class) type)
  "Examine the given object and all derived objects, return a new flat
list containing only those objects that exactly match the given type."
  (let ((result nil))
    (when (eql type (get-type (meta obj)))
      (push obj result))
    (loop :for item :in (derived obj)
          :do (a:appendf result (gather-by-type item type)))
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
    ;; the highest confidence
    (loop :for item :in searchable
          :do (let ((found-obj (gethash (value item) seen)))
                (if found-obj
                    (when (> (get-confidence item) (get-confidence found-obj))
                      (setf (gethash (value item) seen) item))
                    (setf (gethash (value item) seen) item))))
    (loop :for v :being :the :hash-values :in seen
          :do (let ((hash-obj (make-instance 'variation-hash-class
                                             :hash (hash-string (value v))
                                             :id entity-id
                                             :metadata (meta v))))
                (push hash-obj result)))
    result))

;;; ------------------------------------

(defclass variation-hash-class ()
  ((hashed-value :initarg :hash :reader hashed-value :type fixnum)
   (entity-id :initarg :id :reader entity-id :type fixnum)
   (meta :initarg :metadata :reader meta)))

(defmethod metadata-for-index ((obj variation-hash-class))
  (append (list :entity-id (entity-id obj)) (metadata-for-index (meta obj))))

;;; ------------------------------------

(defun make-variation (v type &rest metadata &key (searchablep nil) &allow-other-keys)
  "Create and return an instance of variation-class referencing V as its value and TYPE as its variation type.
Add all matching key/value argument pairs to the new instance as metadata before returning it."
  (let ((obj (make-instance 'variation-class :value v :searchablep searchablep)))
    (apply 'add-metadata obj (append (list :type type) metadata))
    obj))

