;;;; fuzzy-search.lisp

(in-package #:net.bti.fuzzy-search)

;;; ----------------------------------------------------------------------------

(defvar *test-results* nil)
(defvar *test-hash-results* nil)

;;; ------------------------------------

(defun normalize-variation (obj)
  "Return a new variation-class instance."
  (normalize-string obj :searchablep t))

(defun synonymize-variation (obj-or-list syn-list)
  (let ((result (copy-list (a:flatten (a:ensure-list obj-or-list)))))
    (loop :for obj :in (a:flatten (a:ensure-list obj-or-list))
          :do (a:appendf result (apply-synonyms obj syn-list :confidence 0.90 :searchablep t)))
    result))

(defun tokenize-variation (obj-or-list min-word-length)
  (let ((result nil))
    (loop :for obj :in (a:flatten (a:ensure-list obj-or-list))
          :do (let ((word-list (tokenize obj min-word-length :confidence 0.99 :searchablep t)))
                (push word-list result)))
    result))

(defun del-hood-variation (obj-or-list edit-distance min-word-length)
  (let ((result nil))
    (loop :for w :in (a:flatten (a:ensure-list obj-or-list))
          :do (let ((hood (create-deletion-neighborhood w edit-distance min-word-length :confidence 0.80 :searchablep t)))
                (push hood result)))
    result))

(defun metadata-less-p (m1 m2)
  (cond ((and (= (getf m1 :entity-id) (getf m2 :entity-id))
              (uiop:string-prefix-p (getf m1 :type) (getf m2 :type)))
         (< (getf m1 :confidence) (getf m2 :confidence)))
        ((< (getf m1 :entity-id) (getf m2 :entity-id))
         t)))

;;; ------------------------------------

(defclass base-field ()
  ((value :initarg :value :initform "" :reader value)
   (value-type :initarg :value-type :initform "" :reader value-type)
   (edit-distance :initarg :edit-distance :initform 1 :reader edit-distance)
   (min-word-length :initarg :min-word-length :initform 1 :reader min-word-length)
   (min-del-word-length :initarg :min-del-word-length :initform 2 :reader min-del-word-length)
   (synonym-list :initarg :synonym-list :initform nil :reader synonym-list)))

(defmethod process ((obj base-field))
  (let* ((value-obj (make-variation (value obj) (value-type obj)))
         ;; The rest of these modify value-obj or derived/embedded objects
         (normalized-value (normalize-variation value-obj))
         (synonyms (synonymize-variation normalized-value (synonym-list obj)))
         (word-list (tokenize-variation synonyms (min-word-length obj)))
         (hoods (del-hood-variation word-list (edit-distance obj) (min-del-word-length obj))))
    (declare (ignorable hoods))
    value-obj))

;;; ------------------------------------

(defclass full-name-field (base-field)
  ())

(defmethod initialize-instance :after ((obj full-name-field) &rest initargs)
  (declare (ignore initargs))
  (with-slots (synonym-list) obj
    (setf synonym-list +person-given-name-synonyms+)))

;;; ------------------------------------

(defclass street-addr-field (base-field)
  ())

(defmethod initialize-instance :after ((obj street-addr-field) &rest initargs)
  (declare (ignore initargs))
  (with-slots (synonym-list) obj
    (setf synonym-list +address-synonyms+)))

;;; ------------------------------------

(defun reset-index ()
  (setf *test-results* nil
        *test-hash-results* (make-hash-table)))

(defun test-index (entity-id string-value type)
  (unless *test-hash-results*
    (reset-index))
  (let* ((class-symbol (str:string-case type
                         ("FULL-NAME" 'full-name-field)
                         ("STREET-ADDR" 'street-addr-field)
                         (otherwise (error "~S is not a valid value type" type))))
         (full-name-obj (make-instance class-symbol :value string-value :value-type type))
         (value-obj (process full-name-obj)))
    (setf *test-results* value-obj)
    (let ((hash-entries (hash-searchable entity-id value-obj)))
      (loop :for item :in hash-entries
            :do (let ((key (hash-value item))
                      (value (metadata-for-index item)))
                  (push value (gethash key *test-hash-results*))))
      (pretty-print-object value-obj *standard-output*)
      (format *standard-output* "~D hash entries created~%" (length hash-entries)))))
