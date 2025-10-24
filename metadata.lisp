;;;; metadata.lisp

(in-package #:net.bti.fuzzy-search)

;;; ----------------------------------------------------------------------------

(defclass metadata ()
  ((h :initform (make-hash-table) :accessor h)))

(defmethod copy-of ((obj metadata))
  (let ((m (make-instance 'metadata)))
    (setf (h m) (a:copy-hash-table (h obj)))
    m))

(defmethod pretty-print-object ((obj metadata) stream)
  (format stream "(")
  (loop :for k :being :the :hash-keys :in (h obj) :using (:hash-value v)
        :for first = t :then nil
        :do (progn
              (unless first
                (format stream " "))
              (format stream "~S ~S" k v)))
  (format stream ")"))

(defun recreate-metadata (index-data)
  (let ((obj (make-instance 'metadata)))
    (loop :for (k v) :on index-data :by #'cddr
          :do (setf (gethash k (h obj)) v))
    obj))

(defun make-metadata (&rest args)
  (recreate-metadata args))

(defmethod metadata-for-index ((obj metadata))
  (a:hash-table-plist (h obj)))

;;; ------------------------------------

(defmethod has-kv ((obj metadata) key)
  (multiple-value-bind (v has-p) (gethash key (h obj))
    (declare (ignore v))
    has-p))

(defmethod get-kv ((obj metadata) key &optional default-value)
  (gethash key (h obj) default-value))

(defmethod set-kv ((obj metadata) key value)
  (setf (gethash key (h obj)) value))

;;; ----------------

(defmethod get-entity-id ((obj metadata) &optional default-value)
  (get-kv obj :entity-id default-value))

(defmethod set-entity-id ((obj metadata) (new-entity-id fixnum))
  (set-kv obj :entity-id new-entity-id))

(defmethod get-type ((obj metadata) &optional default-value)
  (get-kv obj :type default-value))

(defmethod set-type ((obj metadata) (new-type string))
  (set-kv obj :type new-type))

(defmethod append-type ((obj metadata) (suffix string))
  (multiple-value-bind (old present-p) (get-type obj)
    (if (and old present-p)
        (set-type obj (str:concat old "/" suffix))
        (set-type obj suffix))))

(defmethod prepend-type ((obj metadata) (prefix string))
  (multiple-value-bind (old present-p) (get-type obj)
    (if (and old present-p)
        (set-type obj (str:concat prefix "/" old))
        (set-type obj prefix))))

(defmethod type-depth ((obj metadata))
  (count #\/ (get-type obj)))

(defmethod get-confidence ((obj metadata) &optional default-value)
  (get-kv obj :confidence default-value))

(defmethod set-confidence ((obj metadata) (new-confidence float))
  (set-kv obj :confidence new-confidence))

(defmethod apply-confidence ((obj metadata) (parent-confidence float))
  (set-confidence obj (* (get-confidence obj 1.0) parent-confidence)))

(defmethod apply-confidence ((obj metadata) (parent-obj metadata))
  (set-confidence obj (* (get-confidence obj 1.0) (get-confidence parent-obj 1.0))))

(defmethod get-depth ((obj metadata))
  (count #\/ (get-type obj)))

;;; ------------------------------------

(defmethod inherit-from ((child-obj metadata) (parent-obj metadata))
  (prepend-type child-obj (get-type parent-obj))
  (apply-confidence child-obj parent-obj)
  ;; Propagate unique metadata from parent to child
  (loop :for k :being :the :hash-keys :in (h parent-obj) :using (:hash-value v)
        :do (multiple-value-bind (child-value foundp) (get-kv child-obj k)
              (declare (ignore child-value))
              (unless foundp
                (set-kv child-obj k v))))
  child-obj)

;;; ------------------------------------

(defmethod metadata-less-p ((obj1 metadata) (obj2 metadata))
  (let ((obj1-type (get-type obj1))
        (obj2-type (get-type obj2))
        (obj1-query-word-id (get-kv obj1 :query-word-id 0))
        (obj2-query-word-id (get-kv obj2 :query-word-id 0)))
    (cond ((< obj1-query-word-id obj2-query-word-id)
           t)
          ((> obj1-query-word-id obj2-query-word-id)
           nil)
          ((string= obj1-type obj2-type)
           (< (get-confidence obj1) (get-confidence obj2)))
          ((uiop:string-prefix-p obj1-type obj2-type)
           t)
          (t
           (string< obj1-type obj2-type)))))

(defmethod reduce-metadata-list ((sorted list))
  (loop :with current = (first sorted)
        :with current-type = (get-type current)
        :with current-query-word-id = (get-kv current :query-word-id 0)
        :for next :in (rest sorted)
        :for next-type = (get-type next)
        :for next-query-word-id = (get-kv next :query-word-id 0)
        :if (and (or (zerop current-query-word-id)
                     (zerop next-query-word-id)
                     (= current-query-word-id next-query-word-id))
                 (or (uiop:string-prefix-p next-type current-type)
                     (uiop:string-prefix-p current-type next-type)))
          :do (when (> (get-confidence next) (get-confidence current))
                (setf current next))
        :else
          :collect current :into result
          :and :do (setf current-type next-type
                         current-query-word-id next-query-word-id
                         current next)
        :finally (return (append result (list current)))))

(defmethod merge-metadata ((obj1 metadata) (obj2 metadata))
  (let ((result (copy-of obj2)))
    (loop :for k :being :the :hash-keys :in (h obj1) :using (:hash-value v)
          :do (cond ((eql k :confidence)
                     (apply-confidence result v))
                    ((eql k :edit-distance)
                     (set-kv result k (max (get-kv result :edit-distance 0) v)))
                    ((not (get-kv result k))
                     (set-kv result k v))))
    result))

(defvar word-num-scanner nil)
(defvar field-name-scanner nil)

(defmethod fixup-metadata ((obj metadata))
  (unless word-num-scanner
    (setf word-num-scanner (ppcre:create-scanner "WORD/(\\d+)")))
  (unless field-name-scanner
    (setf field-name-scanner (ppcre:create-scanner "^[^/]+")))
  (let* ((m (copy-of obj))
         (type-str (get-type m)))
    (multiple-value-bind (match-start match-end capture-starts capture-ends) (ppcre:scan word-num-scanner type-str)
      (declare (ignore match-end))
      (when match-start
        (let* ((s (aref capture-starts 0))
               (e (aref capture-ends 0))
               (num (parse-integer type-str :start s :end e)))
          (set-kv m :word-pos num)
          (set-type m (str:concat (str:substring 0 (1- s) type-str) (str:substring e (length type-str) type-str)))
          type-str (get-type m))))
    (multiple-value-bind (match-start match-end) (ppcre:scan field-name-scanner type-str)
      (when match-start
        (set-kv m :field (str:substring match-start match-end type-str))
        (set-type m (str:substring (1+ match-end) (length type-str) type-str))))
    m))

(defmethod describe-metadata ((obj metadata))
  (let ((descrip (str:concat (get-kv obj :field) ": "))
        (synonym-p (get-kv obj :synonym))
        (type-str (get-type obj)))
    (cond ((str:containsp "/WORD" type-str)
           ;; match of an individual word
           (let ((match-type nil)
                 (word-type (if synonym-p "synonym of word" "word")))
             (cond ((str:containsp "/DEL-HOOD" type-str)
                    (setf match-type (format nil "fuzzy match[~D]" (get-kv obj :edit-distance))))
                   ((str:containsp "/METAPHONE" type-str)
                    (setf match-type "phonetic match"))
                   (t
                    (setf match-type "match")))
             (setf descrip (str:concat descrip
                                       match-type
                                       (format nil " on ~A ~D out of ~D words" word-type (get-kv obj :word-pos) (get-kv obj :num-words))))))
          ((string= "NORMALIZED" type-str)
           (let ((match-type (if synonym-p "synonym of entire entry" "entire entry")))
             (setf descrip (str:concat descrip (format nil "match on ~A" match-type)))))
          (t
           (setf descrip (format nil "~S" (a:hash-table-plist (h obj))))))
    descrip))

