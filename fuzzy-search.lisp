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
          :do (let ((word-list (split-words obj min-word-length :confidence 0.99 :searchablep t)))
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

(defun reset-index ()
  (setf *test-results* nil
        *test-hash-results* (make-hash-table)))

(defun test-index (entity-id string-value type)
  (unless *test-hash-results*
    (reset-index))
  (let ((edit-distance 1)
        (min-word-length 1)
        (min-del-word-length 2))
    (let* ((value-obj (make-variation string-value type))
           (normalized-value (normalize-variation value-obj))
           (synonyms (synonymize-variation normalized-value +person-given-name-synonyms+))
           (word-list (tokenize-variation synonyms min-word-length))
           (hoods (del-hood-variation word-list edit-distance min-del-word-length)))
      (declare (ignorable hoods))
      ;; Set temp/test globals
      (setf *test-results* value-obj)
      (let ((hash-entries (hash-searchable entity-id value-obj)))
        (loop :for item :in hash-entries
              :do (let ((key (hash-value item))
                        (value (metadata-for-index item)))
                    (push value (gethash key *test-hash-results*))))
        (pretty-print-object value-obj *standard-output*)
        (format *standard-output* "~D hash entries created~%" (length hash-entries))))))
