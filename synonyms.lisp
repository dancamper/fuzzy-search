;;;; synonyms.lisp

(in-package #:net.bti.fuzzy-search)

;;; ----------------------------------------------------------------------------

(defmethod apply-synonyms ((obj string) (synonyms list) &rest variation-opts)
  "Attempt to apply synonym substitution to the given OBJ value, which should be a string.
SYNONYMS can be either a flat list of strings or a list-of-lists of strings. Within a flat
list, all strings are considered synonyms of each other. Synonyms can be one or more words.
Synonyms must be normalized in the same manner as OBJ. Every occurrence of a synonym within
OBJ will be replaced (i.e. 'replace all').

Function returns a fresh list of any synonyms created. Each element of the list will be
a copy of OBJ with one synonym applied. nil will be returned if no synonyms were found."
  (declare (ignore variation-opts))
  (let ((result nil))
    (labels ((%process-list (synonym-list)
               (cond ((consp (car synonym-list))
                      (loop :for item :in synonym-list
                            :do (%process-list item)))
                     ((> (length synonym-list) 1)
                      (let* ((my-list (copy-list synonym-list)))
                        (dotimes (x (length my-list))
                          (let ((pattern (str:concat "\\b" (car my-list) "\\b")))
                            (loop :named replace-loop
                                  :for replacement :in (cdr my-list)
                                  :do (multiple-value-bind (new-text replacedp) (ppcre:regex-replace-all pattern obj replacement)
                                        (if replacedp
                                            (pushnew new-text result :test #'string-equal)
                                            (return-from replace-loop))))
                            (setf my-list (append (cdr my-list) (list (car my-list)))))))))))
      (%process-list synonyms))
    result))

(defmethod apply-synonyms ((obj variation-class) (synonyms list) &rest variation-opts)
  (let ((new-synonyms (apply-synonyms (value obj) synonyms))
        (new-objs nil))
    (loop :for s :in new-synonyms
          :do (let ((new-obj (add-derived obj (apply 'make-variation s nil (append (list :synonym t) variation-opts)))))
                (push new-obj new-objs)))
    new-objs))
