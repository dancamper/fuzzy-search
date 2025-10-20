;;;; phonetics.lisp

(in-package #:net.bti.fuzzy-search)

;; -----------------------------------------------------------------------------

(defmethod double-metaphone ((obj string) &rest variation-opts)
  (declare (ignore variation-opts))
  (double-metaphone:double-metaphone obj))

(defmethod double-metaphone ((obj variation-class) &rest variation-opts)
  (add-derived obj (apply 'make-variation (double-metaphone (value obj)) "METAPHONE" variation-opts)))
