;;;; normalization.lisp

(in-package #:net.bti.fuzzy-search)

;;; ----------------------------------------------------------------------------

(defmethod normalize-string ((obj string) &rest variation-opts)
  (declare (ignore variation-opts))
  (let ((fn (a:compose 'str:upcase
                       'str:trim
                       'str:collapse-whitespaces
                       'str:remove-punctuation)))
    (funcall fn obj)))

(defmethod normalize-string ((obj variation-class) &rest variation-opts)
  (let* ((new-value (normalize-string (value obj)))
         (conf (if (string-equal new-value (value obj)) 1.0 0.95))
         (new-obj (apply 'make-variation new-value "NORMALIZED" :confidence conf variation-opts)))
    (add-derived obj new-obj)
    new-obj))

;;; ------------------------------------

(defmethod split-words ((obj string) min-word-length &rest variation-opts)
  (declare (ignore variation-opts))
  (remove-if (lambda (w) (< (length w) min-word-length)) (str:words obj)))

(defmethod split-words ((obj variation-class) min-word-length &rest variation-opts)
  (let* ((s (split-words (value obj) min-word-length))
         (end (length s))
         (new-word-objs nil))
    (loop :for n :from 1 :upto end
          :do (let ((word-obj (add-derived obj (apply 'make-variation (nth (1- n) s) (format nil "WORD/~3,'0d" n)
                                                      :num-words end variation-opts))))
                (push word-obj new-word-objs)))
    (reverse new-word-objs)))
