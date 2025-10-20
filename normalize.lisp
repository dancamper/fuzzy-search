;;;; normalization.lisp

(in-package #:net.bti.fuzzy-search)

;;; ----------------------------------------------------------------------------

(defun remove-accents (string)
  "Normalize STRING to plain ASCII. Characters with diacritics are
replaced with ASCII equivalents or dropped."
  (let ((accent-map '(("Á" . "A") ("À" . "A") ("Â" . "A") ("Ã" . "A") ("Ä" . "A")
                      ("á" . "a") ("à" . "a") ("â" . "a") ("ã" . "a") ("ä" . "a")
                      ("É" . "E") ("È" . "E") ("Ê" . "E") ("Ë" . "E")
                      ("é" . "e") ("è" . "e") ("ê" . "e") ("ë" . "e")
                      ("Í" . "I") ("Ì" . "I") ("Î" . "I") ("Ï" . "I")
                      ("í" . "i") ("ì" . "i") ("î" . "i") ("ï" . "i")
                      ("Ó" . "O") ("Ò" . "O") ("Ô" . "O") ("Õ" . "O") ("Ö" . "O")
                      ("ó" . "o") ("ò" . "o") ("ô" . "o") ("õ" . "o") ("ö" . "o")
                      ("Ú" . "U") ("Ù" . "U") ("Û" . "U") ("Ü" . "U")
                      ("ú" . "u") ("ù" . "u") ("û" . "u") ("ü" . "u")
                      ("Ñ" . "N") ("ñ" . "n")
                      ("Ç" . "C") ("ç" . "c")
                      ("ß" . "ss")
                      ("Ø" . "O") ("ø" . "o")
                      ("Ł" . "L") ("ł" . "l"))))
    (with-output-to-string (out)
      (loop :for ch :across string
            :for s = (string ch)
            :do (princ (or (cdr (assoc s accent-map :test #'string=))
                           (if (<= (char-code ch) 127) s "")) out)))))

;; -------------------------------------

(defmethod normalize-string ((obj string) &rest variation-opts)
  (declare (ignore variation-opts))
  (let ((fn (a:compose 'str:upcase
                       'str:trim
                       'remove-accents
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

(defmethod tokenize ((obj string) min-word-length &rest variation-opts)
  (declare (ignore variation-opts))
  (remove-if (lambda (w) (< (length w) min-word-length)) (str:words obj)))

(defmethod tokenize ((obj variation-class) min-word-length &rest variation-opts)
  (let* ((s (tokenize (value obj) min-word-length))
         (end (length s))
         (new-word-objs nil)
         (my-variation-opts (copy-list variation-opts))
         (new-confidence (/ (getf my-variation-opts :confidence 1.0) end)))
    (setf (getf my-variation-opts :confidence) new-confidence)
    (loop :for n :from 1 :upto end
          :do (let ((word-obj (add-derived obj (apply 'make-variation (nth (1- n) s) (format nil "WORD/~3,'0d" n)
                                                      :num-words end
                                                      my-variation-opts))))
                (push word-obj new-word-objs)))
    (reverse new-word-objs)))
