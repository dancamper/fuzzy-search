;;;; deletion-neighborhood.lisp

(in-package #:net.bti.fuzzy-search)

;;; ----------------------------------------------------------------------------

(defmethod adaptive-edit-distance ((src sequence))
  "Choose an edit distance based on the length of the sequence."
  (let ((len (length src)))
    (cond ((< len 3) 0)
          ((< len 9) 1)
          ((< len 13 2))
          (t 0))))

(defgeneric create-deletion-neighborhood (src e min-length &rest variation-opts)
  (:documentation "Apply a deletion neighborhood algorithm to the SRC sequence. The edit distance
is determined by E. If E is not positive then an adaptive edit distance is computed and used.
MIN-LENGTH determines the minimum length of a sequence that would appear in the result.
The result is a list of variation-class class instances."))

(defmethod create-deletion-neighborhood ((src sequence) e min-length &rest variation-opts)
  (labels ((%sequence-kind (s)
             (typecase s
               (string 'string)
               (bit-vector '(vector bit))
               (vector `(vector ,(array-element-type s)))
               (list 'list)
               (t '(vector t))))
           (%extract-confidence (opts)
             (let ((pos (position :confidence opts)))
               (if pos
                   (values (nth (1+ pos) opts)
                           (append (subseq opts 0 pos)
                                   (subseq opts (+ pos 2))))
                   (values 1.0 opts))))
           (%create (s e min-length depth)
             (declare (fixnum e min-length depth))
             (let ((result nil)
                   (s-last (1- (length s))))
               (when (and (plusp e)
                          (>= s-last min-length))
                 (loop :for i :from 1 :upto e
                       :do (multiple-value-bind (opt-conf my-opts) (%extract-confidence variation-opts)
                             (let ((my-conf (max 0.01 (- 1.0 (* depth (- 1.0 opt-conf))))))
                               (loop :for c :from 0 :upto s-last
                                     :do (let ((target (cond ((= c 0)
                                                              (subseq s 1))
                                                             ((= c s-last)
                                                              (subseq s 0 s-last))
                                                             (t
                                                              (concatenate (%sequence-kind s)
                                                                           (subseq s 0 c)
                                                                           (subseq s (1+ c)))))))
                                           (setf result (append result
                                                                (list (apply 'make-variation target
                                                                             "DEL-HOOD"
                                                                             :edit-distance depth
                                                                             :confidence my-conf
                                                                             my-opts))
                                                                (%create target (1- e) min-length (1+ depth))))))))))
               result)))
    (let ((actual-e (if (plusp e) e (adaptive-edit-distance src))))
      (deduplicate-list (%create src actual-e min-length 1)))))

(defmethod create-deletion-neighborhood ((obj variation-class) e min-length &rest variation-opts)
  "After creating the deletion neighborhood, add the new instances as derived objects
to OBJ."
  (let ((derived (apply 'create-deletion-neighborhood (value obj) e min-length variation-opts)))
    (mapcar (lambda (x) (add-derived obj x)) derived)
    derived))
