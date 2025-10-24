;;;; util.lisp

(in-package #:net.bti.fuzzy-search)

;;; ----------------------------------------------------------------------------

(defun truthify (x)
  (not (not x)))

(defun deduplicate-list (the-list &key (key #'identity))
  (let ((seen (make-hash-table :size (* 2 (length the-list)) :test 'equalp)))
    (remove-if-not (lambda (s)
                     (let ((v (funcall key s)))
                       (unless (gethash v seen)
                         (setf (gethash v seen) t))))
                   the-list)))

(defun plist-insert (k v p-list)
  "Inserts or overwrites a key/value pair. Returns a fresh plist."
  (let ((result (copy-list p-list)))
    (cond ((getf result k)
           (setf (getf result k) v))
          (result
           (a:appendf result (list k v)))
          (t
           (setf result (list k v))))
    result))

(defun merge-plists (plist1 plist2 &key (test #'eql))
  "Merge two property lists. Keys in PLIST2 override keys in PLIST1.
Returns a fresh plist."
  (let ((result (when plist1 (copy-list plist1))))
    (loop :for (k v) :on plist2 :by #'cddr
          :do (let ((pos (position k result :test test :from-end t)))
                (if (and pos (evenp pos))
                    ;; key exists, so override value
                    (setf (nth (1+ pos) result) v)
                    ;; else, add new key/value pair
                    (a:appendf result (list k v)))))
    result))

(defun print-plist-to-string (plist)
  "Pretty-print a property list to a string and return it."
  (with-output-to-string (s)
    (format s "{")
    (loop :for (k v) :on plist :by #'cddr
          :for first = t :then nil
          :do (unless first (format s " "))
              (format s "~S ~S" k v))
    (format s "}")))

(defun hash-djb2 (s)
  "Simple hash of a string."
  (declare (string s))
  (reduce #'(lambda (hash c) (mod (+ (* 33 hash) c) (expt 2 64)))
          s
          :initial-value 5381
          :key #'char-code))

(defun hash-string (s)
  (declare (type string s))
  (let* ((fmt (flexi-streams:make-external-format :utf-8))
         (in-buffer (flexi-streams:string-to-octets s :external-format fmt))
         (out-buffer (ironclad:digest-sequence :crc32 in-buffer))
         (result 0))
    (loop :for i :from 3 :downto 0
          :do (setf result (logior (ash result 8) (aref out-buffer i))))
    result))

(defun common-prefix-length (s1 s2)
  "Returns the length of the common prefix shared between the strings S1 and S2."
  (declare (type string s1 s2))
  (loop :for i :below (min (length s1) (length s2))
        :while (char= (char s1 i) (char s2 i))
        :finally (return i)))

;;; ------------------------------------

(defclass top-n ()
  ((heap :initform (make-array 0 :adjustable t :fill-pointer 0))
   (limit :initarg :limit :initform 100)
   (key :initarg :key :initform #'identity)
   (test :initarg :test :initform #'<)
   (sorted-p :initform nil)))

(defun make-top-n (n &key (key #'identity) (test #'<))
  (make-instance 'top-n :limit n :key key :test test))

(defmethod insert ((obj top-n) new-value)
  (with-slots (heap limit key test) obj
    (if (< (length heap) limit)
        (vector-push-extend new-value heap)
        (loop :with worst-pos = 0
              :with worst-value = (funcall key (aref heap 0))
              :for i :from 1 :below limit
              :for candidate-value = (funcall key (aref heap i))
              :when (funcall test worst-value candidate-value)
                :do (setf worst-pos i
                          worst-value candidate-value)
              :finally (when (funcall test (funcall key new-value) worst-value)
                         (setf (aref heap worst-pos) new-value))))))

(defmethod contents ((obj top-n))
  (with-slots (heap key test sorted-p) obj
    (unless sorted-p
      (setf heap (sort heap (lambda (a b) (funcall test a b)) :key key)
            sorted-p t))
    heap))

;;; ------------------------------------

