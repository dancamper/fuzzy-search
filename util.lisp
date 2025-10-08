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

