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
  "Appends synonyms, if any, to OBJ-OR-LIST and returns a new list."
  (let ((result (copy-list (a:flatten (a:ensure-list obj-or-list)))))
    (when syn-list
      (loop :for obj :in (a:flatten (a:ensure-list obj-or-list))
            :do (a:appendf result (apply-synonyms obj syn-list :confidence 0.90 :searchablep t))))
    result))

(defun tokenize-variation (obj-or-list min-word-length)
  "Breaks strings values in OBJ-OR-LIST into individual words, returning
the result in a fresh list."
  (let ((result nil))
    (loop :for obj :in (a:flatten (a:ensure-list obj-or-list))
          :do (let ((word-list (tokenize obj min-word-length :confidence 0.99 :searchablep t)))
                (push word-list result)))
    result))

(defun del-hood-variation (obj-or-list edit-distance min-word-length)
  "OBJ-OR-LIST should represent words; a deletion neighborhood algorithm is applied
to each word and the result is returned in a flat, fresh list."
  (let ((result nil))
    (loop :for w :in (a:flatten (a:ensure-list obj-or-list))
          :do (let ((hood (create-deletion-neighborhood w edit-distance min-word-length :confidence 0.80 :searchablep t)))
                (push hood result)))
    result))

(defun phonetic-variation (obj-or-list)
  "OBJ-OR-LIST should represent words; a phonetic algorithm is applied
to each word and the result is returned in a flat, fresh list."
  (let ((result nil))
    (loop :for w :in (a:flatten (a:ensure-list obj-or-list))
          :do (let ((phonetic (double-metaphone w :confidence 0.75 :searchablep t)))
                (push phonetic result)))
    result))

;;; ------------------------------------

(defclass base-field ()
  ((value :initarg :value :initform "" :reader value)
   (value-type :initarg :value-type :initform "" :reader value-type)
   (edit-distance :initarg :edit-distance :initform 1 :reader edit-distance)
   (min-word-length :initarg :min-word-length :initform 1 :reader min-word-length)
   (min-del-word-length :initarg :min-del-word-length :initform 2 :reader min-del-word-length)
   (synonym-list :initarg :synonym-list :initform nil :reader synonym-list)))

(defmethod process-for-indexing ((obj base-field))
  "Create the variations of the value embedded within OBJ that will then
be indexed and stored in the corpus for later searching."
  (let* ((value-obj (make-variation (value obj) (value-type obj)))
         ;; The rest of these modify value-obj or derived/embedded objects
         (normalized-value (normalize-variation value-obj))
         (synonyms (synonymize-variation normalized-value (synonym-list obj)))
         (word-list (tokenize-variation synonyms (min-word-length obj)))
         (hoods (del-hood-variation word-list (edit-distance obj) (min-del-word-length obj)))
         (phonetics (phonetic-variation word-list)))
    (declare (ignorable hoods phonetics))
    value-obj))

(defmethod process-for-searching ((obj base-field))
  "Create the variations of the value embedded within OBJ that will then
be used for searching against the corpus. Unlike `process-for-indexing'
this method does not create synonym variations."
  (let* ((value-obj (make-variation (value obj) (value-type obj)))
         ;; The rest of these modify value-obj or derived/embedded objects
         (normalized-value (normalize-variation value-obj))
         (word-list (tokenize-variation normalized-value (min-word-length obj)))
         (hoods (del-hood-variation word-list (edit-distance obj) (min-del-word-length obj)))
         (phonetics (phonetic-variation word-list)))
    (declare (ignorable hoods phonetics))
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

(defclass city-addr-field (base-field)
  ())

;;; ------------------------------------

(defclass state-addr-field (base-field)
  ())

(defmethod initialize-instance :after ((obj state-addr-field) &rest initargs)
  (declare (ignore initargs))
  (with-slots (synonym-list) obj
    (setf synonym-list +us-states-and-territory-synonyms+)))

;;; ------------------------------------

(defclass postal-addr-field (base-field)
  ())

;;; ------------------------------------

(defun class-symbol-from-value-type (type)
  (str:string-case type
    ("FULL-NAME" 'full-name-field)
    ("STREET-ADDR" 'street-addr-field)
    ("CITY-ADDR" 'city-addr-field)
    ("STATE-ADDR" 'state-addr-field)
    ("POSTAL-ADDR" 'postal-addr-field)
    (otherwise (error "~S is not a valid value type" type))))

(defun reset-index ()
  (setf *test-results* nil
        *test-hash-results* (make-hash-table)))

(defun index-field-value (entity-id string-value type)
  (let* ((class-symbol (class-symbol-from-value-type type))
         (value-obj (make-instance class-symbol :value string-value :value-type type))
         (processed-obj (process-for-indexing value-obj)))
    (setf *test-results* processed-obj)
    (let ((hash-entries (hash-searchable entity-id processed-obj)))
      (loop :for searchable-item :in hash-entries
            :do (let ((metadata (metadata-for-index searchable-item)))
                  (push metadata (gethash (hashed-value searchable-item) *test-hash-results*)))))
    (format *standard-output* "ID: ~A: ~A: '~A'~%" entity-id type string-value)
    processed-obj))

(defun test-search (string-value &optional collected-results)
  ;; (declare (optimize (debug 3) (safety 3) (speed 0)))
  (unless (and *test-hash-results* (plusp (hash-table-count *test-hash-results*)))
    (error "Test hash has not been populated; please run (fuzzy-search::test-index)"))
  (let ((word-num-scanner (ppcre:create-scanner "WORD/(\\d+)"))
        (field-name-scanner (ppcre:create-scanner "^[^/]+")))
    (let* ((search-obj (make-instance 'base-field :value string-value))
           (value-obj (process-for-searching search-obj))
           (query-frags (hash-searchable 0 value-obj))
           (scored-entities (make-top-n 100 :test #'> :key #'cdr))
           (search-results (make-hash-table)))
      ;; Match hash values in our corpus; isolate matches into per-entity-id buckets
      (loop :for query-frag :in query-frags
            :do (let ((corpus-hits (gethash (hashed-value query-frag) *test-hash-results*)))
                  (loop :for hit :in corpus-hits
                        :do (let* ((corpus-meta (recreate-metadata hit))
                                   (merged-meta (merge-metadata (meta query-frag)corpus-meta)))
                              (push merged-meta (gethash (get-entity-id corpus-meta) search-results))))))
      ;; Reduce the results for each entity-id
      (loop :for entity-id :being :the :hash-keys :in search-results :using (:hash-value hits)
            :do (let* ((sorted (sort hits (lambda (x y) (metadata-less-p x y))))
                       (reduced (reduce-metadata-list sorted))
                       (fixed-up (mapcar #'fixup-metadata reduced)))
                  (setf (gethash entity-id search-results) fixed-up)))
      ;; Score the metadata and combine it for each entity ID
      (loop :for entity-id :being :the :hash-keys :in search-results :using (:hash-value hits)
            :do (let ((score (reduce #'+ hits :key #'get-confidence)))
                  (insert scored-entities (cons entity-id score))))
      (when collected-results
        (loop :for entity-id :being :the :hash-keys :in search-results :using (:hash-value hits)
              :do (a:appendf (gethash entity-id collected-results) hits)))
      (format *standard-output* "Found: ~S~%" (contents scored-entities))
      search-results)))

;;; ------------------------------------

(defun test-index ()
  (reset-index)
  (index-field-value 1001 "daniel scott camper" "FULL-NAME")
  (index-field-value 1001 "224 cr 1559" "STREET-ADDR")
  (index-field-value 1001 "alba" "CITY-ADDR")
  (index-field-value 1001 "tx" "STATE-ADDR")
  (index-field-value 1001 "75410" "POSTAL-ADDR")
  (index-field-value 1002 "jo anna camper" "FULL-NAME")
  (index-field-value 1002 "224 county road 1559" "STREET-ADDR")
  (index-field-value 1002 "alba" "CITY-ADDR")
  (index-field-value 1002 "tx" "STATE-ADDR")
  (index-field-value 1002 "75410" "POSTAL-ADDR")
  (format *standard-output* "Corpus index entries: ~D~%" (hash-table-count *test-hash-results*)))

(defun test-describe (entity-id search-result-hash-table)
  (loop :for hit :in (gethash entity-id search-result-hash-table)
        :do (format *standard-output* "~A~%" (description hit))))
