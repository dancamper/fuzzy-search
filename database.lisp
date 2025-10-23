;;;; database.lisp

(in-package #:net.bti.fuzzy-search)

;; -----------------------------------------------------------------------------

(defun ensure-people-table (conn)
  "Create table 'people' if it doesn't already exist."
  (dbi:do-sql conn
    "CREATE TABLE IF NOT EXISTS people (
       id INTEGER PRIMARY KEY AUTOINCREMENT,
       name TEXT,
       address TEXT,
       city TEXT,
       state TEXT,
       zip TEXT
     );")
  (dbi:do-sql conn "DELETE FROM people;")
  (dbi:do-sql conn "DELETE FROM sqlite_sequence WHERE name='people';"))

(defun insert-person (conn name address city state zip)
  "Insert a single person row into the database."
  (dbi:do-sql conn
    "INSERT INTO people (name, address, city, state, zip) VALUES (?, ?, ?, ?, ?)"
    (list name address city state zip)))

(defun import-csv-to-sqlite (csv-path db-path)
  "Read CSV lines of 'name,address,city,state,zip' and store them in SQLite3 DB.
Creates DB and table if needed."
  (dbi:with-connection (conn :sqlite3 :database-name (truename db-path))
    (ensure-people-table conn)
    (with-open-file (in (truename csv-path) :direction :input)
      (loop :for line = (read-line in nil)
            :while line
            :do (let ((fields (str:split #\, line)))
                  (when (= (length fields) 5)
                    (destructuring-bind (name addr city state zip) fields
                      (insert-person conn name addr city state zip)))))))
  (format t "Imported ~A into ~A~%" csv-path db-path))

;; -----------------------------------------------------------------------------

(defun process-people-table (db-path fn &key (batch-size 100))
  "Read and print all rows from the 'people' table in batches of BATCH-SIZE.
Prints each record to *standard-output* until all rows are read."
  (dbi:with-connection (conn :sqlite3 :database-name (truename db-path))
    (let ((query (dbi:prepare conn
                              "SELECT id, name, address, city, state, zip
                               FROM people
                               ORDER BY id
                               LIMIT ? OFFSET ?")))
      (loop :with offset = 0
            :for rows = (dbi:fetch-all
                         (dbi:execute
                          query
                          (list batch-size offset)))
            :while rows
            :do (dolist (row rows)
                  (funcall fn row))
                (incf offset batch-size)))))

(defun print-people-row (row)
  (format t "~D | ~A | ~A | ~A, ~A ~A~%"
          (getf row :|id|)
          (getf row :|name|)
          (getf row :|address|)
          (getf row :|city|)
          (getf row :|state|)
          (getf row :|zip|)))

(defun print-people-table (db-path &key (batch-size 100))
  (process-people-table db-path #'print-people-row :batch-size batch-size))

;; -----------------------------------------------------------------------------

