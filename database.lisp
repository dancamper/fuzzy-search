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
  (dbi:with-connection (conn :sqlite3 :database-name db-path)
    (ensure-people-table conn)
    (with-open-file (in csv-path :direction :input)
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
  (dbi:with-connection (conn :sqlite3 :database-name db-path)
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

(defun append-random-texas-city (input-path)
  "Reads a simple CSV file (no quotes/escapes), appends a random
city/state/zip triple from Texas to each line, and writes the
result to a new file with '-with-cities' appended before the extension."
  (let* ((in (merge-pathnames input-path))
         (out (make-pathname :name (concatenate 'string
                                                (pathname-name in)
                                                "-with-cities")
                             :type (pathname-type in)
                             :defaults in))
         (tx-cities '(("Austin" "TX" "73301")
                      ("Dallas" "TX" "75201")
                      ("Houston" "TX" "77002")
                      ("San Antonio" "TX" "78205")
                      ("Fort Worth" "TX" "76102")
                      ("El Paso" "TX" "79901")
                      ("Arlington" "TX" "76010")
                      ("Corpus Christi" "TX" "78401")
                      ("Plano" "TX" "75023")
                      ("Laredo" "TX" "78040")
                      ("Lubbock" "TX" "79401")
                      ("Garland" "TX" "75040")
                      ("Irving" "TX" "75039")
                      ("Amarillo" "TX" "79101")
                      ("Grand Prairie" "TX" "75050")
                      ("Brownsville" "TX" "78520")
                      ("Pasadena" "TX" "77502")
                      ("McKinney" "TX" "75069")
                      ("Frisco" "TX" "75034")
                      ("Killeen" "TX" "76541")
                      ("McAllen" "TX" "78501")
                      ("Waco" "TX" "76701")
                      ("Carrollton" "TX" "75006")
                      ("Denton" "TX" "76201")
                      ("Midland" "TX" "79701")
                      ("Abilene" "TX" "79601")
                      ("Beaumont" "TX" "77701")
                      ("Round Rock" "TX" "78664")
                      ("Odessa" "TX" "79761")
                      ("Tyler" "TX" "75701")
                      ("Wichita Falls" "TX" "76301")
                      ("Richardson" "TX" "75080")
                      ("Lewisville" "TX" "75067")
                      ("San Angelo" "TX" "76903")
                      ("College Station" "TX" "77840")
                      ("Edinburg" "TX" "78539")
                      ("Mission" "TX" "78572")
                      ("Bryan" "TX" "77803")
                      ("Longview" "TX" "75601")
                      ("Pharr" "TX" "78577")
                      ("Temple" "TX" "76501")
                      ("New Braunfels" "TX" "78130")
                      ("Conroe" "TX" "77301")
                      ("Port Arthur" "TX" "77640")
                      ("Rosenberg" "TX" "77471")
                      ("Georgetown" "TX" "78626")
                      ("Cedar Park" "TX" "78613")
                      ("Burleson" "TX" "76028")
                      ("Haltom City" "TX" "76117")
                      ("Weatherford" "TX" "76086"))))
    (with-open-file (input in :direction :input)
      (with-open-file (output out :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
        (loop :for line = (read-line input nil)
              :while line
              :do (destructuring-bind (city state zip)
                      (nth (random (length tx-cities)) tx-cities)
                    (format output "~a,~a,~a,~a~%" line city state zip)))))
    (namestring out)))

