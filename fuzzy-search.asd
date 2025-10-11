;;;; fuzzy-search.asd

(asdf:defsystem #:fuzzy-search
  :description "Code to support indexing data and searching it with various fuzzy algorithms"
  :author "Dan S. Camper <dan@bti.net>"
  :license  "Apache 2.0"
  :version "0.0.1"
  :depends-on ("alexandria"
               "cl-ppcre"
               "flexi-streams"
               "ironclad"
               "str")
  :serial t
  :components ((:file "package")
               (:file "util")
               (:file "metadata")
               (:file "variation")
               (:file "deletion-neighborhood")
               (:file "normalize")
               (:file "synonym-data")
               (:file "synonyms")
               (:file "fuzzy-search")))
