#|
  This file is a part of fuzzy-finder project.
  Copyright (c) 2018 hennin (hennin.ltn@gmail.com)
|#

#|
  Author: hennin (hennin.ltn@gmail.com)
|#

(defsystem "fuzzy-finder"
  :version "0.1.0"
  :author "hennin"
  :license "MIT"
  :depends-on ("alexandria")
  :components ((:module "src"
                :components
                ((:file "fuzzy-finder"))))
  :description ""
  :long-description
  #.(read-file-string
     (subpathname *load-pathname* "README.markdown"))
  :in-order-to ((test-op (test-op "fuzzy-finder-test"))))
