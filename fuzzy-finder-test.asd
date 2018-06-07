#|
  This file is a part of fuzzy-finder project.
  Copyright (c) 2018 hennin (hennin.ltn@gmail.com)
|#

(defsystem "fuzzy-finder-test"
  :defsystem-depends-on ("prove-asdf")
  :author "hennin"
  :license "MIT"
  :depends-on ("fuzzy-finder"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "fuzzy-finder"))))
  :description "Test system for fuzzy-finder"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
