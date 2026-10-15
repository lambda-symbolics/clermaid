(asdf:defsystem #:clermaid
  :description "Bounded Mermaid diagrams as terminal text."
  :author "Lambda Symbolics OÜ"
  :license "COLL-Attribution"
  :version "0.1.0"
  :depends-on (#:clinedi)
  :serial t
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "protocol")
                             (:file "graph")
                             (:file "graph-layout")
                             (:file "sequence")
                             (:file "render"))))
  :in-order-to ((asdf:test-op (asdf:test-op #:clermaid/tests))))

(asdf:defsystem #:clermaid/tests
  :depends-on (#:clermaid)
  :serial t
  :components ((:module "tests"
                :serial t
                :components ((:file "graph")
                             (:file "layout")
                             (:file "sequence")
                             (:file "render"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (uiop:symbol-call '#:clermaid '#:run-tests)))
