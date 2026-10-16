(defpackage #:clermaid
  (:use #:cl)
  (:import-from #:clinedi
                #:text-cell-width
                #:grapheme-next-boundary
                #:grapheme-cell-width)
  (:export #:render-mermaid
           #:render-mermaid-spans
           #:mermaid-error
           #:mermaid-error-reason
           #:mermaid-error-position
           #:invalid-diagram
           #:unsupported-syntax
           #:diagram-too-wide
           #:diagram-too-wide-width
           #:diagram-too-wide-required-width
           #:resource-limit-exceeded))

(in-package #:clermaid)
