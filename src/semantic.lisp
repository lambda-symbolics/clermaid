(in-package #:clermaid)

;;;; -- Semantic Terminal Rendering --

(defun render-mermaid-spans (source &key (width 120) (unicode t))
  "Render SOURCE as rows of role/text conses assigned during drawing.
The first value's text and cell widths are identical to RENDER-MERMAID."
  (nth-value 1 (render-mermaid source :width width :unicode unicode)))
