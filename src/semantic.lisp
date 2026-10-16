(in-package #:clermaid)

;;;; -- Semantic Terminal Rendering --

(defun mermaid--semantic-role (character)
  "Return the terminal semantic role for Mermaid CHARACTER."
  (cond
    ((find character "┌┐└┘├┤┬┴┼╭╮╰╯╔╗╚╝╠╣╦╩╬═║─│╴╵╶╷┄┆") ':mermaid-box)
    ((find character "<>▶◀▲▼^v") ':mermaid-arrow)
    ((find character "/*\\+|-") ':mermaid-edge)
    ((char= character #\Space) ':plain)
    (t ':mermaid-label)))

(defun mermaid--semantic-row (row)
  "Convert printable ROW into contiguous semantic spans without changing text."
  (let ((spans nil) (start 0) (role nil))
    (loop for index below (length row)
          for next-role = (mermaid--semantic-role (char row index))
          do (when (and role (not (eq role next-role)))
               (push (cons role (subseq row start index)) spans)
               (setf start index))
             (unless role (setf role next-role)))
    (when role
      (push (cons role (subseq row start)) spans))
    (nreverse spans)))

(defun render-mermaid-spans (source &key (width 120) (unicode t))
  "Render SOURCE as rows of semantic role/text conses.

The returned rows have exactly the same text and cell widths as RENDER-MERMAID.
Roles are portable semantic keywords, not terminal escape sequences, so a host
can apply its own color policy while retaining plain rendering as a fallback."
  (mapcar #'mermaid--semantic-row
          (render-mermaid source :width width :unicode unicode)))
