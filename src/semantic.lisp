(in-package #:clermaid)

;;;; -- Semantic Terminal Rendering --

(defun mermaid--semantic-role (row index)
  "Return the structural terminal role of ROW's character at INDEX."
  (let ((character (char row index))
        (line-has-word (some #'alphanumericp row)))
    (cond
      ((find character "┌┐└┘├┤┬┴┼╭╮╰╯╔╗╚╝╠╣╦╩╬═║─│╴╵╶╷┄┆") ':mermaid-box)
      ((find character "<>▶◀▲▼^") ':mermaid-arrow)
      ((char= character #\Space) ':plain)
      ((and (find character "+|-/*")
            (or (not line-has-word)
                (and (> index 0) (< (1+ index) (length row))
                     (find (char row (1- index)) " +-<>|/*")
                     (find (char row (1+ index)) " +-<>|/*")))) ':mermaid-edge)
      (t ':mermaid-label))))

(defun mermaid--semantic-row (row)
  "Convert printable ROW into structural semantic spans without changing text."
  (let ((spans nil) (start 0) (role nil))
    (loop for index below (length row)
          for next-role = (mermaid--semantic-role row index)
          do (when (and role (not (eq role next-role)))
               (push (cons role (subseq row start index)) spans)
               (setf start index))
             (unless role (setf role next-role)))
    (when role (push (cons role (subseq row start)) spans))
    (nreverse spans)))

(defun render-mermaid-spans (source &key (width 120) (unicode t))
  "Render SOURCE as rows of semantic role/text conses.
The text and cell widths are identical to RENDER-MERMAID; roles carry no ANSI."
  (mapcar #'mermaid--semantic-row
          (render-mermaid source :width width :unicode unicode)))
