(in-package #:clermaid)

;;;; -- Public Rendering Boundary --

(defun source--validate (source)
  "Validate the bounded source and reject terminal control characters."
  (when (> (length source) *maximum-source-length*)
    (error 'resource-limit-exceeded :reason "Mermaid source exceeds the character limit."))
  (loop for character across source
        for position from 0
        for code = (char-code character)
        when (or (and (< code 32)
                      (not (find character '(#\Tab #\Newline #\Return))))
                 (<= 127 code 159))
          do (error 'invalid-diagram :position position
                    :reason "Mermaid source contains a terminal control character."))
  source)

(defun source--kind (source)
  "Identify the first nonblank, noncomment Mermaid header in SOURCE."
  (let* ((line (find-if (lambda (line)
                          (let ((text (trim-text line)))
                            (and (plusp (length text))
                                 (not (text-prefix-p "%%" text)))))
                        (source-lines source)))
         (header (and line (trim-text line)))
         (word (and header
                    (subseq header 0 (position-if
                                      (lambda (character)
                                        (find character '(#\Space #\Tab #\;)))
                                      header)))))
    (cond
      ((null word)
       (error 'invalid-diagram :reason "Mermaid source has no diagram header."))
      ((member word '("flowchart" "graph") :test #'string=)
       ':flowchart)
      ((string= word "sequenceDiagram")
       ':sequence)
      ((member word '("stateDiagram" "stateDiagram-v2") :test #'string=)
       ':state)
      ((string= word "classDiagram")
       ':class)
      ((string= word "erDiagram")
       ':er)
      (t
       (error 'unsupported-syntax :reason "Unsupported Mermaid diagram family.")))))

(defun render-mermaid (source &key (width 120) (unicode t))
  "Render Mermaid SOURCE as a fresh list of printable terminal row strings.

WIDTH is the positive terminal-cell budget. UNICODE selects box drawing rather
than ASCII line characters; labels retain their original Unicode text. No ANSI
sequences, terminal I/O, external programs, or browser runtimes are used.

Signal MERMAID-ERROR for malformed or unsupported input, resource limits, or a
diagram that cannot fit. Callers can then display their original fenced source.
The RETRY-WITH-WIDTH restart accepts a different positive column budget."
  (check-type source string)
  (check-type width (integer 1 *))
  (check-type unicode boolean)
  (source--validate source)
  (restart-case
      (let* ((kind (source--kind source))
             (diagram (diagram-parse kind source))
             (rows (diagram-render diagram :width width :unicode unicode)))
        (dolist (row rows)
          (unless (and (stringp row)
                       (notany (lambda (character)
                                 (let ((code (char-code character)))
                                   (or (< code 32) (<= 127 code 159))))
                               row))
            (error 'invalid-diagram
                   :reason "A diagram label contains a nonprinting control character.")))
        (check-output-size rows width))
    (retry-with-width (new-width)
      :report "Render the original source again with a different column budget."
      (render-mermaid source :width new-width :unicode unicode))))
