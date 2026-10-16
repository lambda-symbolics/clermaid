(in-package #:clermaid)

;;;; -- Conditions and Resource Bounds --

(define-condition mermaid-error (error)
  ((reason :initarg :reason
           :reader mermaid-error-reason
           :documentation "A concise explanation of the failed rendering contract.")
   (position :initarg :position
             :initform nil
             :reader mermaid-error-position
             :documentation "The zero-based source offset when it is known."))
  (:documentation "A Mermaid parse or terminal layout failure.")
  (:report (lambda (condition stream)
             (format stream "~a~@[ (at character ~d)~]"
                     (mermaid-error-reason condition)
                     (mermaid-error-position condition)))))

(define-condition invalid-diagram (mermaid-error) ()
  (:documentation "Malformed input in a supported Mermaid diagram family."))

(define-condition unsupported-syntax (mermaid-error) ()
  (:documentation "Input outside the explicitly supported Mermaid subset."))

(define-condition diagram-too-wide (mermaid-error)
  ((width :initarg :width
          :reader diagram-too-wide-width
          :documentation "The available terminal columns.")
   (required-width :initarg :required-width
                   :reader diagram-too-wide-required-width
                   :documentation "The required terminal columns."))
  (:documentation "A diagram cannot fit without losing labels or topology."))

(define-condition resource-limit-exceeded (mermaid-error) ()
  (:documentation "Input or output exceeds a bounded rendering resource limit."))

(defparameter *maximum-source-length* 65536
  "The maximum number of characters in one Mermaid source block.")
(defparameter *maximum-nodes* 64
  "The maximum nodes or sequence participants in one diagram.")
(defparameter *maximum-edges* 128
  "The maximum edges or sequence events in one diagram.")
(defparameter *maximum-output-rows* 256
  "The maximum rows in a rendered diagram.")
(defparameter *maximum-output-columns* 2048
  "The absolute maximum columns in a rendered diagram.")

;;;; -- Diagram Protocol --

(defclass diagram () ()
  (:documentation "A parsed Mermaid diagram suitable for terminal layout."))

(defgeneric diagram-parse (kind source)
  (:documentation "Parse SOURCE as KIND, signaling a MERMAID-ERROR on failure."))

(defgeneric diagram-render (diagram &key width unicode)
  (:documentation
   "Return plain terminal rows and corresponding rows of (ROLE . TEXT) spans.
Both values fit WIDTH terminal cells. UNICODE selects box-drawing characters.
Do not mutate DIAGRAM."))

;;;; -- Shared Text Operations --

(defun source-lines (source)
  "Split SOURCE into lines, accepting LF and CRLF line endings."
  (loop for start = 0 then (1+ end)
        for end = (position #\Newline source :start start)
        collect (string-right-trim '(#\Return)
                                   (subseq source start end))
        while end))

(defun trim-text (text)
  "Remove surrounding spaces and tabs from TEXT."
  (string-trim '(#\Space #\Tab #\Return) text))

(defun text-prefix-p (prefix text)
  "Return exactly T when TEXT starts with PREFIX."
  (and (<= (length prefix) (length text))
       (string= prefix text :end2 (length prefix))))

(defun pad-text (text width)
  "Right-pad TEXT to WIDTH terminal cells without truncation."
  (concatenate 'string text
               (make-string (max 0 (- width (text-cell-width text)))
                            :initial-element #\Space)))

(defun check-diagram-size (nodes edges)
  "Enforce the shared semantic size limits before allocating a layout."
  (when (or (> nodes *maximum-nodes*) (> edges *maximum-edges*))
    (error 'resource-limit-exceeded
           :reason "Diagram has too many nodes or edges.")))

(defun check-output-size (rows width)
  "Validate ROWS against WIDTH and absolute output bounds; return ROWS."
  (when (> (length rows) *maximum-output-rows*)
    (error 'resource-limit-exceeded :reason "Diagram has too many terminal rows."))
  (let ((required (loop for row in rows maximize (text-cell-width row) into size
                        finally (return (or size 0)))))
    (when (> required *maximum-output-columns*)
      (error 'resource-limit-exceeded :reason "Diagram exceeds the column limit."))
    (when (> required width)
      (error 'diagram-too-wide :width width :required-width required
             :reason "Diagram does not fit the available terminal width.")))
  rows)
