(in-package #:clermaid)

(defclass sequence-diagram (diagram)
  ((participants :initarg       :participants
                 :reader        sequence-participants
                 :documentation "The participants in order of first mention.")
   (events :initarg       :events
           :reader        sequence-events
           :documentation "The messages, notes, and fragment boundaries in source order."))
  (:documentation "A parsed sequence diagram."))

(defstruct (sequence-participant (:constructor make-sequence-participant (id label kind order)))
  "A sequence participant with its ID, display label, actor or participant kind, and order of first mention."
  id label kind order)

(defstruct (sequence-event (:constructor make-sequence-event (kind &key from to label arrow depth note side)))
  "A message, note, or fragment boundary in a sequence diagram, with its fragment nesting depth."
  kind from to label arrow depth note side)

(defun sequence--fail (class reason &optional position)
  "Signal a Mermaid error of class CLASS with REASON and the optional source POSITION."
  (error class :reason reason :position position))

(defun sequence--trim (s)
  "Remove surrounding spaces, tabs, and carriage returns from S."
  (trim-text s))

(defun sequence--unquote (s)
  "Trim S and remove one pair of matching single or double quotes around it."
  (let ((s (sequence--trim s)))
    (if (and (> (length s) 1) (member (char s 0) '(#\" #\'))
             (char= (char s 0) (char s (1- (length s)))))
        (subseq s 1 (1- (length s))) s)))

(defun sequence--find (participants id)
  "Return the participant in PARTICIPANTS whose ID is ID, or NIL."
  (find id participants :key #'sequence-participant-id :test #'string=))

(defun sequence--validate-id (id)
  "Reject syntax extensions rather than interpreting them as participant names."
  (unless (and (plusp (length id))
               (or (alphanumericp (char id 0)) (char= (char id 0) #\_))
               (every (lambda (character)
                        (or (alphanumericp character) (find character "_-"))) id))
    (sequence--fail 'unsupported-syntax "Participant IDs must contain letters, digits, underscores, or hyphens.")))

(defun sequence--ensure (participants id counter &key label kind)
  "Return PARTICIPANTS with a participant for ID, and that participant as a second value.

A new participant takes order COUNTER. LABEL and KIND replace the stored values
when given."
  (sequence--validate-id id)
  (let ((p (sequence--find participants id)))
    (if p
        (progn
          (when label (setf (sequence-participant-label p) label))
          (when kind (setf (sequence-participant-kind p) kind))
          (values participants p))
        (let ((p (make-sequence-participant id (or label id) (or kind :participant) counter)))
          (values (append participants (list p)) p)))))

(defun sequence--arrow-at (line limit)
  "Return the earliest arrow token in LINE before LIMIT as (INDEX . TOKEN), or NIL."
  (let ((tokens '("-->>" "->>" "-->" "->" "--x" "-x" "--)" "-)")))
    (loop with best = nil
          for token in tokens
          for at = (search token line :end2 limit :test #'char=)
          when (and at (or (null best) (< at (first best)))) do (setf best (cons at token))
          finally (return best))))

(defun sequence--split-arrow (line)
  "Return the source, target, arrow token, and label of the message LINE as four values."
  (let* ((colon (position #\: line))
         (limit (or colon (length line)))
         (found (sequence--arrow-at line limit)))
    (unless found (sequence--fail 'invalid-diagram "Sequence message has no supported arrow."))
    (let* ((at (first found)) (arrow (rest found))
           (from         (sequence--trim (subseq line 0 at)))
           (suffix-start (+ at (length arrow)))
           (endpoint-end (or colon (length line)))
           (to           (sequence--trim (subseq line suffix-start endpoint-end)))
           (label        (if colon (sequence--trim (subseq line (1+ colon))) "")))
      (when (or (zerop (length from)) (zerop (length to)))
        (sequence--fail 'invalid-diagram "Sequence message has an empty endpoint."))
      (values from to arrow label))))

(defun sequence--parse-declaration (line)
  "Return the ID, label, and kind of the participant or actor declaration LINE as three values."
  (let* ((space (position-if (lambda (c) (member c '(#\Space #\Tab))) line))
         (kind  (string-downcase (subseq line 0 (or space (length line)))))
         (rest  (sequence--trim (if space (subseq line (1+ space)) ""))))
    (unless (member kind '("participant" "actor") :test #'string=)
      (sequence--fail 'invalid-diagram "Invalid participant declaration."))
    (let ((as (search " as " rest :test #'char-equal)))
      (if as
          (values (sequence--unquote (sequence--trim (subseq rest 0 as)))
                  (sequence--unquote (subseq rest (+ as 4)))
                  (if (string= kind "actor") :actor :participant))
          (values (sequence--unquote rest) (sequence--unquote rest)
                  (if (string= kind "actor") :actor :participant))))))

(defun sequence--parse-note (line)
  "Return the side, comma-separated participant names, and text of the note LINE as three values."
  (let* ((rest  (sequence--trim (subseq line 4)))
         (colon (position #\: rest)))
    (unless colon (sequence--fail 'invalid-diagram "Note has no text."))
    (let* ((where (sequence--trim (subseq rest 0 colon)))
           (text  (sequence--trim (subseq rest (1+ colon))))
           (parts (cond ((text-prefix-p "over " (string-downcase where))
                         (list :over (sequence--trim (subseq where 5))))
                        ((text-prefix-p "left of " (string-downcase where))
                         (list :left (sequence--trim (subseq where 8))))
                        ((text-prefix-p "right of " (string-downcase where))
                         (list :right (sequence--trim (subseq where 9))))
                        (t
                         (sequence--fail 'invalid-diagram "Invalid note endpoint.")))))
      (values (first parts) (second parts) text))))

(defun sequence--unsupported-p (line)
  "Recognize unsupported sequence directives before parsing messages."
  (not (null (some (lambda (word) (text-prefix-p word (string-downcase line)))
                   '("activate " "deactivate " "par " "critical " "break " "rect " "autonumber" "link ")))))

(defun sequence--parse (source)
  "Parse SOURCE into a sequence diagram, checking fragment nesting and size limits."
  (let ((participants nil) (events nil) (stack nil) (order 0) (line-no 0) (header nil))
    (dolist (raw (source-lines source))
      (incf line-no)
      (let ((line (sequence--trim raw)))
        (cond
          ((or (zerop (length line)) (text-prefix-p "%%" line) (text-prefix-p "//" line))
           nil)
          ((null header)
           (unless (string-equal line "sequenceDiagram")
             (sequence--fail 'invalid-diagram "Sequence diagram must start with sequenceDiagram."))
           (setf header t))
          ((sequence--unsupported-p line)
           (sequence--fail 'unsupported-syntax "Unsupported sequence syntax." line-no))
          ((or (text-prefix-p "participant " line) (text-prefix-p "actor " line))
           (multiple-value-bind (id label kind) (sequence--parse-declaration line)
             (when (zerop (length id)) (sequence--fail 'invalid-diagram "Empty participant ID." line-no))
             (multiple-value-bind (new p) (sequence--ensure participants id order :label label :kind kind)
               (setf participants new) (unless (sequence-participant-order p) (setf (sequence-participant-order p) order)))))
          ((or (text-prefix-p "Note " line) (text-prefix-p "note " line))
           (multiple-value-bind (side who text) (sequence--parse-note line)
             (let ((ids (mapcar #'sequence--trim (uiop:split-string who :separator ","))))
                (unless (and (every (lambda (id) (plusp (length id))) ids)
                             (if (eq side :over) (<= 1 (length ids) 2) (= 1 (length ids))))
                  (sequence--fail 'invalid-diagram "A note needs one participant, or two for over." line-no))
               (dolist (id ids)
                 (multiple-value-bind (new p) (sequence--ensure participants id order)
                   (declare (ignore p)) (setf participants new) (incf order)))
               (push (make-sequence-event :note :from (first ids) :to ids :label text :side side :depth (length stack)) events))))
          ((member (string-downcase (subseq line 0 (or (position #\Space line) (length line)))) '("loop" "alt" "opt") :test #'string=)
           (let* ((space (position #\Space line)) (type (string-downcase (subseq line 0 (or space (length line)))))
                  (label (sequence--trim (if space (subseq line (1+ space)) ""))))
              (push (list type nil) stack)
             (push (make-sequence-event :fragment :note type :label label :depth (1- (length stack))) events)))
          ((string-equal line "else")
           (unless (and stack (string-equal (first (first stack)) "alt")
                        (not (second (first stack))))
             (sequence--fail 'invalid-diagram "else requires an alt without an earlier else." line-no))
           (setf (second (first stack)) t)
           (push (make-sequence-event :else :depth (1- (length stack))) events))
          ((text-prefix-p "else " line)
           (unless (and stack (string-equal (first (first stack)) "alt")
                        (not (second (first stack))))
             (sequence--fail 'invalid-diagram "else requires an alt without an earlier else." line-no))
           (setf (second (first stack)) t)
           (push (make-sequence-event :else :label (sequence--trim (subseq line 5)) :depth (1- (length stack))) events))
          ((string-equal line "end")
           (unless stack (sequence--fail 'invalid-diagram "Unexpected end." line-no))
           (pop stack) (push (make-sequence-event :end :depth (length stack)) events))
          (t
           (multiple-value-bind (from to arrow label) (sequence--split-arrow line)
             (dolist (id (list from to))
               (multiple-value-bind (new p) (sequence--ensure participants id order)
                 (declare (ignore p)) (setf participants new) (incf order)))
             (push (make-sequence-event :message :from from :to to :arrow arrow :label label :depth (length stack)) events))))))
    (unless header (sequence--fail 'invalid-diagram "Empty sequence diagram."))
    (when stack (sequence--fail 'invalid-diagram "Unclosed sequence fragment."))
    (unless participants (sequence--fail 'invalid-diagram "Sequence diagram has no participants."))
    (check-diagram-size (length participants) (length events))
    (make-instance 'sequence-diagram :participants participants :events (nreverse events))))

(defmethod diagram-parse ((kind (eql :sequence)) source)
  "Parse SOURCE as a Mermaid sequence diagram."
  (declare (ignore kind))
  (when (> (length source) *maximum-source-length*)
    (sequence--fail 'resource-limit-exceeded "Source exceeds the configured limit."))
  (sequence--parse source))

;;;; -- Sequence Geometry --

(defun sequence--event-caption (event)
  "Return the complete caption for a fragment boundary."
  (format nil "~a~@[ ~a~]"
          (if (eq (sequence-event-kind event) :fragment)
              (sequence-event-note event) "else")
          (sequence-event-label event)))

(defun sequence--event-height (event)
  "Reserve label, connector, and spacing rows before allocating a canvas."
  (case (sequence-event-kind event)
    (:message
     (if (string= (sequence-event-from event) (sequence-event-to event)) 5 3))
    (:note
     4)
    (otherwise
     2)))

(defun sequence--segment (canvas from to &key dashed)
  "Connect the aligned cells FROM and TO, each a (column . row) cons, including both endpoints."
  (destructuring-bind ((x1 . y1) (x2 . y2)) (list from to)
    (loop with dx = (signum (- x2 x1))
          with dy = (signum (- y2 y1))
          for x = x1 then (+ x dx)
          for y = y1 then (+ y dy)
          until (and (= x x2) (= y y2))
          do (graph--connect canvas (cons x y) (cons (+ x dx) (+ y dy)) :dashed dashed))))

(defun sequence--box (canvas x y &key label columns unicode)
  "Draw a note or participant frame and its label with separate roles."
  (let ((bar (make-string (- columns 2) :initial-element (if unicode #\─ #\-))))
    (graph--put-text canvas x y
                     :text (format nil "~a~a~a" (if unicode "┌" "+") bar (if unicode "┐" "+"))
                     :role ':mermaid-box)
    (graph--put-text canvas x (1+ y)
                     :text (format nil "~a ~a ~a" (if unicode "│" "|")
                                   (pad-text label (- columns 4)) (if unicode "│" "|"))
                     :role ':mermaid-box)
    (graph--put-text canvas (+ x 2) (1+ y) :text label :role ':mermaid-label)
    (graph--put-text canvas x (+ y 2)
                     :text (format nil "~a~a~a" (if unicode "└" "+") bar (if unicode "┘" "+"))
                     :role ':mermaid-box)))

(defun sequence--head (arrow rightward unicode)
  "Select a destination-facing head for each supported message token."
  (cond ((find #\x arrow)
         (if unicode "×" "x"))
        ((find #\) arrow)
         (if rightward (if unicode "▷" ")") (if unicode "◁" "(")))
        ((search ">>" arrow)
         (if rightward (if unicode "▶" ">>") (if unicode "◀" "<<")))
        (t
         (if rightward (if unicode "▷" ">") (if unicode "◁" "<")))))

(defmethod diagram-render ((diagram sequence-diagram) &key (width 120) (unicode t))
  "Render lifelines, directional messages, self-calls, notes, and nested frames."
  (let* ((participants  (sequence-participants diagram))
         (events        (sequence-events diagram))
         (box-width     (max 5 (+ 4 (loop for p in participants maximize
                                          (text-cell-width (sequence-participant-label p))))))
         (text-width    (loop for e in events maximize (text-cell-width (or (sequence-event-label e) "")) into maximum
                              finally (return (or maximum 0))))
         (depth         (loop for e in events maximize (sequence-event-depth e) into maximum
                              finally (return (or maximum 0))))
         (caption-width (loop for e in events when (member (sequence-event-kind e) '(:fragment :else))
                             maximize (text-cell-width (sequence--event-caption e)) into maximum
                             finally (return (or maximum 0))))
         (gutter        (+ (* 2 depth) (if (plusp caption-width) (+ caption-width 4) 0)))
         (margin        (+ gutter text-width 6))
         (spacing       (+ box-width text-width 6))
         (columns       (+ margin box-width (* (1- (length participants)) spacing) text-width 6))
         (height        (+ 3 (reduce #'+ events :key #'sequence--event-height :initial-value 0))))
    (check-diagram-size (length participants) (length events))
    (when (> columns width)
      (error 'diagram-too-wide :width width :required-width columns
             :reason "Sequence labels and participants do not fit the available terminal width."))
    (let ((canvas    (graph--canvas columns height))
          (positions (make-hash-table :test #'equal))
          (y         3)
          (frames    nil))
      (loop for p in participants for index from 0
            for left = (+ margin (* index spacing))
            for center = (+ left (floor box-width 2))
            do (setf (gethash (sequence-participant-id p) positions) center)
               (sequence--box canvas left 0
                              :label   (sequence-participant-label p)
                              :columns box-width
                              :unicode unicode)
               (when (> height 3)
                 (sequence--segment canvas (cons center 3) (cons center (1- height)))))
      (labels ((position-of (id)
                 (or (gethash id positions)
                     (sequence--fail 'invalid-diagram "Unknown sequence participant.")))

               (boundary (event row)
                 (let* ((level (sequence-event-depth event))
                        (left (* 2 level)) (right (- columns 1 (* 2 level))))
                   (sequence--segment canvas (cons left row) (cons right row))
                   (unless (eq (sequence-event-kind event) :end)
                     (graph--put-text canvas (+ left 2) row :text (sequence--event-caption event))))))
        (dolist (event events)
          (case (sequence-event-kind event)
            (:fragment
             (push (cons (sequence-event-depth event) y) frames)
             (boundary event y))
            (:else
             (boundary event y))
            (:end
             (let* ((frame (pop frames))
                    (left (* 2 (first frame))) (right (- columns 1 left)))
               (sequence--segment canvas (cons left (rest frame)) (cons left y))
               (sequence--segment canvas (cons right (rest frame)) (cons right y))
               (boundary event y)))
            (:message
             (let* ((a         (position-of (sequence-event-from event)))
                    (b         (position-of (sequence-event-to event)))
                    (arrow     (sequence-event-arrow event))
                    (self      (= a b))
                    (rightward (< a b))
                    (head      (sequence--head arrow rightward unicode))
                    (row       (+ y (if self 3 1)))
                    (dashed    (text-prefix-p "--" arrow)))
               (graph--put-text canvas (+ (min a b) 2) y :text (sequence-event-label event))
               (if self
                   (let ((turn (+ a 4)))
                     (sequence--segment canvas (cons a (1+ y)) (cons turn (1+ y)) :dashed dashed)
                     (sequence--segment canvas (cons turn (1+ y)) (cons turn row) :dashed dashed)
                     (sequence--segment canvas (cons turn row) (cons a row) :dashed dashed))
                   (sequence--segment canvas (cons a row) (cons b row) :dashed dashed))
               (graph--put-text canvas (if rightward (- b (1- (text-cell-width head))) b) row
                                :text head
                                :role ':mermaid-arrow)))
            (:note
             (let* ((xs         (mapcar #'position-of (sequence-event-to event)))
                    (a (reduce #'min xs)) (b (reduce #'max xs))
                    (label      (sequence-event-label event))
                    (note-width (max (+ 4 (text-cell-width label)) (+ 3 (- b a))))
                    (left       (case (sequence-event-side event)
                                  (:left
                                   (- a note-width 2))
                                  (:right
                                   (+ a 2))
                                  (otherwise
                                   (- (floor (+ a b) 2) (floor note-width 2))))))
               (sequence--box canvas left y :label label :columns note-width :unicode unicode))))
          (incf y (sequence--event-height event))))
      (multiple-value-bind (rows semantic-rows) (graph--canvas-rows canvas unicode)
        (values (check-output-size rows width) semantic-rows)))))
