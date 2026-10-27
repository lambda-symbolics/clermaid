(in-package #:clermaid)

(defclass graph-node ()
  ((parent :initform      nil
           :accessor      graph-node-parent
           :documentation "The subgraph that contains this node, or NIL at the top level.")
   (id :initarg       :id
       :reader        graph-node-id
       :documentation "The source identifier that names this node.")
   (label :initarg       :label
          :accessor      graph-node-label
          :documentation "The text drawn inside the node.")
   (shape :initarg       :shape
          :initform      ':rectangle
          :accessor      graph-node-shape
          :documentation "The frame shape keyword, such as :RECTANGLE, :ROUNDED, or :DIAMOND.")
   (members :initarg       :members
            :initform      nil
            :accessor      graph-node-members
            :documentation "The class or entity member lines, newest first.")
   (explicit-label-p :initarg       :explicit-label-p
                     :initform      nil
                     :accessor      graph-node-explicit-label-p
                     :documentation "True when the source gives the node its own label."))
  (:documentation "A graph diagram node with its label, shape, and enclosing subgraph."))

(defclass graph-subgraph (graph-node)
  ((direction :initform      nil
              :accessor      graph-subgraph-direction
              :documentation "The layout direction the subgraph declares, or NIL to use the enclosing one."))
  (:documentation "A flowchart subgraph, drawn as a frame around the nodes it contains."))

(defparameter *graph-current-group* nil
  "The flowchart group that contains newly declared nodes.")

(defparameter *maximum-subgraph-depth* 16
  "The maximum number of nested flowchart subgraphs.")

(defclass graph-edge ()
  ((from :initarg       :from
         :reader        graph-edge-from
         :documentation "The node the edge starts at.")
   (to :initarg       :to
       :reader        graph-edge-to
       :documentation "The node the edge ends at.")
   (kind :initarg       :kind
         :initform      ':arrow
         :reader        graph-edge-kind
         :documentation "The line and head style keyword, such as :ARROW, :DOTTED, or :INHERITANCE.")
   (label :initarg       :label
          :initform      nil
          :reader        graph-edge-label
          :documentation "The text drawn along the edge, or NIL.")
   (relation :initarg       :relation
             :initform      nil
             :reader        graph-edge-relation
             :documentation "The relation token as written in the source, such as \"-->\"."))
  (:documentation "A connection between two graph diagram nodes."))

(defclass graph-diagram (diagram)
  ((family :initarg       :family
           :reader        graph-family
           :documentation "The diagram family keyword, one of :FLOWCHART, :GRAPH, :STATE, :CLASS, or :ER.")
   (direction :initarg       :direction
              :initform      ':td
              :reader        graph-direction
              :documentation "The layout direction keyword, such as :TD or :LR.")
   (nodes :initarg       :nodes
          :reader        graph-nodes
          :documentation "The nodes and subgraphs in declaration order.")
   (edges :initarg       :edges
          :reader        graph-edges
          :documentation "The edges in source order."))
  (:documentation "A parsed flowchart, state, class, or entity relationship diagram."))

(defun graph-fail (type reason &optional position)
  "Signal a Mermaid error of class TYPE with REASON and the optional source POSITION."
  (error type :reason reason :position position))

(defun graph-statements (source)
  "Split SOURCE at unquoted newlines and semicolons, preserving quoted punctuation."
  (unless (stringp source) (graph-fail 'invalid-diagram "Source is not a string."))
  (when (> (length source) *maximum-source-length*) (graph-fail 'resource-limit-exceeded "Source is too large."))
  (let ((result nil) (start 0) (quote nil) (escape nil) (square 0) (paren 0))
    (labels ((emit (end)
               (let ((s (trim-text (subseq source start end))))
                 (unless (or (zerop (length s)) (text-prefix-p "%%" s)) (push s result)))))
      (loop for i from 0 below (length source)
            for c = (char source i)
            do (cond
                 ((and quote escape)
                  (setf escape nil))
                 ((and quote (char= c #\\))
                  (setf escape t))
                 ((char= c #\")
                  (setf quote (not quote)))
                 ((not quote)
                  (case c
                    (#\[
                     (incf square))
                    (#\]
                     (if (plusp square) (decf square) (graph-fail 'invalid-diagram "Unmatched bracket." i)))
                    (#\(
                     (incf paren))
                    (#\)
                     (if (plusp paren) (decf paren) (graph-fail 'invalid-diagram "Unmatched parenthesis." i)))
                    (#\;
                     (when (and (zerop square) (zerop paren)) (emit i) (setf start (1+ i))))
                    (#\Newline
                     (when (and (zerop square) (zerop paren)) (emit i) (setf start (1+ i))))))))
      (when quote (graph-fail 'invalid-diagram "Unclosed quote."))
      (when (or (plusp square) (plusp paren)) (graph-fail 'invalid-diagram "Unclosed bracket."))
      (emit (length source))
      (nreverse result))))

(defun graph-header-p (text prefix)
  "Return true when TEXT is the keyword PREFIX alone or followed by a space or tab."
  (and (text-prefix-p prefix text)
       (or (= (length text) (length prefix))
           (find (char text (length prefix)) '(#\Space #\Tab)))))

(defun graph-direction-from-text (text)
  "Return the direction keyword TEXT names, or :TD when TEXT is blank."
  (let ((s (string-upcase (trim-text text))))
    (cond ((zerop (length s))
           :td)
          ((string= s "LR")
           :lr)
          ((string= s "RL")
           :rl)
          ((string= s "TD")
           :td)
          ((string= s "TB")
           :tb)
          ((string= s "BT")
           :bt)
          (t
           (graph-fail 'unsupported-syntax (format nil "Unsupported direction ~a." s))))))

(defun graph-top-position (text char)
  "Return the index of the first CHAR in TEXT outside quotes, brackets, and parentheses, or NIL."
  (let ((quote nil) (square 0) (paren 0))
    (loop for i from 0 below (length text)
          for c = (char text i)
          do (cond
               ((char= c #\")
                (setf quote (not quote)))
               ((not quote)
                (if (and (zerop square) (zerop paren) (char= c char))
                    (return i)
                    (case c
                      (#\[
                       (incf square))
                      (#\]
                       (decf square))
                      (#\(
                       (incf paren))
                      (#\)
                       (decf paren))))))
          finally (return nil))))

(defun graph-split-top (text char)
  "Split TEXT at each CHAR outside quotes, brackets, and parentheses."
  (let ((at (graph-top-position text char)))
    (if at (cons (subseq text 0 at) (graph-split-top (subseq text (1+ at)) char)) (list text))))

(defun graph-unquote (text)
  "Trim TEXT and remove one pair of surrounding double quotes."
  (let ((s (trim-text text)))
    (if (and (> (length s) 1) (char= (char s 0) #\") (char= (char s (1- (length s))) #\"))
        (subseq s 1 (1- (length s))) s)))

(defun graph-node-spec (text table order &key forced-shape)
  "Return the node TEXT declares, creating it in TABLE and ORDER on first use.

TEXT is an identifier with an optional shaped label. FORCED-SHAPE overrides the
default rectangle. A later explicit label replaces an implicit one."
  (let* ((s (trim-text text)) (end 0))
    (loop while (and (< end (length s)) (not (find (char s end) '(#\Space #\Tab #\[ #\] #\( #\) #\{ #\})))) do (incf end))
    (when (zerop end) (graph-fail 'invalid-diagram "Missing node identifier."))
    (let* ((id (subseq s 0 end)) (tail (trim-text (subseq s end))) (shape (or forced-shape :rectangle)) (label id) (explicit nil))
      (unless (zerop (length tail))
        (setf explicit t)
        (cond ((and (text-prefix-p "([" tail) (>= (length tail) 4) (char= (char tail (- (length tail) 2)) #\]))
               (setf shape :stadium label (subseq tail 2 (- (length tail) 2))))
              ((char= (char tail 0) #\[)
               (unless (char= (char tail (1- (length tail))) #\]) (graph-fail 'invalid-diagram "Unclosed node label.")) (setf label (subseq tail 1 (1- (length tail)))))
              ((char= (char tail 0) #\()
               (unless (char= (char tail (1- (length tail))) #\)) (graph-fail 'invalid-diagram "Unclosed node label.")) (setf shape :rounded label (subseq tail 1 (1- (length tail)))))
              ((char= (char tail 0) #\{)
               (unless (char= (char tail (1- (length tail))) #\}) (graph-fail 'invalid-diagram "Unclosed node label.")) (setf shape :diamond label (subseq tail 1 (1- (length tail)))))
              (t
               (graph-fail 'unsupported-syntax (format nil "Unsupported node syntax ~a." tail))))
        (setf label (graph-unquote label)))
      (let ((node (gethash id table)))
        (if node
            (when (and explicit (graph-node-explicit-label-p node) (not (string= label (graph-node-label node)))) (graph-fail 'invalid-diagram "Conflicting explicit labels."))
            (progn
              (when (>= (hash-table-count table) *maximum-nodes*) (graph-fail 'resource-limit-exceeded "Too many nodes."))
              (setf node (make-instance 'graph-node :id id :label label :shape shape :explicit-label-p explicit))
              (setf (gethash id table) node) (vector-push-extend node order)))
        (when (and node explicit (not (graph-node-explicit-label-p node))) (setf (graph-node-label node) label (graph-node-shape node) shape (graph-node-explicit-label-p node) t))
        (when (and *graph-current-group* (not (typep node 'graph-subgraph))
                   (null (graph-node-parent node)))
          (setf (graph-node-parent node) *graph-current-group*))
        node))))

(defun graph-node-group (text table order)
  "Return the nodes TEXT declares, one for each part joined by &."
  (mapcar (lambda (part) (graph-node-spec part table order)) (graph-split-top text #\&)))

(defparameter *graph-relations*
  (append (loop for left in '("||" "|o" "}o" "}|")
                append (loop for line in '("--" "..")
                             append (loop for right in '("||" "o|" "o{" "|{")
                                          collect (concatenate 'string left line right))))
          '("<|--" "--|>" "*--" "o--" "..>" "--.." "-.->" "-.-" "-->" "---"))
  "The relation tokens of flowchart, class, and ER statements, tried in this order at each position.")

(defun graph-relation-at (text)
  "Return the first relation outside quoted or bracketed node labels."
  (let ((quote nil) (escape nil) (nesting 0))
    (loop for i below (length text) for character = (char text i)
          do (cond
               (escape
                (setf escape nil))
               ((and quote (char= character #\\))
                (setf escape t))
               ((char= character #\")
                (setf quote (not quote)))
               ((not quote)
                (when (zerop nesting)
                  (dolist (relation *graph-relations*)
                    (let ((end (+ i (length relation))))
                      (when (and (<= end (length text))
                                 (string= relation text :start2 i :end2 end))
                        (return-from graph-relation-at (values i relation))))))
                (cond ((find character "[({")
                       (incf nesting))
                      ((find character "])}")
                       (decf nesting)))))
          finally (return (values nil nil)))))

(defun graph-flow-flat (source family &optional groups)
  "Resolve scoped flowchart statements against one shared node namespace."
  (let* ((statements (if (listp source) source (graph-statements source))) (header (first statements)) (table (make-hash-table :test #'equal)) (order (make-array 0 :adjustable t :fill-pointer 0)) (edges nil) (direction :td))
    (unless (and header (or (graph-header-p header "flowchart") (graph-header-p header "graph"))) (graph-fail 'invalid-diagram "Graph header is missing."))
    (setf direction (graph-direction-from-text (subseq header (if (text-prefix-p "flowchart" header) 9 5))))
    (dolist (group groups)
      (setf (gethash (graph-node-id group) table) group)
      (vector-push-extend group order))
    (dolist (entry (rest statements))
      (let* ((*graph-current-group* (and (consp entry) (first entry)))
             (statement             (if (consp entry) (rest entry) entry))
             (cursor                statement))
        (loop
          (multiple-value-bind (at relation) (graph-relation-at cursor)
            (if (null at)
                (progn (graph-node-group cursor table order) (return))
                (let ((left (graph-node-group (subseq cursor 0 at) table order)) (right (subseq cursor (+ at (length relation)))) (label nil))
                  (when (and (plusp (length right)) (char= (char right 0) #\|))
                    (let ((end (position #\| right :start 1))) (unless end (graph-fail 'invalid-diagram "Unclosed edge label.")) (setf label (trim-text (subseq right 1 end)) right (trim-text (subseq right (1+ end))))))
                  (multiple-value-bind (next ignored) (graph-relation-at right)
                    (declare (ignore ignored))
                    (let ((right-nodes (graph-node-group (if next (subseq right 0 next) right) table order)))
                      (unless (member relation '("-->" "---" "-.->" "-.-") :test #'string=)
                        (graph-fail 'unsupported-syntax "Unsupported flowchart relation."))
                      (dolist (from left)
                        (dolist (to right-nodes)
                          (when (>= (length edges) *maximum-edges*)
                            (graph-fail 'resource-limit-exceeded "Too many edges."))
                          (push (make-instance 'graph-edge :from from :to to
                                               :kind (cond ((string= relation "---")
                                                            :line)
                                                           ((string= relation "-.-")
                                                            :dotted-line)
                                                           ((string= relation "-.->")
                                                            :dotted)
                                                           (t
                                                            :arrow))
                                               :label label :relation relation) edges)))
                      (if next (setf cursor right) (return))))))))))
    (make-instance 'graph-diagram :family family :direction direction :nodes (coerce order 'list) :edges (nreverse edges))))

(defmethod diagram-parse ((kind (eql :flowchart)) source)
  "Parse SOURCE as a Mermaid flowchart."
  (graph-flow source :flowchart))

(defmethod diagram-parse ((kind (eql :graph)) source)
  "Parse SOURCE as a Mermaid graph."
  (graph-flow source :graph))

(defun parse-state (source)
  "Parse SOURCE as a Mermaid state diagram."
  (let ((table (make-hash-table :test #'equal))
        (order (make-array 0 :adjustable t :fill-pointer 0))
        (edges nil) (header nil) (direction :td))
    (dolist (statement (graph-statements source))
      (cond
        ((or (string= statement "stateDiagram")
             (string= statement "stateDiagram-v2"))
         (setf header t))
        ((not header)
         (graph-fail 'invalid-diagram "State header is missing."))
        ((graph-header-p statement "direction")
         (setf direction (graph-direction-from-text (subseq statement 9))))
        ((text-prefix-p "state " statement)
         (let* ((rest (trim-text (subseq statement 6)))
                (as   (search " as " rest)))
           (unless as (graph-fail 'unsupported-syntax "State declaration requires an alias."))
           (let ((label (graph-unquote (subseq rest 0 as)))
                 (id    (trim-text (subseq rest (+ as 4)))))
             (graph-node-spec (format nil "~a[~a]" id label) table order))))
        ((search "-->" statement)
         (let* ((at    (search "-->" statement))
                (left  (trim-text (subseq statement 0 at)))
                (right (trim-text (subseq statement (+ at 3))))
                (colon (graph-top-position right #\:))
                (label nil))
           (when colon
             (setf label (trim-text (subseq right (1+ colon)))
                   right (trim-text (subseq right 0 colon))))
           (let ((from (graph-node-spec (if (string= left "[*]") "__initial__" left)
                                        table order
                                        :forced-shape (when (string= left "[*]") :initial)))
                 (to   (graph-node-spec (if (string= right "[*]") "__final__" right)
                                        table order
                                        :forced-shape (when (string= right "[*]") :final))))
             (when (string= left "[*]") (setf (graph-node-label from) "[*]"))
             (when (string= right "[*]") (setf (graph-node-label to) "[*]"))
             (push (make-instance 'graph-edge :from from :to to
                                  :label label :relation "-->") edges))))
        (t
         (graph-fail 'unsupported-syntax
                     (format nil "Unsupported state syntax ~a." statement)))))
    (unless header (graph-fail 'invalid-diagram "State header is missing."))
    (make-instance 'graph-diagram :family :state :direction direction
                   :nodes (coerce order 'list) :edges (nreverse edges))))

(defmethod diagram-parse ((kind (eql :state)) source)
  "Parse SOURCE as a Mermaid state diagram."
  (parse-state source))

(defun graph-entity-kind (relation)
  "Return the edge kind keyword for the class diagram RELATION token."
  (cond ((search ".." relation)
         (if (search "--" relation) :dotted-line :dotted))
        ((find #\* relation)
         :composition)
        ((find #\o relation)
         :aggregation)
        ((find #\| relation)
         :inheritance)
        (t
         :association)))

(defun parse-entity (source family)
  "Parse class or ER declarations, closed member blocks, and relations."
  (let ((table (make-hash-table :test #'equal))
        (order (make-array 0 :adjustable t :fill-pointer 0))
        (edges nil) (header nil) (current nil))
    (dolist (statement (graph-statements source))
      (cond
        ((not header)
         (unless (string= statement (if (eq family :class) "classDiagram" "erDiagram"))
           (graph-fail 'invalid-diagram "Entity header is missing."))
         (setf header t))
        ((string= statement "}")
         (unless current (graph-fail 'invalid-diagram "Unexpected closing brace."))
         (setf current nil))
        (current
         (when (or (find #\{ statement) (find #\} statement))
           (graph-fail 'unsupported-syntax "Nested or malformed entity blocks are unsupported."))
         (push statement (graph-node-members current)))
        ((or (and (eq family :class) (text-prefix-p "class " statement))
             (and (eq family :er) (position #\{ statement)
                  (null (nth-value 0 (graph-relation-at statement)))))
         (let* ((declaration (if (eq family :class) (subseq statement 6) statement))
                (brace       (position #\{ declaration))
                (node        (graph-node-spec (if brace (subseq declaration 0 brace) declaration) table order)))
           (when brace
             (unless (zerop (length (trim-text (subseq declaration (1+ brace)))))
               (graph-fail 'unsupported-syntax "Member blocks require a separate closing brace."))
             (setf current node))))
        (t
         (multiple-value-bind (at relation) (graph-relation-at statement)
           (unless at (graph-fail 'unsupported-syntax (format nil "Unsupported entity syntax ~a." statement)))
           (let* ((left  (trim-text (subseq statement 0 at)))
                  (right (trim-text (subseq statement (+ at (length relation)))))
                  (colon (graph-top-position right #\:)) (label nil))
             (when colon
               (setf label (trim-text (subseq right (1+ colon)))
                     right (trim-text (subseq right 0 colon))))
             (let ((from (graph-node-spec left table order)) (to (graph-node-spec right table order)))
              (push (make-instance 'graph-edge :from from :to to
                                   :kind (if (eq family :er)
                                             (if (search ".." relation) :dotted-line :association)
                                             (graph-entity-kind relation))
                                   :label label :relation relation) edges)))))))
    (unless header (graph-fail 'invalid-diagram "Entity header is missing."))
    (when current (graph-fail 'invalid-diagram "Unclosed entity member block."))
    (make-instance 'graph-diagram :family family :direction :lr
                   :nodes (coerce order 'list) :edges (nreverse edges))))

(defmethod diagram-parse ((kind (eql :class)) source)
  "Parse SOURCE as a Mermaid class diagram."
  (parse-entity source :class))

(defmethod diagram-parse ((kind (eql :er)) source)
  "Parse SOURCE as a Mermaid entity relationship diagram."
  (parse-entity source :er))
