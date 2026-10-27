(in-package #:clermaid)

(defun graph--validate-subgraph-edges (diagram)
  "Reject edges between a group and any of its descendants before layout."
  (flet ((contains-p (group node)
           (and (typep group 'graph-subgraph)
                (loop for parent = (graph-node-parent node) then (graph-node-parent parent)
                      while parent thereis (eq parent group)))))
    (dolist (edge (graph-edges diagram))
      (let ((from (graph-edge-from edge)) (to (graph-edge-to edge)))
        (when (or (contains-p from to) (contains-p to from))
          (graph-fail 'invalid-diagram
                      "Edges between a subgraph and its descendants are not supported.")))))
  diagram)

(defun graph-flow (source family)
  "Parse balanced subgraph scopes before resolving node and group endpoints."
  (let* ((statements (graph-statements source))
         (body nil) (groups nil) (stack nil)
         (ids        (make-hash-table :test #'equal)))
    (dolist (statement (rest statements))
      (cond
        ((graph-header-p statement "subgraph")
         (when (>= (length stack) *maximum-subgraph-depth*)
           (graph-fail 'resource-limit-exceeded "Subgraph nesting is too deep."))
         (when (>= (length groups) *maximum-nodes*)
           (graph-fail 'resource-limit-exceeded "Too many subgraphs."))
         (let* ((title   (trim-text (subseq statement 8)))
                (bracket (graph-top-position title #\[))
                (id      (if bracket (trim-text (subseq title 0 bracket)) (graph-unquote title)))
                (label   (if bracket
                             (progn
                               (unless (and (plusp (length title))
                                            (char= (char title (1- (length title))) #\]))
                                 (graph-fail 'invalid-diagram "Invalid subgraph label."))
                               (graph-unquote (subseq title (1+ bracket) (1- (length title)))))
                             (graph-unquote title))))
           (when (or (zerop (length id)) (gethash id ids))
             (graph-fail 'invalid-diagram "Missing or duplicate subgraph identifier."))
           (when (and bracket (find-if (lambda (c) (find c '(#\Space #\Tab #\"))) id))
             (graph-fail 'invalid-diagram "Invalid subgraph identifier."))
           (let ((group (make-instance 'graph-subgraph :id id :label label :explicit-label-p t)))
             (setf (graph-node-parent group) (first stack)
                   (gethash id ids) group)
             (push group groups)
             (push group stack))))
        ((string= statement "end")
         (unless stack (graph-fail 'invalid-diagram "Subgraph end has no opening declaration."))
         (pop stack))
        ((graph-header-p statement "direction")
         (unless stack (graph-fail 'invalid-diagram "Direction must be inside a subgraph."))
         (let ((value (trim-text (subseq statement 9))))
           (when (zerop (length value)) (graph-fail 'invalid-diagram "Missing subgraph direction."))
           (setf (graph-subgraph-direction (first stack)) (graph-direction-from-text value))))
        (t
         (push (cons (first stack) statement) body))))
    (when stack (graph-fail 'invalid-diagram "Unclosed subgraph."))
    (graph--validate-subgraph-edges
     (graph-flow-flat (cons (first statements) (nreverse body)) family (nreverse groups)))))

(defun graph--scope-child (node parent)
  "Return the direct child of PARENT that contains NODE, or NIL outside it."
  (loop for current = node then (graph-node-parent current)
        while current
        when (eq (graph-node-parent current) parent) return current))

(defun graph--place-subgraphs (diagram unicode)
  "Lay out each scope as indivisible child rectangles, then translate descendants."
  (let ((*graph-sizing-edges* (graph-edges diagram))
        (*graph-group-sizes*  (make-hash-table :test #'eq))
        (contents             (make-hash-table :test #'eq)))
    (labels ((scope (parent direction)
               (let* ((nodes (remove-if-not (lambda (node) (eq (graph-node-parent node) parent))
                                            (graph-nodes diagram)))
                      (edges nil))
                 (dolist (node nodes)
                   (when (typep node 'graph-subgraph)
                     (multiple-value-bind (children width height)
                         (scope node (or (graph-subgraph-direction node) direction))
                       (let ((degree (loop for edge in (graph-edges diagram)
                                           sum (+ (if (eq node (graph-edge-from edge)) 1 0)
                                                  (if (eq node (graph-edge-to edge)) 1 0)))))
                         (setf (gethash node contents) children
                               (gethash node *graph-group-sizes*)
                               (cons (max (+ width 6) (+ 4 (text-cell-width (graph-node-label node)))
                                          (+ degree 2))
                                     (max (+ height 5) (+ degree 2))))))))
                 (dolist (edge (graph-edges diagram))
                   (let ((from (graph--scope-child (graph-edge-from edge) parent))
                         (to   (graph--scope-child (graph-edge-to edge) parent)))
                     (when (and from to (not (eq from to)))
                       (push (make-instance 'graph-edge :from from :to to) edges))))
                 (graph--place-nodes
                  (make-instance 'graph-diagram :family :flowchart :direction direction
                                 :nodes nodes :edges (nreverse edges)) unicode)))

             (expand (placements dx dy)
               (loop for placement in placements
                     for node = (graph-placement-node placement)
                     do (incf (graph-placement-x placement) dx)
                        (incf (graph-placement-y placement) dy)
                     append (cons placement
                                  (when (typep node 'graph-subgraph)
                                    (expand (gethash node contents)
                                            (+ 3 (graph-placement-x placement))
                                            (+ 3 (graph-placement-y placement))))))))
      (multiple-value-bind (placements width height) (scope nil (graph-direction diagram))
        (values (expand placements 0 0) width height)))))

(defun graph--draw-subgraph (canvas placement)
  "Draw a permeable group frame. Keep its title cells clear of routes."
  (let* ((x (graph-placement-x placement)) (y (graph-placement-y placement))
         (right  (+ x (graph-placement-width placement) -1))
         (bottom (+ y (graph-placement-height placement) -1))
         (lines  (graph-canvas-lines canvas))
         (roles  (graph-canvas-roles canvas)))
    (flet ((put (column row mask)
             (setf (aref lines row column) mask
                   (aref roles row column) ':mermaid-box)))
      (loop for column from (1+ x) below right do (put column y 10) (put column bottom 10))
      (loop for row from (1+ y) below bottom do (put x row 5) (put right row 5))
      (put x y 6) (put right y 12) (put x bottom 3) (put right bottom 9))
    (graph--put-text canvas (+ x 2) (1+ y)
                     (graph-node-label (graph-placement-node placement)) :blocked t)))
