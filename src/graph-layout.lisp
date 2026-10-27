(in-package #:clermaid)

;;;; -- Terminal Cell Canvas --

(defstruct (graph-canvas (:constructor graph--make-canvas
                                      (width height text roles lines dashed blocked)))
  "A bounded cell canvas with text, semantic roles, connections, and obstacles."
  width height text roles lines dashed blocked
  (reserved (make-hash-table :test #'equal)))

(defun graph--canvas (width height)
  "Allocate a bounded WIDTH by HEIGHT canvas before routing any edges."
  (when (or (> width *maximum-output-columns*) (> height *maximum-output-rows*))
    (error 'resource-limit-exceeded :reason "Graph layout exceeds the terminal canvas limit."))
  (graph--make-canvas width height
                      (make-array (list height width) :initial-element nil)
                      (make-array (list height width) :initial-element ':plain)
                      (make-array (list height width) :element-type '(unsigned-byte 4)
                                                       :initial-element 0)
                      (make-array (list height width) :element-type 'bit :initial-element 0)
                      (make-array (list height width) :element-type 'bit :initial-element 0)))

(defun graph--put-text (canvas x y text &key blocked (role ':mermaid-label))
  "Place TEXT at cell X,Y and record ROLE without splitting graphemes."
  (loop for start = 0 then end
        while (< start (length text))
        for end = (grapheme-next-boundary text start)
        for cells = (grapheme-cell-width text start end)
        do (when (or (zerop cells) (< x 0) (< y 0)
                     (> (+ x cells) (graph-canvas-width canvas))
                     (>= y (graph-canvas-height canvas)))
             (error 'unsupported-syntax :reason "Graph text cannot be placed in terminal cells."))
           (setf (aref (graph-canvas-text canvas) y x) (subseq text start end))
           (setf (aref (graph-canvas-roles canvas) y x) role)
           (loop for column from (1+ x) below (+ x cells)
                 do (setf (aref (graph-canvas-text canvas) y column) ':continuation
                          (aref (graph-canvas-roles canvas) y column) role))
           (when blocked
             (loop for column from x below (+ x cells)
                   do (setf (aref (graph-canvas-blocked canvas) y column) 1)))
           (incf x cells)))

(defun graph--line-glyph (mask unicode dashed)
  "Return the connection glyph for MASK (north=1,east=2,south=4,west=8)."
  (cond
    ((and dashed (not (zerop mask)) (zerop (logand mask 10)))
     (if unicode "┆" ":"))
    ((and dashed (not (zerop mask)) (zerop (logand mask 5)))
     (if unicode "┄" "."))
    (unicode
     (aref #(" " "╵" "╶" "╰" "╷" "│" "╭" "├"
              "╴" "╯" "─" "┴" "╮" "┤" "┬" "┼") mask))
    ((zerop mask) " ")
    ((zerop (logand mask 10)) "|")
    ((zerop (logand mask 5)) "-")
    (t "+")))

(defun graph--canvas-rows (canvas unicode)
  "Return plain rows and matching role/text rows, trimming unused trailing cells."
  (let ((rows nil)
        (semantic-rows nil))
    (dotimes (y (graph-canvas-height canvas))
      (let ((spans nil)
            (pieces nil)
            (current-role nil))
        (flet ((flush ()
                 (when pieces
                   (push (cons current-role (apply #'concatenate 'string (nreverse pieces))) spans)
                   (setf pieces nil))))
          (dotimes (x (graph-canvas-width canvas))
            (let* ((text (aref (graph-canvas-text canvas) y x))
                   (mask (aref (graph-canvas-lines canvas) y x)))
              (unless (eq text ':continuation)
                (let ((role (cond (text (aref (graph-canvas-roles canvas) y x))
                                  ((plusp mask) (if (eq (aref (graph-canvas-roles canvas) y x) ':mermaid-box)
                                                   ':mermaid-box ':mermaid-edge))
                                  (t ':plain)))
                      (piece (or text (graph--line-glyph mask unicode
                                                       (= 1 (aref (graph-canvas-dashed canvas) y x))))))
                  (unless (eq current-role role)
                    (flush)
                    (setf current-role role))
                  (push piece pieces)))))
          (flush))
        (loop while spans
              for trimmed = (string-right-trim '(#\Space) (rest (first spans)))
              do (if (zerop (length trimmed))
                     (pop spans)
                     (progn (setf (rest (first spans)) trimmed) (return))))
        (setf spans (nreverse spans))
        (push (apply #'concatenate 'string (mapcar #'rest spans)) rows)
        (push spans semantic-rows)))
    (values (nreverse rows) (nreverse semantic-rows))))

;;;; -- Node Placement --

(defstruct (graph-placement (:constructor graph--placement (node rows)))
  "One node's rendered box and its terminal-cell rectangle."
  node rows (x 0) (y 0) (width 0) (height 0))

(defparameter *graph-group-sizes* nil
  "Dynamically computed child scope sizes during grouped layout.")

(defparameter *graph-sizing-edges* nil
  "All edges used to size ports across scope boundaries.")

(defun graph--node-rows (node unicode port-count &key horizontal-p (self-count 0))
  "Return an intact box with port space only on the sides used by this direction."
  (when (typep node 'graph-subgraph)
    (let ((size (gethash node *graph-group-sizes*)))
      (when (or (> (car size) *maximum-output-columns*)
                (> (cdr size) *maximum-output-rows*))
        (graph-fail 'resource-limit-exceeded "Subgraph exceeds the terminal canvas limit."))
      (return-from graph--node-rows
        (make-list (cdr size) :initial-element (make-string (car size) :initial-element #\Space)))))
  (when (member (graph-node-shape node) '(:initial :final))
    (return-from graph--node-rows
      (list "   " (if (eq (graph-node-shape node) ':initial)
                       (if unicode " ● " " * ")
                       (if unicode " ◉ " "(*)")) "   ")))
  (let* ((members (reverse (graph-node-members node)))
         (labels (cons (graph-node-label node) members))
         (inner (max 1 (- (if horizontal-p self-count port-count) 2)
                     (loop for text in labels maximize (text-cell-width text))))
         (shape (graph-node-shape node))
         (horizontal (if unicode #\─ #\-))
         (vertical (if unicode "│" "|"))
         (corners (case shape
                    ((:rounded :stadium) (if unicode '("╭" "╮" "╰" "╯") '("(" ")" "(" ")")))
                    ((:decision :diamond) '("/" "\\" "\\" "/"))
                    (otherwise (if unicode '("┌" "┐" "└" "┘") '("+" "+" "+" "+")))))
         (body-count (+ 1 (length members) (if members 1 0)))
         (padding (max 0 (- (if horizontal-p port-count self-count) body-count))))
    (when (or (> (+ inner 4) *maximum-output-columns*)
              (> (+ body-count padding 2) *maximum-output-rows*))
      (error 'resource-limit-exceeded :reason "A graph node exceeds the terminal canvas limit."))
    (let ((bar (make-string (+ inner 2) :initial-element horizontal)))
      (append
       (list (concatenate 'string (first corners) bar (second corners))
             (format nil "~a ~a ~a"
                     (if (member shape '(:decision :diamond)) "<" vertical)
                     (pad-text (first labels) inner)
                     (if (member shape '(:decision :diamond)) ">" vertical)))
       (when members
         (cons (format nil "~a~a~a" (if unicode "├" "+") bar (if unicode "┤" "+"))
               (mapcar (lambda (text) (format nil "~a ~a ~a" vertical (pad-text text inner) vertical))
                       members)))
       (loop repeat padding collect (format nil "~a~a~a" vertical
                                           (make-string (+ inner 2) :initial-element #\Space)
                                           vertical))
       (list (concatenate 'string (third corners) bar (fourth corners)))))))

(defun graph--node-ranks (diagram)
  "Return deterministic topological ranks, breaking cycles in declaration order."
  (let ((ranks (make-hash-table :test #'eq))
        (incoming (make-hash-table :test #'eq))
        (processed (make-hash-table :test #'eq))
        (nodes (graph-nodes diagram)))
    (dolist (node nodes)
      (setf (gethash node ranks) 0 (gethash node incoming) 0))
    (dolist (edge (graph-edges diagram))
      (unless (eq (graph-edge-from edge) (graph-edge-to edge))
        (incf (gethash (graph-edge-to edge) incoming))))
    (loop with queue = (remove-if-not (lambda (node) (zerop (gethash node incoming))) nodes)
          while queue
          for node = (pop queue)
          do (setf (gethash node processed) t)
             (dolist (edge (graph-edges diagram))
               (when (and (eq node (graph-edge-from edge))
                          (not (eq node (graph-edge-to edge))))
                 (let ((target (graph-edge-to edge)))
                   (setf (gethash target ranks)
                         (max (gethash target ranks) (1+ (gethash node ranks))))
                   (when (zerop (decf (gethash target incoming)))
                     (setf queue (append queue (list target))))))))
    (let ((next (1+ (loop for node in nodes maximize (gethash node ranks) into rank
                          finally (return (or rank -1))))))
      (dolist (node nodes)
        (unless (gethash node processed)
          (setf (gethash node ranks) next)
          (incf next))))
    ranks))

(defun graph--place-nodes (diagram unicode)
  "Return placements and required canvas dimensions, respecting graph direction."
  (let* ((horizontal (not (null (member (graph-direction diagram) '(:lr :rl)))))
         (ranks (graph--node-ranks diagram))
         (placements (mapcar (lambda (node)
                               (let* ((degree (loop for edge in (or *graph-sizing-edges* (graph-edges diagram))
                                                    sum (+ (if (eq node (graph-edge-from edge)) 1 0)
                                                           (if (eq node (graph-edge-to edge)) 1 0))))
                                      (self-count (count-if
                                                   (lambda (edge)
                                                     (and (eq node (graph-edge-from edge))
                                                          (eq node (graph-edge-to edge))))
                                                   (graph-edges diagram)))
                                      (rows (graph--node-rows node unicode degree
                                                              :horizontal-p horizontal
                                                              :self-count (if *graph-sizing-edges* (max degree self-count) self-count)))
                                      (placement (graph--placement node rows)))
                                 (setf (graph-placement-width placement) (text-cell-width (first rows))
                                       (graph-placement-height placement) (length rows))
                                 placement))
                             (graph--ordered-nodes diagram ranks)))
         (maximum (loop for placement in placements
                        maximize (gethash (graph-placement-node placement) ranks) into rank
                        finally (return (or rank 0))))
         (primary 3)
         (secondary-maximum 0))
    (loop for rank from 0 to maximum
          for layer = (remove-if-not
                       (lambda (placement)
                         (= rank (gethash (graph-placement-node placement) ranks)))
                       placements)
          for primary-size = (loop for placement in layer
                                   maximize (if horizontal (graph-placement-width placement)
                                                (graph-placement-height placement)) into size
                                   finally (return (or size 0)))
          do (let ((secondary 3))
               (dolist (placement layer)
                 (setf (graph-placement-x placement) (if horizontal primary secondary)
                       (graph-placement-y placement) (if horizontal secondary primary))
                 (incf secondary (+ (if horizontal (graph-placement-height placement)
                                           (graph-placement-width placement))
                                   (if horizontal 4 6))))
               (setf secondary-maximum (max secondary-maximum secondary)))
             (incf primary (+ primary-size (if horizontal 8 5))))
    (loop for rank from 0 to maximum
          for layer = (remove-if-not
                       (lambda (placement) (= rank (gethash (graph-placement-node placement) ranks)))
                       placements)
          for used = (loop for placement in layer
                           maximize (if horizontal
                                        (+ (graph-placement-y placement) (graph-placement-height placement))
                                        (+ (graph-placement-x placement) (graph-placement-width placement)))
                             into end finally (return (or end 3)))
          for shift = (floor (- secondary-maximum (if horizontal 4 6) used) 2)
          do (dolist (placement layer)
               (if horizontal
                   (incf (graph-placement-y placement) shift)
                   (incf (graph-placement-x placement) shift))))
    (let ((canvas-width (if horizontal (max 7 (- primary 5)) (max 7 (- secondary-maximum 3))))
          (canvas-height (if horizontal (max 7 (- secondary-maximum 1)) (max 7 (- primary 2)))))
      (when (member (graph-direction diagram) '(:rl :bt))
        (dolist (placement placements)
          (if horizontal
              (setf (graph-placement-x placement)
                    (- canvas-width (graph-placement-x placement) (graph-placement-width placement)))
              (setf (graph-placement-y placement)
                    (- canvas-height (graph-placement-y placement) (graph-placement-height placement))))))
      (values placements canvas-width canvas-height))))

;;;; -- Orthogonal Edge Routing --

(defun graph--edge-horizontal (from to default)
  "Use separated rectangles to select ports in graphs with nested directions."
  (if (not (or (typep (graph-placement-node from) 'graph-subgraph)
               (typep (graph-placement-node to) 'graph-subgraph)
               (graph-node-parent (graph-placement-node from))
               (graph-node-parent (graph-placement-node to))))
      default
      (cond ((or (<= (+ (graph-placement-x from) (graph-placement-width from)) (graph-placement-x to))
                 (<= (+ (graph-placement-x to) (graph-placement-width to)) (graph-placement-x from))) t)
            ((or (<= (+ (graph-placement-y from) (graph-placement-height from)) (graph-placement-y to))
                 (<= (+ (graph-placement-y to) (graph-placement-height to)) (graph-placement-y from))) nil)
            (t default))))

(defun graph--outside-port (port side)
  "Return the cell immediately outside PORT along SIDE."
  (cons (+ (first port) (case side (:east 1) (:west -1) (otherwise 0)))
        (+ (rest port) (case side (:south 1) (:north -1) (otherwise 0)))))

(defun graph--route (canvas start end)
  "Minimize crossings first, then route length and bends without merging tracks.
Each cell has four arrival states so crossings continue straight through."
  (let* ((width (graph-canvas-width canvas))
         (height (graph-canvas-height canvas))
         (size (* 4 width height))
         (crossing-cost (1+ (* 8 size)))
         (previous (make-array size :element-type 'fixnum :initial-element -1))
         (distance (make-array size :element-type 'fixnum :initial-element most-positive-fixnum))
         (heap (make-array 128 :adjustable t :fill-pointer 0))
         (start-cell (+ (first start) (* width (rest start))))
         (end-cell (+ (first end) (* width (rest end))))
         (start-state (* 4 start-cell))
         (steps #((1 . 0) (0 . 1) (-1 . 0) (0 . -1)))
         (found nil))
    (labels ((enqueue (cost state)
               (let ((index (vector-push-extend (cons cost state) heap)))
                 (loop while (plusp index)
                       for parent = (floor (1- index) 2)
                       while (< cost (first (aref heap parent)))
                       do (rotatef (aref heap index) (aref heap parent))
                          (setf index parent))))

             (dequeue ()
               (let ((result (aref heap 0))
                     (last (vector-pop heap)))
                 (when (plusp (length heap))
                   (setf (aref heap 0) last)
                   (loop with index = 0
                         for left = (1+ (* 2 index))
                         while (< left (length heap))
                         for right = (1+ left)
                         for child = (if (and (< right (length heap))
                                              (< (first (aref heap right)) (first (aref heap left))))
                                         right left)
                         while (< (first (aref heap child)) (first (aref heap index)))
                         do (rotatef (aref heap child) (aref heap index))
                            (setf index child)))
                 result)))
      (unless (and (zerop (aref (graph-canvas-lines canvas) (rest start) (first start)))
                   (zerop (aref (graph-canvas-lines canvas) (rest end) (first end))))
        (error 'resource-limit-exceeded :reason "An edge approach cell already contains a track."))
      (setf (aref previous start-state) start-state
            (aref distance start-state) 0)
      (enqueue 0 start-state)
      (loop while (plusp (length heap))
            for entry = (dequeue)
            for cost = (first entry)
            for current = (rest entry)
            for cell = (floor current 4)
            for direction = (mod current 4)
            for x = (mod cell width)
            for y = (floor cell width)
            for current-mask = (aref (graph-canvas-lines canvas) y x)
            when (= cost (aref distance current))
              do (when (= cell end-cell) (setf found current) (return))
                 (loop for next-direction below 4
                       for step = (aref steps next-direction)
                       for next-x = (+ x (first step))
                       for next-y = (+ y (rest step))
                        when (and (or (zerop current-mask) (= direction next-direction))
                                  (<= 0 next-x) (< next-x width)
                                  (<= 0 next-y) (< next-y height)
                                  (zerop (aref (graph-canvas-blocked canvas) next-y next-x))
                                  (or (equal (cons next-x next-y) start)
                                      (equal (cons next-x next-y) end)
                                      (not (gethash (cons next-x next-y)
                                                    (graph-canvas-reserved canvas)))))
                         do (let* ((mask (aref (graph-canvas-lines canvas) next-y next-x))
                                   (next (+ next-direction (* 4 (+ next-x (* width next-y)))))
                                   (next-cost (+ cost 4 (if (or (= cell start-cell)
                                                                               (= direction next-direction)) 0 2)
                                                 (if (zerop mask) 0 crossing-cost))))
                              (when (and (< next-cost (aref distance next))
                                         (or (zerop mask)
                                             (and (= mask 5) (evenp next-direction))
                                             (and (= mask 10) (oddp next-direction))))
                                (setf (aref previous next) current
                                      (aref distance next) next-cost)
                                (enqueue next-cost next))))))
    (unless found
      (error 'resource-limit-exceeded :reason "Graph edges cannot be routed without merging tracks."))
    (loop with route = nil
          for current = found then (aref previous current)
          for cell = (floor current 4)
          do (push (cons (mod cell width) (floor cell width)) route)
          when (= current start-state) return route)))

(defun graph--connect (canvas from to &key dashed)
  "Draw a unit orthogonal connection between FROM and TO."
  (let* ((dx (- (first to) (first from))) (dy (- (rest to) (rest from)))
         (forward (cond ((plusp dx) 2) ((minusp dx) 8) ((plusp dy) 4) (t 1)))
         (backward (case forward (1 4) (2 8) (4 1) (8 2))))
    (setf (aref (graph-canvas-lines canvas) (rest from) (first from))
          (logior forward (aref (graph-canvas-lines canvas) (rest from) (first from)))
          (aref (graph-canvas-lines canvas) (rest to) (first to))
          (logior backward (aref (graph-canvas-lines canvas) (rest to) (first to))))
    (setf (aref (graph-canvas-roles canvas) (rest from) (first from)) ':mermaid-edge
          (aref (graph-canvas-roles canvas) (rest to) (first to)) ':mermaid-edge)
    (when dashed
      (setf (aref (graph-canvas-dashed canvas) (rest from) (first from)) 1
            (aref (graph-canvas-dashed canvas) (rest to) (first to)) 1))))

(defun graph--reserve-edge-approaches (canvas diagram placements ports)
  "Keep each edge's approach cells free of every other edge's tracks."
  (dolist (edge (graph-edges diagram))
    (dolist (endpoint '(:from :to))
      (let* ((node (if (eq endpoint ':from) (graph-edge-from edge) (graph-edge-to edge)))
             (placement (find node placements :key #'graph-placement-node))
             (port (gethash (list edge endpoint) ports))
             (side (cond ((= (first port) (graph-placement-x placement)) ':west)
                         ((= (rest port) (graph-placement-y placement)) ':north)
                         ((= (first port) (+ (graph-placement-x placement)
                                             (graph-placement-width placement) -1)) ':east)
                         (t ':south))))
        (setf (gethash (graph--outside-port port side) (graph-canvas-reserved canvas)) t)))))

(defun graph--draw-edge (canvas edge &key placements horizontal unicode port-counts)
  "Connect EDGE using assigned ports and record edge and arrow provenance."
  (let* ((from (find (graph-edge-from edge) placements :key #'graph-placement-node))
         (to (find (graph-edge-to edge) placements :key #'graph-placement-node))
         (horizontal (graph--edge-horizontal from to horizontal))
         (self-p (eq from to))
         (forward-p (if horizontal
                        (< (graph-placement-x from) (graph-placement-x to))
                        (< (graph-placement-y from) (graph-placement-y to))))
         (from-side (if self-p ':east
                        (if horizontal (if forward-p ':east ':west)
                            (if forward-p ':south ':north))))
         (to-side (if self-p ':north
                      (if horizontal (if forward-p ':west ':east)
                          (if forward-p ':north ':south))))
         (start-port (gethash (list edge ':from) port-counts))
         (end-port (gethash (list edge ':to) port-counts))
         (route (append (list start-port)
                        (graph--route canvas (graph--outside-port start-port from-side)
                                             (graph--outside-port end-port to-side))
                        (list end-port))))
    (loop for (point next) on route while next
          do (graph--connect canvas point next
                             :dashed (not (null (member (graph-edge-kind edge)
                                                        '(:dotted :dotted-line :dependency))))))
    (setf (aref (graph-canvas-text canvas) (rest start-port) (first start-port))
          (if unicode
              (ecase from-side (:north "┴") (:east "├") (:south "┬") (:west "┤"))
              "+")
          (aref (graph-canvas-roles canvas) (rest start-port) (first start-port)) ':mermaid-edge
          (aref (graph-canvas-text canvas) (rest end-port) (first end-port))
          (if (member (graph-edge-kind edge) '(:line :dotted-line :association :composition :aggregation :inheritance))
              (if unicode (ecase to-side (:north "┴") (:east "├") (:south "┬") (:west "┤")) "+")
              (ecase to-side
                (:north (if unicode "▼" "v")) (:east (if unicode "◀" "<"))
                (:south (if unicode "▲" "^")) (:west (if unicode "▶" ">"))))
          (aref (graph-canvas-roles canvas) (rest end-port) (first end-port))
          (if (member (graph-edge-kind edge) '(:line :dotted-line :association :composition :aggregation :inheritance))
              ':mermaid-edge ':mermaid-arrow))
    route))

(defun graph--place-edge-label (canvas route text)
  "Place TEXT beside an edge with a blank cell separating it from other content."
  (let ((cells (text-cell-width text)))
    (loop for (point next) on route
          for x = (first point) for y = (rest point)
          thereis
          (and next (= y (rest next))
               (loop for row in (list (1- y) (1+ y))
                     thereis
                     (when (and (<= 0 row) (< row (graph-canvas-height canvas))
                                (<= (+ x cells 1) (graph-canvas-width canvas))
                                (loop for column from (max 0 (1- x)) below (+ x cells 1)
                                      always (and (null (aref (graph-canvas-text canvas) row column))
                                                  (zerop (aref (graph-canvas-lines canvas) row column)))))
                       (graph--put-text canvas x row text)
                       t))))))

(defun graph--draw-node (canvas placement)
  "Draw the known frame and label regions with separate semantic roles."
  (let* ((node (graph-placement-node placement))
         (x (graph-placement-x placement))
         (y (graph-placement-y placement)))
    (loop for row in (graph-placement-rows placement)
          for row-y from y
          do (graph--put-text canvas x row-y row :blocked t :role ':mermaid-box))
    (unless (member (graph-node-shape node) '(:initial :final))
      (graph--put-text canvas (+ x 2) (1+ y) (graph-node-label node) :role ':mermaid-label)
      (loop for member in (reverse (graph-node-members node))
            for member-y from (+ y 3)
            do (graph--put-text canvas (+ x 2) member-y member :role ':mermaid-label)))))

(defmethod diagram-render ((diagram graph-diagram) &key (width 120) (unicode t))
  "Render each node once and route actual orthogonal connections around its box."
  (check-diagram-size (length (graph-nodes diagram)) (length (graph-edges diagram)))
  (multiple-value-bind (placements required-width required-height)
      (if (some (lambda (node) (typep node 'graph-subgraph)) (graph-nodes diagram))
          (graph--place-subgraphs diagram unicode)
          (graph--place-nodes diagram unicode))
    (when (> required-width width)
      (error 'diagram-too-wide :width width :required-width required-width
             :reason "Graph nodes do not fit the available terminal width."))
    (let ((canvas (graph--canvas required-width required-height))
          (horizontal (not (null (member (graph-direction diagram) '(:lr :rl)))))
          (port-counts (graph--assign-edge-ports diagram placements))
          (routes nil) (legend nil))
      (graph--reserve-edge-approaches canvas diagram placements port-counts)
      (dolist (placement placements)
        (if (typep (graph-placement-node placement) 'graph-subgraph)
            (graph--draw-subgraph canvas placement)
            (graph--draw-node canvas placement)))
      (dolist (edge (graph-edges diagram))
        (push (cons edge (graph--draw-edge canvas edge :placements placements
                                                       :horizontal horizontal
                                                       :unicode unicode
                                                       :port-counts port-counts)) routes))
      (dolist (entry (nreverse routes))
        (let* ((edge (first entry))
               (relation (and (member (graph-family diagram) '(:class :er))
                              (graph-edge-relation edge)))
               (label (graph-edge-label edge))
               (text (format nil "~@[~a~]~:[~; ~]~@[~a~]" relation (and relation label) label)))
          (when (plusp (length text))
            (unless (graph--place-edge-label canvas (rest entry) text)
              (push (format nil "~a -> ~a: ~a"
                            (graph-node-label (graph-edge-from edge))
                            (graph-node-label (graph-edge-to edge)) text)
                    legend)))))
      (multiple-value-bind (rows semantic-rows) (graph--canvas-rows canvas unicode)
        (let* ((start (or (position-if (lambda (row) (plusp (length row))) rows) 0))
               (last (position-if (lambda (row) (plusp (length row))) rows :from-end t))
               (end (if last (1+ last) 0))
               (labels (nreverse legend)))
          (values (check-output-size
                   (append (subseq rows start end) (when labels (cons "" labels))) width)
                  (append (subseq semantic-rows start end)
                          (when labels
                            (cons nil (mapcar (lambda (label) (list (cons ':mermaid-label label))) labels))))))))))
