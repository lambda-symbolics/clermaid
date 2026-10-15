(in-package #:clermaid)

;;;; -- Terminal Cell Canvas --

(defstruct (graph-canvas (:constructor graph--make-canvas (width height text lines dashed blocked)))
  "A bounded cell canvas with separate text, connection masks, and obstacles."
  width height text lines dashed blocked)

(defun graph--canvas (width height)
  "Allocate a bounded WIDTH by HEIGHT canvas before routing any edges."
  (when (or (> width *maximum-output-columns*) (> height *maximum-output-rows*))
    (error 'resource-limit-exceeded :reason "Graph layout exceeds the terminal canvas limit."))
  (graph--make-canvas width height
                      (make-array (list height width) :initial-element nil)
                      (make-array (list height width) :element-type '(unsigned-byte 4)
                                                       :initial-element 0)
                      (make-array (list height width) :element-type 'bit :initial-element 0)
                      (make-array (list height width) :element-type 'bit :initial-element 0)))

(defun graph--put-text (canvas x y text &key blocked)
  "Place TEXT at cell X,Y without splitting a grapheme or losing wide cells."
  (loop for start = 0 then end
        while (< start (length text))
        for end = (grapheme-next-boundary text start)
        for cells = (grapheme-cell-width text start end)
        do (when (or (zerop cells) (< x 0) (< y 0)
                     (> (+ x cells) (graph-canvas-width canvas))
                     (>= y (graph-canvas-height canvas)))
             (error 'unsupported-syntax :reason "Graph text cannot be placed in terminal cells."))
           (setf (aref (graph-canvas-text canvas) y x) (subseq text start end))
           (loop for column from (1+ x) below (+ x cells)
                 do (setf (aref (graph-canvas-text canvas) y column) ':continuation))
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
     (aref #(" " "│" "─" "└" "│" "│" "┌" "├"
              "─" "┘" "─" "┴" "┐" "┤" "┬" "┼") mask))
    ((zerop mask) " ")
    ((zerop (logand mask 10)) "|")
    ((zerop (logand mask 5)) "-")
    (t "+")))

(defun graph--canvas-rows (canvas unicode)
  "Serialize CANVAS as grapheme-safe rows, omitting unused trailing space."
  (loop for y below (graph-canvas-height canvas)
        collect (string-right-trim
                 '(#\Space)
                 (with-output-to-string (out)
                   (loop for x below (graph-canvas-width canvas)
                         for text = (aref (graph-canvas-text canvas) y x)
                         do (cond
                              ((eq text ':continuation) nil)
                              (text (write-string text out))
                              (t (write-string
                                  (graph--line-glyph
                                   (aref (graph-canvas-lines canvas) y x) unicode
                                   (= 1 (aref (graph-canvas-dashed canvas) y x))) out))))))))

;;;; -- Node Placement --

(defstruct (graph-placement (:constructor graph--placement (node rows)))
  "One node's rendered box and its terminal-cell rectangle."
  node rows (x 0) (y 0) (width 0) (height 0))

(defun graph--node-rows (node unicode port-count)
  "Return a shaped box with intact labels and room for distinct connection ports."
  (when (member (graph-node-shape node) '(:initial :final))
    (return-from graph--node-rows
      (list "   " (if (eq (graph-node-shape node) ':initial)
                       (if unicode " ● " " * ")
                       (if unicode " ◉ " "(*)")) "   ")))
  (let* ((members (reverse (graph-node-members node)))
         (labels (cons (graph-node-label node) members))
         (inner (max 1 (- port-count 2)
                     (loop for text in labels maximize (text-cell-width text))))
         (shape (graph-node-shape node))
         (horizontal (if unicode #\─ #\-))
         (vertical (if unicode "│" "|"))
         (corners (case shape
                    ((:rounded :stadium) (if unicode '("╭" "╮" "╰" "╯") '("(" ")" "(" ")")))
                    ((:decision :diamond) '("/" "\\" "\\" "/"))
                    (otherwise (if unicode '("┌" "┐" "└" "┘") '("+" "+" "+" "+")))))
         (body-count (+ 1 (length members) (if members 1 0)))
         (padding (max 0 (- port-count body-count))))
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
                               (let* ((degree (loop for edge in (graph-edges diagram)
                                                    sum (+ (if (eq node (graph-edge-from edge)) 1 0)
                                                           (if (eq node (graph-edge-to edge)) 1 0))))
                                      (rows (graph--node-rows node unicode degree))
                                      (placement (graph--placement node rows)))
                                 (setf (graph-placement-width placement) (text-cell-width (first rows))
                                       (graph-placement-height placement) (length rows))
                                 placement))
                             (graph-nodes diagram)))
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

(defun graph--port (placement side counts)
  "Allocate a distinct boundary port, starting at the center of SIDE."
  (let* ((key (list placement side))
         (index (gethash key counts 0))
         (x (graph-placement-x placement)) (y (graph-placement-y placement))
         (width (graph-placement-width placement)) (height (graph-placement-height placement))
         (available (- (if (member side '(:north :south)) width height) 2))
         (center (ceiling available 2))
         (offsets (loop for delta below available
                        append (if (zerop delta) (list center)
                                   (remove-if-not (lambda (offset) (<= 1 offset available))
                                                  (list (+ center delta) (- center delta))))))
         (offset (nth index offsets)))
    (unless offset
      (error 'resource-limit-exceeded :reason "A graph node has too many connection ports."))
    (incf (gethash key counts 0))
    (ecase side
      (:north (cons (+ x offset) y))
      (:east (cons (+ x width -1) (+ y offset)))
      (:south (cons (+ x offset) (+ y height -1)))
      (:west (cons x (+ y offset))))))

(defun graph--outside-port (port side)
  "Return the cell immediately outside PORT along SIDE."
  (cons (+ (first port) (case side (:east 1) (:west -1) (otherwise 0)))
        (+ (rest port) (case side (:south 1) (:north -1) (otherwise 0)))))

(defun graph--route (canvas start end)
  "Route around boxes and existing bends; cross straight edges perpendicularly.
Track the incoming direction so a crossing cannot turn into an accidental
junction or reuse another edge's track. Each route search has four states per cell."
  (let* ((width (graph-canvas-width canvas))
         (height (graph-canvas-height canvas))
         (size (* 4 width height))
         (previous (make-array size :element-type 'fixnum :initial-element -1))
         (queue (make-array size :element-type 'fixnum))
         (head 0) (tail 0) (found nil)
         (start-cell (+ (first start) (* width (rest start))))
         (end-cell (+ (first end) (* width (rest end))))
         (start-state (* 4 start-cell))
         (steps #((1 . 0) (0 . 1) (-1 . 0) (0 . -1))))
    (setf (aref previous start-state) start-state
          (aref queue tail) start-state)
    (incf tail)
    (loop while (< head tail)
          for current = (aref queue head)
          for cell = (floor current 4)
          for direction = (mod current 4)
          for x = (mod cell width)
          for y = (floor cell width)
          for current-mask = (aref (graph-canvas-lines canvas) y x)
          do (incf head)
             (when (= cell end-cell) (setf found current) (return))
             (loop for next-direction below 4
                   for step = (aref steps next-direction)
                   for next-x = (+ x (first step))
                   for next-y = (+ y (rest step))
                   when (and (or (= cell start-cell) (zerop current-mask)
                                 (= direction next-direction))
                             (<= 0 next-x) (< next-x width)
                             (<= 0 next-y) (< next-y height)
                             (zerop (aref (graph-canvas-blocked canvas) next-y next-x)))
                     do (let* ((mask (aref (graph-canvas-lines canvas) next-y next-x))
                               (next (+ next-direction (* 4 (+ next-x (* width next-y))))))
                          (when (and (= -1 (aref previous next))
                                     (or (zerop mask)
                                         (and (= mask 5) (evenp next-direction))
                                         (and (= mask 10) (oddp next-direction))))
                            (setf (aref previous next) current
                                  (aref queue tail) next)
                            (incf tail)))))
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
    (when dashed
      (setf (aref (graph-canvas-dashed canvas) (rest from) (first from)) 1
            (aref (graph-canvas-dashed canvas) (rest to) (first to)) 1))))

(defun graph--draw-edge (canvas edge &key placements horizontal unicode port-counts)
  "Connect EDGE's two placed nodes and return its routed cell coordinates."
  (let* ((from (find (graph-edge-from edge) placements :key #'graph-placement-node))
         (to (find (graph-edge-to edge) placements :key #'graph-placement-node))
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
         (start-port (graph--port from from-side port-counts))
         (end-port (graph--port to to-side port-counts))
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
          (aref (graph-canvas-text canvas) (rest end-port) (first end-port))
          (if (member (graph-edge-kind edge) '(:line :dotted-line :association :composition :aggregation :inheritance))
              (if unicode (ecase to-side (:north "┴") (:east "├") (:south "┬") (:west "┤")) "+")
              (ecase to-side
                (:north (if unicode "▼" "v")) (:east (if unicode "◀" "<"))
                (:south (if unicode "▲" "^")) (:west (if unicode "▶" ">")))))
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

(defmethod diagram-render ((diagram graph-diagram) &key (width 120) (unicode t))
  "Render each node once and route actual orthogonal connections around its box."
  (check-diagram-size (length (graph-nodes diagram)) (length (graph-edges diagram)))
  (multiple-value-bind (placements required-width required-height)
      (graph--place-nodes diagram unicode)
    (when (> required-width width)
      (error 'diagram-too-wide :width width :required-width required-width
             :reason "Graph nodes do not fit the available terminal width."))
    (let ((canvas (graph--canvas required-width required-height))
          (horizontal (not (null (member (graph-direction diagram) '(:lr :rl)))))
          (port-counts (make-hash-table :test #'equal))
          (routes nil) (legend nil))
      (dolist (placement placements)
        (loop for row in (graph-placement-rows placement)
              for y from (graph-placement-y placement)
              do (graph--put-text canvas (graph-placement-x placement) y row :blocked t)))
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
      (let* ((rows (graph--canvas-rows canvas unicode))
             (trimmed (member-if (lambda (row) (plusp (length row)))
                                 (reverse (member-if (lambda (row) (plusp (length row)))
                                                     (reverse rows))))))
        (check-output-size (append trimmed (when legend (cons "" (nreverse legend)))) width)))))
