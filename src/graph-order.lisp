(in-package #:clermaid)

;;;; -- Crossing Reduction and Connection Order --

(defun graph--ordered-nodes (diagram ranks)
  "Reduce layer crossings with stable barycentric sweeps, retaining the best order."
  (let* ((maximum   (loop for node in (graph-nodes diagram) maximize (gethash node ranks)))
         (layers    (make-array (1+ (or maximum 0)) :initial-element nil))
         (positions (make-hash-table :test #'eq))
         (edges     (graph-edges diagram)))
    (dolist (node (reverse (graph-nodes diagram)))
      (push node (aref layers (gethash node ranks))))
    (labels ((update-positions ()
               (loop for layer across layers
                     do (loop for node in layer for index from 0
                              do (setf (gethash node positions) (/ index (max 1 (1- (length layer))))))))

             (crossings ()
               (loop for (edge . remaining) on edges
                     for from = (graph-edge-from edge)
                     for to = (graph-edge-to edge)
                     sum (loop for other in remaining
                               for other-from = (graph-edge-from other)
                               for other-to = (graph-edge-to other)
                               count (and (= (gethash from ranks) (gethash other-from ranks))
                                          (= (gethash to ranks) (gethash other-to ranks))
                                          (< (* (- (gethash from positions) (gethash other-from positions))
                                                (- (gethash to positions) (gethash other-to positions))) 0)))))

             (barycenter (node forward-p)
               (let ((neighbors
                       (loop for edge in edges
                             for other = (cond ((eq node (graph-edge-to edge))
                                                (graph-edge-from edge))
                                               ((eq node (graph-edge-from edge))
                                                (graph-edge-to edge)))
                             when (and other (if forward-p
                                                 (< (gethash other ranks) (gethash node ranks))
                                                 (> (gethash other ranks) (gethash node ranks))))
                               collect (gethash other positions))))
                 (if neighbors (/ (reduce #'+ neighbors) (length neighbors)) (gethash node positions)))))
      (update-positions)
      (let ((best       (map 'vector #'copy-list layers))
            (best-score (crossings)))
        (dotimes (iteration 6)
          (declare (ignorable iteration))
          (dolist (forward-p '(t nil))
            (loop for index in (if forward-p
                                   (loop for index below (length layers) collect index)
                                   (loop for index downfrom (1- (length layers)) to 0 collect index))
                  do (setf (aref layers index)
                           (stable-sort (aref layers index) #'<
                                        :key (lambda (node) (barycenter node forward-p))))
                     (update-positions))
            (let ((score (crossings)))
              (when (< score best-score)
                (setf best-score score best (map 'vector #'copy-list layers))))))
        (loop for layer across best append layer)))))

(defun graph--assign-edge-ports (diagram placements)
  "Assign monotone ports by opposite endpoint position, avoiding fan crossings."
  (let ((groups     (make-hash-table :test #'equal))
        (ports      (make-hash-table :test #'equal))
        (horizontal (member (graph-direction diagram) '(:lr :rl))))
    (labels ((add-end (edge endpoint placement other side)
               (let ((coordinate (if (member side '(:north :south))
                                     (+ (graph-placement-x other) (/ (graph-placement-width other) 2))
                                     (+ (graph-placement-y other) (/ (graph-placement-height other) 2)))))
                 (push (list coordinate edge endpoint) (gethash (list placement side) groups)))))
      (dolist (edge (graph-edges diagram))
        (let* ((from       (find (graph-edge-from edge) placements :key #'graph-placement-node))
               (to         (find (graph-edge-to edge) placements :key #'graph-placement-node))
               (horizontal (graph--edge-horizontal from to horizontal))
               (self-p     (eq from to))
               (forward-p  (if horizontal (< (graph-placement-x from) (graph-placement-x to))
                                         (< (graph-placement-y from) (graph-placement-y to))))
               (from-side  (if self-p ':east (if horizontal (if forward-p ':east ':west)
                                                           (if forward-p ':south ':north))))
               (to-side    (if self-p ':north (if horizontal (if forward-p ':west ':east)
                                                            (if forward-p ':north ':south)))))
          (add-end edge ':from from to from-side)
          (add-end edge ':to to from to-side)))
      (maphash
       (lambda (key entries)
         (destructuring-bind (placement side) key
           (let* ((vertical-side-p (member side '(:north :south)))
                  (available       (- (if vertical-side-p (graph-placement-width placement)
                                                         (graph-placement-height placement)) 2))
                  (ordered         (stable-sort (nreverse entries) #'< :key #'first))
                  (count           (length ordered))
                  (x               (graph-placement-x placement))
                  (y               (graph-placement-y placement)))
             (when (> count available)
               (error 'resource-limit-exceeded :reason "A graph node has too many connection ports."))
             (loop for entry in ordered for index from 1
                   for offset = (round (* index (1+ available)) (1+ count))
                   for port = (ecase side
                                (:north
                                 (cons (+ x offset) y))
                                (:south
                                 (cons (+ x offset) (+ y (graph-placement-height placement) -1)))
                                (:west
                                 (cons x (+ y offset)))
                                (:east
                                 (cons (+ x (graph-placement-width placement) -1) (+ y offset))))
                   do (setf (gethash (list (second entry) (third entry)) ports) port)))))
       groups))
    ports))
