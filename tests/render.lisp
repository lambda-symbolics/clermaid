(in-package #:clermaid)

(defun render-tests ()
  "Exercise public dispatch, output safety, limits, and width recovery."
  (let ((checks 0))
    (labels ((check (value)
               (incf checks)
               (assert value))

             (signals-p (type thunk)
               (handler-case (progn (funcall thunk) nil)
                 (mermaid-error (condition) (typep condition type)))))
      (dolist (source (list (format nil "flowchart LR~%A[Start] --> B[Finish]")
                            (format nil "sequenceDiagram~%A->>B: hello")
                            (format nil "stateDiagram-v2~%Idle --> Active")
                            (format nil "classDiagram~%class Widget")
                            (format nil "erDiagram~%USER ||--o{ ORDER : places")))
        (let ((rows (render-mermaid source :width 120)))
          (check (and rows (every #'stringp rows)))
          (check (every (lambda (row) (<= (text-cell-width row) 120)) rows))
          (check (equal rows (render-mermaid source :width 120)))))
      (dolist (source (list (format nil "flowchart TD~%A[v<>+┌東京é] -->|v<>+┌| B[ok]")
                            (format nil "sequenceDiagram~%participant A as v<>+┌東京é~%A->>B: v<>+┌")
                            (format nil "stateDiagram-v2~%[*] --> Ready~%Ready --> [*]")
                            (format nil "classDiagram~%Animal <|-- Dog~%class Dog {~%+String name~%+bark()~%}")
                            (format nil "erDiagram~%USER ||--o{ ORDER : places")))
        (dolist (unicode '(t nil))
          (let* ((rows (render-mermaid source :width 200 :unicode unicode))
                 (semantic (render-mermaid-spans source :width 200 :unicode unicode)))
            (check (equal rows (mapcar (lambda (row)
                                        (apply #'concatenate 'string (mapcar #'rest row))) semantic)))
            (check (every (lambda (row)
                            (every (lambda (span)
                                     (and (member (first span) '(:plain :mermaid-box :mermaid-edge
                                                                 :mermaid-arrow :mermaid-label))
                                          (stringp (rest span)))) row)) semantic))
            (when (search "v<>+┌東京é" source)
              (check (some (lambda (row)
                             (some (lambda (span)
                                     (and (eq (first span) ':mermaid-label)
                                          (search "v<>+┌東京é" (rest span)))) row)) semantic))))))
      (dolist (source '("" "  " "%% comment only"))
        (check (signals-p 'invalid-diagram (lambda () (render-mermaid source)))))
      (check (signals-p 'unsupported-syntax (lambda () (render-mermaid "pie"))))
      (check (signals-p 'invalid-diagram
                        (lambda () (render-mermaid
                                    (format nil "flowchart LR~%A[~c[31m]" #\Escape)))))
      (check (signals-p 'invalid-diagram
                        (lambda () (render-mermaid
                                    (format nil "flowchart LR~%A[~c]" (code-char #x9b))))))
      (let ((*maximum-source-length* 8))
        (check (signals-p 'resource-limit-exceeded
                          (lambda () (render-mermaid "flowchart LR")))))
      (let ((source (format nil "flowchart LR~%A[Start] --> B[Finish]")))
        (check (signals-p 'diagram-too-wide
                          (lambda () (render-mermaid source :width 1))))
        (check (equal (render-mermaid source :width 120)
                      (handler-bind
                          ((diagram-too-wide
                             (lambda (condition)
                               (declare (ignore condition))
                               (invoke-restart 'retry-with-width 120))))
                        (render-mermaid source :width 1))))))
    checks))

(defun run-tests ()
  "Run the standalone library's behavioral tests and return their check count."
  (let ((checks (+ (graph-tests) (layout-tests) (subgraph-tests) (sequence-tests) (render-tests))))
    (format t "~&Clermaid: ~d checks passed.~%" checks)
    checks))
