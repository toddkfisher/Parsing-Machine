(defclass GRAPHVIZ-NODE ()
  ((label :initarg label)
   (id :initarg id)))

;; General node categories.
(defclass BINARY-OPERATION-NODE ()
  ((left-arg :initarg left-arg)
   (right-arg :initarg right-arg)))
(defclass UNARY-OPERATION-NODE ()
  ((arg :initarg arg)))
(defclass REPETITION-NODE (UNARY-OPERATION-NODE)
  ())
(defclass NAMELIST-NODE ()
  ((namelist :initarg namelist)
   (expr :initarg expr))

(defclass ND-ORDERED-CHOICE (BINARY-OPERATION-NODE GRAPHVIZ-NODE)
  ())
(defclass ND-RULE (GRAPHVIZ-NODE)
  ((name :initarg name)
   (expr :initarg expr)
   (semantic-action :initarg semantic-action)))
;; TODO: Not used - remove all refs in code.
(defclass ND-NAME (GRAPHVIZ-NODE)
  ((name :initarg name)))
(defclass ND-CONCAT (BINARY-OPERATION-NODE GRAPHVIZ-NODE)
  ())
(defclass ND-0-OR-1 (REPETITION-NODE GRAPHVIZ-NODE)
  ())
(defclass ND-0-OR-MANY (REPETITION-NODE GRAPHVIZ-NODE)
  ())
(defclass ND-1-OR-MANY (REPETITION-NODE GRAPHVIZ-NODE)
  ())
(defclass ND-NOT (UNARY-OPERATION-NODE GRAPHVIZ-NODE)
  ())
(defclass ND-TEXT (GRAPHVIZ-NODE)
  ((text :initarg text)))
(defclass ND-RANGE (BINARY-OPERATION-NODE GRAPHVIZ-NODE)
  ())
(defclass ND-CAPTURE (BINARY-OPERATION-NODE GRAPHVIZ-NODE)
  ())
(defclass ND-CALL-RULE (UNARY-OPERATION-NODE GRAPHVIZ-NODE)
  ())
(defclass ND-REENTER-INVALIDATION-SCOPE (NAMELIST-NODE GRAPHVIZ-NODE)
  ((capture-names :initarg capture-names)
   (expr :initarg expr)))
(defclass ND-LEAVE-INVALIDATION-SCOPE (NAMELIST-NODE GRAPHVIZ-NODE)
  ((capture-names :initarg capture-names)
   (expr :initarg expr)))

;;* Parsing

(defmacro simple-token-r (rule-name token-text)
  `(defrule ,rule-name
       (and ws*-r ,token-text ws*-r)
     (:constant ,token-text)))

(simple-token-r semicolon-tk ";")
(simple-token-r ordered-choice-op-tk "|")
(simple-token-r lparen-tk "(")
(simple-token-r rparen-tk ")")
(simple-token-r semantic-action-open-bracket-tk "%{")
(simple-token-r semantic-action-close-bracket-tk "%}")
(simple-token-r arrow-tk "=>")
(simple-token-r not-tk "~")
(simple-token-r zero-or-1-tk "?")
(simple-token-r zero-or-many-tk "*")
(simple-token-r one-or-many-tk "+")
(simple-token-r not-tk "!")
(simple-token-r range-tk "range")
(simple-token-r comma-tk ",")
(simple-token-r quote-tk "'")

(defrule grammar-r
    (and rule-r (? (and semicolon-tk grammar-r)))
  (:destructure (first-rule semicolon-other-rules*)
                (cons first-rule (cadr semicolon-other-rules*))))

(defrule expression-r
    (and term-r (* (and ordered-choice-op-tk term-r)))
  (:destructure (term oc-op-expr*)
                (if oc-op-expr*
                    ;; Left rotate series of ND-ORDERED-CHOICE - this eliminates
                    ;; sequences of redundant (LEAVE|REENTER)-INVALIDATION-SCOPE instructions
                    (reduce (lambda (l r) (make-instance 'ND-ORDERED-CHOICE 'left-arg l 'right-arg r))
                            (mapcar (lambda (x) (cadr x))
                                    oc-op-expr*)
                            :initial-value term)
                    term)))

(defrule term-r
    (and factor-r (? term-r))
  (:destructure (fct concat-expr)
                (if concat-expr
                    (make-instance 'ND-CONCAT 'left-arg fct 'right-arg concat-expr)
                    fct)))

(defrule capture-r
    (and name-tk ":")
  (:destructure (nm colon)
                nm))

(defrule suffix-op-tk
    (and ws*-r (or "?" "!" "*" "+") ws*-r)
  (:destructure (ws0 suffix-op ws1)
                suffix-op))

(defrule factor-r
    (and (? capture-r)
         (? not-tk)
         simple-factor-r
         (? suffix-op-tk))
  (:destructure (capture-name not-op simp-fac sffx-op)
                (let ((nd (cond ((string= sffx-op "?")
                                 (make-instance 'ND-0-OR-1 'arg simp-fac))
                                ((string= not-op "~")
                                 (make-instance 'ND-NOT 'arg simp-fac))
                                ((string= sffx-op "*")
                                 (make-instance 'ND-0-OR-MANY 'arg simp-fac))
                                ((string= sffx-op "+")
                                 (make-instance 'ND-1-OR-MANY 'arg simp-fac))
                                (t
                                 simp-fac))))
                  (if capture-name
                      (make-instance 'ND-CAPTURE
                                     'left-arg capture-name
                                     'right-arg nd)
                      nd))))

(defrule paren-expr-r
    (and lparen-tk expression-r rparen-tk)
  (:destructure (lp expr rp)
                expr))

(defrule call-r
    name-tk
  (:lambda (nm)
    (make-instance 'ND-CALL-RULE 'rule-name nm)))

(defrule simple-factor-r
    (or text-r
        range-r
        call-r
        paren-expr-r)
  (:identity t))

;;
;; Syntax: "range" "(" ch "," ch ")"
(defrule range-r
    (and range-tk
         lparen-tk
         quote-tk
         character
         quote-tk
         comma-tk
         quote-tk
         character
         quote-tk
         rparen-tk)
  (:destructure (kw lp q0 begin-ch q1 dash q2 end-ch q3 rp)
                (make-instance 'ND-RANGE 'left-arg begin-ch 'right-arg end-ch)))

(defrule semantic-action-char-r
    (and (and (! "%}")
              (! "%{"))
         (or "%%{"
             "%%}"
             character))
  (:destructure (junk txt)
                (cond ((string= txt "%%{")
                       "%{")
                      ((string= txt "%%}")
                       "%}")
                      (t
                       (text txt)))))

(defrule semantic-action-text-r
    (and (* semantic-action-char-r))
  (:text t))

(defrule semantic-action-r
    (and semantic-action-open-bracket-tk
         semantic-action-text-r
         semantic-action-close-bracket-tk)
  (:destructure (open-bracket sem-act-txt close-bracket)
                sem-act-txt))

(defrule ordinary-char-r
    (and (not (or #\$ #\')))
  (:destructure (ch)
                ch))

(defrule escaped-char-r
    (and #\$
         character)
  (:destructure (backslash ch)
                ch))

(defrule string-with-escapes-r
    (and (* (or escaped-char-r
                ordinary-char-r)))
  (:destructure (ch*)
                (text ch*)))

(defrule text-r
    (and quote-tk
         string-with-escapes-r
         quote-tk)
  (:destructure (l-quote str r-quote)
                (make-instance 'ND-TEXT 'text (text str))))

(defrule rule-r
    (and name-tk arrow-tk expression-r (? semantic-action-r))
  (:destructure (nm arr expr sem-action)
                (make-instance 'ND-RULE 'name nm 'expr expr 'semantic-action sem-action)))

(defrule ws*-r
    (* (or #\space #\tab #\newline)))

(defrule digit-r
    (digit-char-p character))

(defrule name-tk
    (and ws*-r
         (or (lower-case-p character)
             (upper-case-p character))
         (* (or (upper-case-p character)
                (lower-case-p character)
                digit-r
                #\?
                #\_))
         ws*-r)
  (:destructure (ws0 first-char rest-chars ws1)
                (text first-char rest-chars)))

;;* "Compiler"

(defmacro add-instruction (instruction-list-name single-instr)
  `(setf ,instruction-list-name (append ,instruction-list-name (list ,single-instr))))

`(defmacro add-multiple-instructions (instruction-list-name instruction-list-to-add)
  `(setf ,instruction-list-name (append ,() instruction-list-to-add)))

(defvar *instruction-list* nil)
(defvar *label-idx* 0)

(defun find-duplicates (L &key (test #'equal))
  (if (null L)
      nil
      (let ((1st (car L))
            (the-rest (cdr L)))
        (if (member 1st the-rest :test test)
            (let ((new-L (remove 1st the-rest :test test)))
              (cons 1st (find-duplicates new-L :test test)))
            (find-duplicates the-rest :test test)))))

(defun collect-capture-names (root)
  (if (null root)
      nil
  (typecase root
    (ND-ORDERED-CHOICE
     (append (collect-capture-names (slot-value root 'left-arg))
             (collect-capture-names (slot-value root 'right))))
    (ND-RULE
     (collect-capture-names (slot-value root 'expr)))
    (ND-NAME nil)
    (ND-CONCAT
     (append (collect-capture-names (slot-value root 'left-arg))
             (collect-capture-names (slot-value root 'right))))
    (ND-CAPTURE
     (append (list (slot-value root 'left-arg))
             (collect-capture-names (slot-value root 'right-arg))))
    (ND-0-OR-1
     (collect-capture-names (slot-value root 'expr)))
    (ND-0-OR-MANY
     (collect-capture-names (slot-value root 'expr)))
    (ND-1-OR-MANY
     (collect-capture-names (slot-value root 'expr)))
    (ND-NOT
     (collect-capture-names (slot-value root 'expr)))
    (ND-TEXT nil)
    (ND-CALL-RULE nil)
    (ND-REENTER-INVALIDATION-SCOPE
     (collect-capture-names (slot-value root 'expr)))
    (ND-LEAVE-INVALIDATION-SCOPE
     (collect-capture-names (slot-vlaue root 'expr))))))

(defun init-compile ()
  (setf *instruction-list* nil)
  (setf *label-idx* 0))

(defun new-label ()
  (prog1 (format nil "L~a" *label-idx*)
    (setf *label-idx* (1+ *label-idx*))))

(defmacro vlet* (vars-value* &rest body)
  (labels ((expand-mv-bindings (vars-value*)
             (if vars-value*
                 (let ((var-list (caar vars-value*))
                       (value (cadar vars-value*)))
                   (if (null (cdr vars-value*))
                       `(multiple-value-bind ,var-list ,value ,@body)
                       `(multiple-value-bind ,var-list ,value ,(expand-mv-bindings
                                                                (cdr vars-value*))))))))
    (expand-mv-bindings vars-value*)))

(defun add-invalidation-scopes (root active-capture-names)
  (if root
      (typecase root
        (ND-ORDERED-CHOICE
         ;;
         ;; (0) Every capture currently  in invalidation scope has that scope  suspended on the left
         ;;     branch  of an  ordered choice.   Active captures  do not  get passed  down the  left
         ;;     branch.  Active  captures do  get passed to  the right branch  since failure  of the
         ;;     right branch means that there are no more alternatives and hence, the entire ordered
         ;;     choice expression has failed.
         ;;
         ;; (1) The captures introduced on the left branch  do not have an invalidation scope on the
         ;;     right branch of ordered choice.
         ;;
         (vlet* (((left-root left-branch-capture-set)
                  (add-invalidation-scopes (slot-value root 'left-arg) nil))
                 ((right-root new-capture-set)
                  (add-invalidation-scopes (slot-value root 'right-arg) active-capture-names)))
                (let ((new-left-root (make-instance 'ND-LEAVE-INVALIDATION-SCOPE
                                      'namelist active-capture-names
                                      'expr left-root))
                      (new-right-root (make-instance 'ND-REENTER-INVALIDATION-SCOPE
                                       'namelist active-capture-names
                                       'expr right-root)))
                  (make-instance 'ND-ORDERED-CHOICE
                                 'left-arg new-left-root
                                 'right-arg new-right-root))))
        (ND-RULE
         (vlet* (((new-expr capture-set) (add-invalidation-scopes (slot-value root 'expr)
                                                                  active-capture-names)))
                (setf (slot-value root 'expr) new-expr)
                (values root capture-set)))
        (ND-NAME
         (values root
                 active-capture-names))
        (ND-CONCAT
         ;;
         ;; (0) Captures currently within invalidation scope are passed down the left branch.
         ;;
         ;; (1) New captures may  enter the current scope from the left branch  so we will receive a
         ;;     (possibly) expanded set of capture names.
         ;;
         ;; (2) We also receive a new (possibly modified) ldft branch of this operator.
         ;;
         ;; (3) The new  capture set from (1)  is passed to the  right branch and we  also receive a
         ;;     (possibly) even new capture set.
         ;;
         ;; (4) We join the  new left and right branches with a 'concat'  opertor and pass the newly
         ;;     created concat node and newer capture set back up to the caller.
         ;;
         (vlet* (((new-left new-capture-set)
                  (add-invalidation-scopes (slot-value root 'left-arg) active-capture-names))
                 ((new-right newer-capture-set)
                  (add-invalidation-scopes (slot-value root 'right-arg) new-capture-set)))
                (values (make-instance 'ND-CONCAT 'left-arg new-left 'right-arg new-right)
                        newer-capture-set)))
        (ND-CAPTURE
         (let ((new-capture-set (adjoin (slot-value root 'left-arg)
                                        active-capture-names)))
           ;;
           ;; (0) After adding the new capture name to  the current capture set, we pass the new set
           ;;     down to the expression being captured.
           ;;
           ;; (1) We receive a (possibly) enlarged capture set and a (possibly) modified expression.
           ;;
           ;; (2) Join the capture name and new capture expression with a capture node.
           ;;
           (vlet* (((new-capture-expr newer-capture-set)
                    (add-invalidation-scopes (slot-value root 'left-arg) new-capture-set)))
                  (values (make-instance 'ND-CAPTURE
                                         'left-arg (ND-CAPTURE-capture-name root)
                                         'right-arg new-capture-expr)
                          newer-capture-set))))
        (ND-0-OR-1
         ;;
         ;; '<expr>?' never invalidates any captures.  Only new captures introduced in <expr> can be
         ;; invalidated.  Therefore, we pass nothing to  the expression subordinate to '?'  However,
         ;; <expr> may introduce  new capture names into  the invalidation scope so we  need to pick
         ;; those up so that they get passed to the right branch.
         ;;
         (vlet* (((new-expr new-capture-set)
                  (add-invalidation-scopes (slot-value root 'expr) nil)))
                (values (make-instance 'ND-0-OR-1 'expr new-expr)
                        new-capture-set)))
        (ND-0-OR-MANY  ;; a* = (| a | aa | aaa | aaaa | ... )
         ;;
         ;; The situation with ND-0-OR-MANY is exactly equivalent to ND-0-OR-1
         ;;
         (vlet* (((new-expr new-capture-set)
                  (add-invalidation-scopes (slot-value root 'expr) nil)))
                (values (make-instance 'ND-0-OR-1 'expr new-expr)
                        new-capture-set)))
        (ND-1-OR-MANY
         ;;
         ;; In 'a+',  'a' must  always succeed  at least once  and therefore  it can  invalidate any
         ;; currently active captures.  And, any captures introduced can also be invalidated further
         ;; down the line.
         ;;
         (vlet* (((new-expr new-capture-set)
                  (add-invalidation-scopes (slot-value root 'expr) active-capture-names)))
                (values (make-instance 'ND-1-OR-MANY 'expr (add-invalidation-scopes new-expr new-capture-set))
                        new-capture-set)))
        (ND-NOT
         (values root active-capture-names))
        (ND-TEXT
         (values root active-capture-names))
        (ND-CALL-RULE
         (values root active-capture-names))
        (ND-REENTER-INVALIDATION-SCOPE  ;; Shouldn't happen
         (values root active-capture-names))
        (ND-RANGE
         (values root active-capture-names))
        (ND-LEAVE-INVALIDATION-SCOPE ;; Shouldn't happen
         (values root active-capture-names)))))

(defun peg-compile-grammar (rule-list)
  (init-compile)
  (loop for rule in rule-list
     do (let ((rule-with-invalidation-scopes (add-invalidation-scopes rule nil)))
          (peg-compile rule-with-invalidation-scopes nil)))
  *instruction-list*)
;;
;; This function compiles rules only. Grammars are simply lists of rules.
;; Top-level compile function is 'peg-compile-grammar'
;;
(defun peg-compile (root capture-names)
  (if root
      (typecase root
        (ND-CAPTURE
         (let ((capture-slot-idx (position (slot-value root 'left-arg)
                                           capture-names :test #'string-equal)))
           (add-instruction *instruction-list* (format nil "I_BEGIN_CAPTURE ~a" capture-slot-idx))
           (peg-compile (slot-value root 'right-arg) capture-names)
           (add-instruction *instruction-list* (format nil "I_END_CAPTURE ~a" capture-slot-idx))))
        (ND-REENTER-INVALIDATION-SCOPE
         (loop for capture-name in (slot-value root 'namelist)
            do (let ((capture-slot-idx (position capture-name capture-names :test #'string-equal)))
                 (add-instruction *instruction-list* (format nil "I_REENTER_INVALIDATION_SCOPE ~a"
                                                             capture-slot-idx))))
         (peg-compile (slot-value root 'expr) capture-names))
        (ND-LEAVE-INVALIDATION-SCOPE
         (loop for capture-name in (slot-value root 'namelist)
            do (let ((capture-slot-idx (position capture-name capture-names :test #'string-equal)))
                 (add-instruction *instruction-list* (format nil "I_LEAVE_INVALIDATION_SCOPE ~a"
                                                             capture-slot-idx))))
         (peg-compile (slot-value root 'expr) capture-names))
        (ND-CONCAT
         (progn (peg-compile (slot-value root 'left-arg) capture-names)
                (peg-compile (slot-value root 'right-arg) capture-names)))
        (ND-CALL-RULE
         (add-instruction *instruction-list* (format nil "I_CALL ~a" (slot-value root 'rule-name))))
        (ND-TEXT
         (do ((i 0 (+ i 1))
              (txt (slot-value root 'text))
              (len (length (slot-value root 'text))))
             ((>= i len))
           (add-instruction *instruction-list* (format nil "I_CHAR '~a'" (elt txt i)))))
        (ND-RANGE
         (add-instruction *instruction-list* (format nil "I_RANGE '~a' '~a'"
                                                     (slot-value root 'left-arg)
                                                     (slot-value root 'right-arg))))
        (ND-RULE
         (progn
           (add-instruction *instruction-list* (format nil "~a:  // Rule" (ND-RULE-name root)))
           (let* ((new-capture-names (collect-capture-names root))
                  (duplicate-names (find-duplicates capture-names :test #'string-equal)))
             (if duplicate-names
                 (error (format nil "Rule ~a has duplicate captures: ~a" (ND-RULE-name root)
                                duplicate-names))
                 (progn (add-instruction *instruction-list* (format nil "I_CREATE_CAPTURE_SLOTS ~a"
                                                                    (length new-capture-names)))
                        (peg-compile (slot-value root 'expr) new-capture-names))))))
        (ND-ORDERED-CHOICE
         (let ((L0 (new-label))
               (L1 (new-label)))
           (add-instruction *instruction-list* (format nil "I_CHOICE ~a" L0))
           (peg-compile (slot-value root 'left-arg) capture-names)
           (add-instruction *instruction-list* (format nil "I_COMMIT ~a" L1))
           (add-instruction *instruction-list* (format nil "~a:" L0))
           (peg-compile (slot-value root 'right-arg) capture-names)
           (add-instruction *instruction-list* (format nil "~a:" L1))))
        (ND-0-OR-MANY
         (let ((L0 (new-label))
               (L1 (new-label)))
           (add-instruction *instruction-list* (format nil "I_CHOICE ~a" L0))
           (add-instruction *instruction-list* (format nil "~a:" L1))
           (peg-compile (slot-value root 'arg) capture-names)
           (add-instruction *instruction-list* (format nil "I_PARTIAL_COMMIT ~a" L1))
           (add-instruction *instruction-list* (format nil "~a:" L0))))
        (ND-0-OR-1
         (let ((L (new-label)))
           (add-instruction *instruction-list* (format nil "I_CHOICE ~a" L))
           (peg-compile (slot-value root 'arg) capture-names)
           (add-instruction *instruction-list* (format nil "I_COMMIT ~a" L))
           (add-instruction *instruction-list* (format nil "~a:" L))))
        (ND-1-OR-MANY
         (let ((L0 (new-label))
               (L1 (new-label)))
           (add-instruction *instruction-list* (format nil "~a:" L0))
           (peg-compile (slot-value root 'arg) capture-names)
           (add-instruction *instruction-list* (format nil "I_CHOICE ~a" L1))
           (add-instruction *instruction-list* (format nil "I_PARTIAL_COMIT ~a" L0))
           (add-instruction *instruction-list* (format nil "~a:" L1)))))))

;; Graphviz output.

(defmethod initialize-instance :after ((nd GRAPHVIZ-NODE) &rest args)
  (setf (slot-value nd 'id) (symbol-name (gensym "N"))))

(defmacro initialize-with-fixed-label (nd-type label)
  `(defmethod initialize-instance :after ((nd ,nd-type) &rest args)
              (setf (slot-value nd 'label) ,label)))

(initialize-with-fixed-label ND-ORDERED-CHOICE "ordered choice")
(initialize-with-fixed-label ND-RULE "rule")
(initialize-with-fixed-label ND-CONCAT "concat")
(initialize-with-fixed-label ND-0-OR-1 "0-or-1")
(initialize-with-fixed-label ND-0-OR-MANY "0-or-many")
(initialize-with-fixed-label ND-NOT "not")
(initialize-with-fixed-label ND-RANGE "range")
(initialize-with-fixed-label ND-CAPTURE "capture")
(initialize-with-fixed-label ND-CALL-RULE "call rule")
(initialize-with-fixed-label ND-REENTER-INVALIDATION-SCOPE "RIS")
(initialize-with-fixed-label ND-LEAVE-INVALIDATION-SCOPE "LIS")

(defgeneric gv-node-string (nd))

(defmethod gv-node-string ((nd BINARY-OPERATION-NODE))
  (let* ((left-arg (slot-value nd 'left-arg))
         (right-arg (slot-value nd 'right-arg))
         (left-arg-id (slot-value left-arg 'id))
         (right-arg-id (slot-value right-arg 'id))
         (nd-id (slot-value nd 'id))
         (nd-label (slot-value nd 'label)))
    (format nil "~a [shape=record, label=\"{~a|{<left>L|<right>R\"}];~%~a -> ~a:left;~%~a -> ~a:right;~%"
            nd-id
            nd-label
            nd-id
            left-arg-id
            nd-id
            right-arg-id)))
