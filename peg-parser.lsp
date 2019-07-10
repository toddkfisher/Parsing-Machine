;;* Misc util

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

(defun make-string-copies (s n)
  (do ((result ""))
      ((<= n 0) result)
    (setf result (concatenate 'string result s))
    (setf n (1- n))))

(defun string-join (string-list &key (separator ""))
  (let ((result ""))
    (do ((i 0 (1+ i)))
        ((>= i (length string-list))
         result)
      (setf result (concatenate 'string
                                result
                                (if (not (string= result ""))
                                    separator
                                    "")
                                (elt string-list i))))))

;;* Generic classes

(defclass GRAPHVIZ-NODE ()
  ((label :initarg label)
   (id :initarg id)))

(defclass BINARY-OPERATION-NODE ()
  ((left-arg :initarg left-arg)
   (right-arg :initarg right-arg)))

(defclass UNARY-OPERATION-NODE ()
  ((arg :initarg arg)))

(defclass REPETITION-NODE (UNARY-OPERATION-NODE)
  ())

;;* Specific classes

(defclass ND-ORDERED-CHOICE (BINARY-OPERATION-NODE GRAPHVIZ-NODE)
  ((names-in-invalidation-scope :initarg names-in-invalidation-scope)))

(defclass ND-RULE (GRAPHVIZ-NODE)
  ((name :initarg name)
   (expr :initarg expr)
   (semantic-action :initarg semantic-action)))

(defclass ND-CONCAT (BINARY-OPERATION-NODE GRAPHVIZ-NODE)
  ())

(defclass ND-0-OR-1 (REPETITION-NODE GRAPHVIZ-NODE)
  ())

(defclass ND-1-OR-MANY (REPETITION-NODE GRAPHVIZ-NODE)
  ())

(defclass ND-0-OR-MANY (REPETITION-NODE GRAPHVIZ-NODE)
  ())

(defclass ND-NOT (UNARY-OPERATION-NODE GRAPHVIZ-NODE)
  ())

(defclass ND-TEXT (GRAPHVIZ-NODE)
  ((text :initarg text)))

(defclass ND-RANGE (BINARY-OPERATION-NODE GRAPHVIZ-NODE)
  ())

(defclass ND-CAPTURE (GRAPHVIZ-NODE)
  ((capture-name :initarg capture-name)
   (expr :initarg expr)))

(defclass ND-CALL-RULE (GRAPHVIZ-NODE)
  ((rule-name :initarg rule-name)))

;;* Token-parsing macros

;; Macro to define rules for simple tokens.  That is, fixed pieces of text (possibly) surrounded
;; by whitespace.
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
(simple-token-r zero-or-1-tk "?")
(simple-token-r zero-or-many-tk "*")
(simple-token-r one-or-many-tk "+")
(simple-token-r not-tk "!")
(simple-token-r range-tk "range")
(simple-token-r comma-tk ",")
(simple-token-r quote-tk "'")
(simple-token-r any-tk ".")

;;* Parsing

;; Grammar => Rule (";" Grammar)?
(defrule grammar-r
    (and rule-r (? (and semicolon-tk grammar-r)))
  (:destructure (first-rule semicolon-other-rules*)
                (cons first-rule (cadr semicolon-other-rules*))))

;; Expression => Term ("|" Term)*
(defrule expression-r
    (and term-r (? (and ordered-choice-op-tk expression-r)))
  (:destructure (term oc-tk&expr)
                (if oc-tk&expr
                    (make-instance 'ND-ORDERED-CHOICE
                                   'left-arg term
                                   'right-arg (cadr oc-tk&expr))
                    term)))

;; Term => Factor Term?
(defrule term-r
    (and factor-r (? term-r))
  (:destructure (fct concat-expr)
                (if concat-expr
                    (make-instance 'ND-CONCAT
                                   'left-arg fct
                                   'right-arg concat-expr)
                    fct)))
;; Capture => Name ":"
(defrule capture-r
    (and name-tk ":")
  (:destructure (nm colon)
                nm))

;; SuffixOpTk => Whitespace ("?" | "*" | "+")
(defrule suffix-op-tk
    (and ws*-r (or "?" "*" "+") ws*-r)
  (:destructure (ws0 suffix-op ws1)
                suffix-op))

;; Factor => Capture? "!"? SimpleFactor SuffixOpTk?
(defrule factor-r
    (and (? capture-r)
         (? not-tk)
         simple-factor-r
         (? suffix-op-tk))
  (:destructure (capture-name not-op simp-fac sffx-op)
                (let ((nd nil)
                      (not-nd nil))
                  (cond ((string= sffx-op "?")
                         (setf nd (make-instance 'ND-0-OR-1 'arg simp-fac)))
                        ((string= sffx-op "*")
                         (setf nd (make-instance 'ND-0-OR-MANY 'arg simp-fac)))
                        ((string= sffx-op "+")
                         (setf nd (make-instance 'ND-1-OR-MANY 'arg simp-fac))))
                  (if (null nd)
                      (setf nd simp-fac))
                  (if (string= not-op "!")
                      (setf nd (make-instance 'ND-NOT 'arg nd)))
                  (if capture-name
                      (make-instance 'ND-CAPTURE
                                     'capture-name capture-name
                                     'expr nd)
                      nd))))

;; ParenExpr => "(" Expression ")"
(defrule paren-expr-r
    (and lparen-tk expression-r rparen-tk)
  (:destructure (lp expr rp)
                expr))

;; Call => Name
(defrule call-r
    name-tk
  (:lambda (nm)
    (make-instance 'ND-CALL-RULE 'rule-name nm)))

;; SimpleFactor => Text | Range | Call | ParenExpr
(defrule simple-factor-r
    (or text-r
        range-r
        call-r
        paren-expr-r
        any-r)
  (:identity t))

(defrule any-r
    any-tk
  (:lambda (any)
    (make-instance 'ND-ANY)))

;; Range => "range" "(" ch "," ch ")"
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

;; :(
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

;; Name => (UpperCase | LowerCase) (UpperCase | LowerCase | Digit | '?' | '_')*
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

;;* Grammar post-processing.

(defun post-proc-grammar (rule-list)
  (mapcar (lambda (rule)
            (add-invalidation-scopes rule nil))
          (combine-rules-with-same-name rule-list)))

(defgeneric add-invalidation-scopes (root active-capture-names))

(defmethod add-invalidation-scopes ((root ND-ORDERED-CHOICE) active-capture-names)
  (setf (slot-value root 'names-in-invalidation-scope)
        active-capture-names)
  (vlet* (((new-left-arg left-capture-names)
           (add-invalidation-scopes (slot-value root 'left-arg) nil))
          ((new-right-arg right-capture-names)
           (add-invalidation-scopes (slot-value root 'right-arg)  active-capture-names)))
         (setf (slot-value root 'left-arg) new-left-arg)
         (setf (slot-value root 'right-arg) new-right-arg)
         (values root (union left-capture-names right-capture-names))))

(defmethod add-invalidation-scopes ((root ND-RULE) active-capture-names)
  (vlet* (((new-expr new-capture-names)
           (add-invalidation-scopes (slot-value root 'expr) nil)))
         (setf (slot-value root 'expr) new-expr)
         (values root new-capture-names)))

(defmethod add-invalidation-scopes ((root ND-CONCAT) active-capture-names)
  (vlet* (((new-left-arg left-capture-names)
           (add-invalidation-scopes (slot-value root 'left-arg) active-capture-names))
          ((new-right-arg right-capture-names)
           (add-invalidation-scopes (slot-value root 'right-arg) left-capture-names)))
         (setf (slot-value root 'left-arg) new-left-arg)
         (setf (slot-value root 'right-arg) new-right-arg)
         (values root right-capture-names)))

(defmethod add-invalidation-scopes ((root ND-CAPTURE) active-capture-names)
  (vlet* (((new-expr new-active-capture-names)
           (add-invalidation-scopes (slot-value root 'expr)
                                    (adjoin (slot-value root 'capture-name)
                                            active-capture-names))))
         (setf (slot-value root 'expr) new-expr)
         (values root new-active-capture-names)))

(defmethod add-invalidation-scopes ((root ND-0-OR-1) active-capture-names)
  (vlet* (((new-arg new-active-capture-names)
           (add-invalidation-scopes (slot-value root 'arg) nil)))
         (setf (slot-value root 'arg) new-arg)
         (values root new-active-capture-names)))

(defmethod add-invalidation-scopes ((root ND-0-OR-MANY) active-capture-names)
  (vlet* (((new-arg new-active-capture-names)
           (add-invalidation-scopes (slot-value root 'arg) nil)))
         (setf (slot-value root 'arg) new-arg)
         (values root new-active-capture-names)))

(defmethod add-invalidation-scopes ((root ND-1-OR-MANY) active-capture-names)
  (vlet* (((new-arg new-active-capture-names)
           (add-invalidation-scopes (slot-value root 'arg) active-capture-names)))
         (setf (slot-value root 'arg) new-arg)
         (values root new-active-capture-names)))

(defmethod add-invalidation-scopes ((root ND-NOT) active-capture-names)
  (vlet* (((new-arg new-active-capture-names)
           (add-invalidation-scopes (slot-value root 'arg) active-capture-names)))
         (setf (slot-value root 'arg) new-arg)
         (values root new-active-capture-names)))

(defmethod add-invalidation-scopes ((root ND-CALL-RULE) active-capture-names)
  (values root active-capture-names))

(defmethod add-invalidation-scopes ((root ND-RANGE) active-capture-names)
  (values root active-capture-names))

(defmethod add-invalidation-scopes ((root ND-TEXT) active-capture-names)
  (values root active-capture-names))

(defun has-semantic-action-p (nd)
  (and (typep nd 'ND-RULE)
       (slot-value nd 'semantic-action)))

(defun make-rewrite-names (rule-name n-rewrites)
  (mapcar (lambda (n) (format nil "~a%~a" rule-name n))
          (alexandria:iota n-rewrites)))

(defun combine-rule-exprs (rule-name rule-node-list)
  (let ((n-rewrites (length rule-node-list)))
    (if (> n-rewrites 1)
        (let* ((rewrite-names (make-rewrite-names rule-name n-rewrites))
               (new-rule-list (mapcar (lambda (rewrite-name rule-node)
                                        (setf (slot-value rule-node 'name) rewrite-name)
                                        ;; Return the original node since setting the slot
                                        ;; yields the value that we set the slot to and not
                                        ;; the object itself.
                                        rule-node)
                                      rewrite-names
                                      rule-node-list))
               (rewrite-call-list (mapcar (lambda (rewrite-name)
                                            (make-instance 'ND-CALL-RULE
                                                           'rule-name rewrite-name))
                                          rewrite-names))
               (top-level-rule
                (make-instance 'nd-rule
                               'name rule-name
                               'expr (reduce (lambda (x y) (make-instance 'ND-ORDERED-CHOICE
                                                                          'left-arg x
                                                                          'right-arg y))
                                             rewrite-call-list
                                             :from-end t))))
          `(,top-level-rule ,@new-rule-list))
        rule-node-list)))

(defun combine-rules-with-same-name (rule-list)
  (let ((rule-name-to-rule-exprs-table (make-hash-table :test #'equal)))
    (dolist (rule rule-list)
      (let* ((rule-name (slot-value rule 'name))
             (rule-exprs-list (gethash rule-name rule-name-to-rule-exprs-table 'NOT-FOUND)))
        (if (eq rule-exprs-list 'NOT-FOUND)
            (setf (gethash rule-name rule-name-to-rule-exprs-table) `(,rule))
            (setf (gethash rule-name rule-name-to-rule-exprs-table)
                  (append rule-exprs-list `(,rule))))))
    (let ((new-rule-list nil))
      (maphash (lambda (rule-name expr-list)
                 (setf new-rule-list (append new-rule-list (combine-rule-exprs rule-name expr-list))))
               rule-name-to-rule-exprs-table)
      new-rule-list)))

;;* Compiling

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
             (collect-capture-names (slot-value root 'right-arg))))
    (ND-RULE
     (collect-capture-names (slot-value root 'expr)))
    (ND-CONCAT
     (append (collect-capture-names (slot-value root 'left-arg))
             (collect-capture-names (slot-value root 'right-arg))))
    (ND-CAPTURE
     (append (list (slot-value root 'capture-name))
             (collect-capture-names (slot-value root 'expr))))
    (ND-0-OR-1
     (collect-capture-names (slot-value root 'arg)))
    (ND-0-OR-MANY
     (collect-capture-names (slot-value root 'arg)))
    (ND-1-OR-MANY
     (collect-capture-names (slot-value root 'arg)))
    (ND-NOT
     (collect-capture-names (slot-value root 'arg)))
    (ND-TEXT nil)
    (ND-CALL-RULE nil))))

(defun init-compile ()
  (setf *instruction-list* nil)
  (setf *label-idx* 0))

(defun new-label ()
  (prog1 (format nil "L~a" *label-idx*)
    (setf *label-idx* (1+ *label-idx*))))

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
(defgeneric peg-compile (root capture-names-in-rule))

(defmethod peg-compile ((root ND-CAPTURE) capture-names-in-rule)
  (let* ((capture-name (slot-value root 'capture-name))
         (capture-slot-idx (position capture-name
                                     capture-names-in-rule
                                     :test #'string-equal)))
    (add-instruction *instruction-list* (format nil "I_BEGIN_CAPTURE ~a" capture-slot-idx))
    (peg-compile (slot-value root 'expr) capture-names-in-rule)
    (add-instruction *instruction-list* (format nil "I_END_CAPTURE ~a" capture-slot-idx))))

(defmethod peg-compile ((root ND-CONCAT) capture-names-in-rule)
  (progn (peg-compile (slot-value root 'left-arg) capture-names-in-rule)
         (peg-compile (slot-value root 'right-arg) capture-names-in-rule)))

(defmethod peg-compile ((root ND-CALL-RULE) capture-names-in-rule)
  (add-instruction *instruction-list* (format nil "I_CALL ~a" (slot-value root 'rule-name))))

(defmethod peg-compile ((root ND-TEXT) capture-names-in-rule)
  (do ((i 0 (1+ i))
       (txt (slot-value root 'text))
       (len (length (slot-value root 'text))))
      ((>= i len))
    (add-instruction *instruction-list* (format nil "I_CHAR '~a'" (elt txt i)))))

(defmethod peg-compile ((root ND-RANGE) capture-names-in-rule)
  (add-instruction *instruction-list* (fomat nil "I_RANGE '~a' '~a'"
                                             (slot-value root 'left-arg)
                                             (slot-value root 'right-arg))))

(defmethod peg-compile ((root ND-RULE) capture-names-in-rule)
  (progn (add-instruction *instruction-list* (format nil "~a: // Rule" (slot-value root 'rule-name)))
         (let* ((capture-names-in-rule (collect-capture-names root))
                (duplicate-capture-names (find-duplicates capture-names-in-rule :test #'string-equal)))
           (if duplicate-capture-names
               (error (format nil "Rule ~a has duplicate captures: ~a" (slot-value root 'rule-name)
                              duplicate-capture-names))
               (progn (add-instruction *instruction-list* (format nil "I_CREATE_CAPTURE_SLOTS ~a"
                                                                  (length capture-names-in-rule)))
                      (peg-compile (slot-value root 'expr) capture-names-in-rule))))))

(defun leave-invalidation-scope (capture-names)
  (dolist (var capture-names nil)
    (add-instruction *instruction-list* (format nil "I_LEAVE_INVALIDATION_SCOPE ~a" var))))

(defun reenter-invalidation-scope (capture-names)
  (dolist (var capture-names nil)
    (add-instruction *instruction-list* (format nil "I_REENTER_INVALIDATION_SCOPE ~a" var))))

(defmethod peg-compile ((root ND-ORDERED-CHOICE) capture-names-in-rule)
  (let ((L0 (new-label))
        (L1 (new-label)))
    (add-instruction *instruction-list* (format nil "I_CHOICE ~a" L0))
    (leave-invalidation-scope (slot-value root 'names-in-invalidation-scope))
    (peg-compile (slot-value root 'left-arg) capture-names-in-rule)
    (add-instruction *instruction-list* (format nil "I_COMMIT ~a" L1))
    (add-instruction *instruction-list* (format nil "~a:" L0))
    (reenter-invalidation-scope (slot-value root 'names-in-invalidation-scope))
    (peg-compile (slot-value root 'right-arg) capture-names-in-rule)
    (add-instruction *instruction-list* (format nil "~a:" L1))))

(defmethod peg-compile ((root ND-0-OR-MANY) capture-names-in-rule)
  (let ((L0 (new-label))
        (L1 (new-label)))
    (add-instruction *instruction-list* (format nil "I_CHOICE ~a" L0))
    (add-instruction *instruction-list* (format nil "~a:" L1))
    (peg-compile (slot-value root 'arg) capture-names-in-rule)
    (add-instruction *instruction-list* (format nil "I_PARTIAL_COMMIT ~a" L1))
    (add-instruction *instruction-list* (format nil "~a:" L0))))

(defmethod peg-compile ((root ND-0-OR-1) capture-names-in-rule)
  (let ((L (new-label)))
    (add-instruction *instruction-list* (format nil "I_CHOICE ~a" L))
    (peg-compile (slot-value root 'arg) capture-names-in-rule)
    (add-instruction *instruction-list* (format nil "I_COMMIT ~a" L))
    (add-instruction *instruction-list* (format nil "~a:" L))))

(defmethod peg-compile ((root ND-1-OR-MANY) capture-names-in-rule)
  (let ((L0 (new-label))
        (L1 (new-label)))
    (add-instruction *instruction-list* (format nil "~a:" L0))
    (peg-compile (slot-value root 'arg) capture-names-in-rule)
    (add-instruction *instruction-list* (format nil "I_CHOICE ~a" L1))
    (add-instruction *instruction-list* (format nil "I_PARTIAL_COMIT ~a" L0))
    (add-instruction *instruction-list* (format nil "~a:" L1))))

(defmethod peg-ocmpile ((root ND-NOT) capture-names-in-rule)
  (let ((L0 (new-label)))
    (add-instruction *instruction-list* (format nil "I_CHOICE ~a" L0))
    (peg-compile (slot-value root 'arg) capture-names-in-rule)
    (add-instruction *instruction-list* "I_FAIL")
    (add-instruction *instruction-list* (format nil "~a:" L0))))

;;* Graphviz output

(defmethod initialize-instance :after ((nd GRAPHVIZ-NODE) &rest args)
  (setf (slot-value nd 'id) (symbol-name (gensym "N"))))

(defmacro initialize-with-fixed-label (nd-type label)
  `(defmethod initialize-instance :after ((nd ,nd-type) &rest args)
              (setf (slot-value nd 'label) ,label)))

(initialize-with-fixed-label ND-ORDERED-CHOICE "ordered choice")
(initialize-with-fixed-label ND-RULE "rule")
(initialize-with-fixed-label ND-CONCAT "concat")
(initialize-with-fixed-label ND-1-OR-MANY "1-or-many")
(initialize-with-fixed-label ND-0-OR-1 "0-or-1")
(initialize-with-fixed-label ND-0-OR-MANY "0-or-many")
(initialize-with-fixed-label ND-NOT "not")
(initialize-with-fixed-label ND-RANGE "range")
(initialize-with-fixed-label ND-CAPTURE "capture")
(initialize-with-fixed-label ND-CALL-RULE "call rule")

(defgeneric gv-node-string (nd))

(defmethod gv-node-string ((nd BINARY-OPERATION-NODE))
  (let* ((left-arg (slot-value nd 'left-arg))
         (right-arg (slot-value nd 'right-arg))
         (left-arg-id (slot-value left-arg 'id))
         (right-arg-id (slot-value right-arg 'id))
         (nd-id (slot-value nd 'id))
         (nd-label (slot-value nd 'label)))
    (format nil "~a [shape=record, label=\"{~a|{<left>left|<right>right}}\"];~%~a:left -> ~a;~%~a:right -> ~a;~%"
            nd-id
            nd-label
            nd-id
            left-arg-id
            nd-id
            right-arg-id)))

(defmethod gv-node-string ((nd UNARY-OPERATION-NODE))
  (let* ((arg (slot-value nd 'arg))
         (arg-id (slot-value arg 'id))
         (nd-id (slot-value nd 'id))
         (nd-label (slot-value nd 'label)))
    (format nil "~a [shape=record, label=\"{~a|{<arg>arg}}\"];~%~a:arg -> ~a;~%"
            nd-id
            nd-label
            nd-id
            arg-id)))

(defmethod gv-node-string ((nd ND-ORDERED-CHOICE))
  (let* ((nd-id (slot-value nd 'id))
         (nd-label (slot-value nd 'label))
         (nd-left-arg (slot-value nd 'left-arg))
         (left-arg-id (slot-value nd-left-arg 'id))
         (nd-right-arg (slot-value nd 'right-arg))
         (right-arg-id (slot-value nd-right-arg 'id))
         (namelist (slot-value nd 'names-in-invalidation-scope))
         (tail-dot-string (concatenate 'string (make-string-copies "}" (+ 2 (length namelist)))
                                       (format nil "\"];~%")))
         (head-dot-string (format nil "~a [shape=record, label=\"{~a|{<left>left|<right>right|" nd-id nd-label))
         (namelist-dot-string (string-join namelist :separator "|{")))
    (concatenate 'string head-dot-string namelist-dot-string tail-dot-string
                 (format nil "~a:left -> ~a;~%" nd-id left-arg-id)
                 (format nil "~a:right -> ~a;~%" nd-id right-arg-id))))

(defmethod gv-node-string ((nd ND-TEXT))
  (format nil "~a [shape=record label=\"'~a'\"];~%" (slot-value nd 'id) (slot-value nd 'text)))

(defmethod gv-node-string ((nd ND-RULE))
  (let ((nd-id (slot-value nd 'id))
        (nd-label (slot-value nd 'label)))
    (format nil "~a [shape=record label=\"{~a|{~a|<expr>expr}}\"];~%~a:expr -> ~a~%"
            nd-id
            nd-label
            (slot-value nd 'name)
            nd-id
            (slot-value (slot-value nd 'expr) 'id))))

(defmethod gv-node-string ((nd ND-CAPTURE))
  (let* ((nd-id (slot-value nd 'id))
         (nd-label (slot-value nd 'label))
         (node-str "")
         (edge-str ""))
    (setf node-str (format nil "~a [shape=record, label=\"{~a|{<expr>expr|~a}}\"];~%" nd-id nd-label
                           (slot-value nd 'capture-name)))
    (setf edge-str (format nil "~a:expr -> ~a;~%" nd-id (slot-value (slot-value nd 'expr) 'id)))
    (concatenate 'string node-str edge-str)))

(defmethod gv-node-string ((nd ND-CALL-RULE))
  (format nil "~a [shape=record, label=\"{~a|~a}\"];~%"
          (slot-value nd 'id)
          (slot-value nd 'label)
          (slot-value nd 'rule-name)))

(defgeneric node-children (nd))

(defmethod node-children ((nd ND-RULE))
  `(,(slot-value nd 'expr)))

(defmethod node-children ((nd BINARY-OPERATION-NODE))
  `(,(slot-value nd 'left-arg) ,(slot-value nd 'right-arg)))

(defmethod node-children ((nd ND-ORDERED-CHOICE))
  `(,(slot-value nd 'left-arg)
     ,(slot-value nd 'right-arg)))

(defmethod node-children ((nd UNARY-OPERATION-NODE))
  `(,(slot-value nd 'arg)))

(defmethod node-children ((nd ND-TEXT))
  nil)

(defmethod node-children ((nd ND-CAPTURE))
  `(,(slot-value nd 'expr)))

(defmethod node-children ((nd ND-CALL-RULE))
  nil)

(defun gv-graph-body-string (root)
  (let* ((node-string (gv-node-string root))
         (child-string-list (map 'list (lambda (child) (gv-graph-body-string child))
                                 (node-children root))))
    (concatenate 'string
                 node-string
                 (reduce (lambda (x y) (concatenate 'string x y))
                         child-string-list
                         :initial-value ""))))

(defun gv-write-dot-file (expr-list output-file-name)
  (with-open-file (outf output-file-name
                        :direction :output
                        :if-exists :supersede)
    (format outf "digraph PEGTree {~%")
    (dolist (an-expr expr-list)
      (format outf "~a" (gv-graph-body-string an-expr)))
    (format outf "}~%")))

(defun gv-write (expr-string)
  (let* ((rule-list (parse 'grammar-r expr-string))
         (post-processed-rule-list (post-proc-grammar rule-list)))
    (gv-write-dot-file post-processed-rule-list "test.dot")
    post-processed-rule-list))

;;(defun gv-write-dot-file (root output-file-name)
;;  (with-open-file (outf output-file-name
;;                        :direction :output
;;                        :if-exists :supersede)
;;    (format outf "digraph PEGTree {~%")
;;    (format outf "~a" (gv-graph-body-string root))
;;    (format outf "}~%")))
