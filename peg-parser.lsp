(defstruct ND-ORDERED-CHOICE :left :right)
(defstruct ND-RULE :name :expr :semantic-action)
(defstruct ND-NAME :name)
(defstruct ND-CONCAT :left :right)
(defstruct ND-CAPTURE :capture-name :capture-expr)
(defstruct ND-0-OR-1 :expr)
(defstruct ND-0-OR-MANY :expr)
(defstruct ND-1-OR-MANY :expr)
(defstruct ND-NOT :expr)
(defstruct ND-TEXT :text)
(defstruct ND-CALL-RULE :rule-name)
(defstruct ND-REENTER-INVALIDATION-SCOPE :capture-names :expr)
(defstruct ND-LEAVE-INVALIDATION-SCOPE :capture-names :expr)

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

(defrule grammar-r
    (and rule-r (? (and semicolon-tk grammar-r)))
  (:destructure (first-rule semicolon-other-rules*)
                (cons first-rule (cadr semicolon-other-rules*))))

(defrule expression-r
    (and term-r (? (and ordered-choice-op-tk expression-r)))
  (:destructure (term oc-op-expr)
                (if oc-op-expr
                    (make-ND-ORDERED-CHOICE :left term
                                            :right (cadr oc-op-expr))
                    term)))

(defrule term-r
    (and factor-r (? term-r))
  (:destructure (fct concat-expr)
                (if concat-expr
                    (make-ND-CONCAT :left fct :right concat-expr)
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
                                 (make-ND-0-OR-1 :expr simp-fac))
                                ((string= not-op "~")
                                 (make-ND-NOT :expr simp-fac))
                                ((string= sffx-op "*")
                                 (make-ND-0-OR-MANY :expr simp-fac))
                                ((string= sffx-op "+")
                                 (make-ND-1-OR-MANY :expr simp-fac))
                                (t
                                 simp-fac))))
                  (if capture-name
                      (make-ND-CAPTURE :capture-name capture-name
                                       :capture-expr nd)
                      nd))))

(defrule paren-expr-r
    (and lparen-tk expression-r rparen-tk)
  (:destructure (lp expr rp)
                expr))

(defrule call-r
    name-tk
  (:lambda (nm)
    (make-ND-CALL-RULE :rule-name nm)))

(defrule simple-factor-r
    (or text-r
        call-r
        paren-expr-r)
  (:identity t))

(defrule test-rule-r
    (+ text-r )
  (:identity t))

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
    (and ws*-r
         #\'
         string-with-escapes-r
         #\'
         ws*-r)
  (:destructure (ws0 l-quote str r-quote ws1)
                (make-ND-TEXT :text (text str))))

(defrule rule-r
    (and name-tk arrow-tk expression-r (? semantic-action-r))
  (:destructure (nm arr expr sem-action)
                (make-ND-RULE :name nm :expr expr :semantic-action sem-action)))

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

(defmacro add-multiple-instructions (instruction-list-name instruction-list-to-add)
  `(setf ,instruction-list-name (append ,() instruction-list-to-add)))

(defvar *instruction-list* nil)
(defvar *label-idx* 0)

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
                       `(multiple-value-bind ,var-list ,value ,(expand-mv-bindings (cdr vars-value*))))))))
    (expand-mv-bindings vars-value*)))

(defun add-invalidation-scopes (root active-capture-names)
  (if root
      (typecase root
        (ND-ORDERED-CHOICE ;;
         ;;
         ;; (0) Every capture currently in invalidation scope has that scope suspended on the
         ;;     left branch of an ordered choice.  Active captures do not get passed down
         ;;     the left branch.  Active captures do get passed to the right branch since failure of the
         ;;     right branch means that there are no more alternatives and hence, the entire ordered choice
         ;;     expression has failed.
         ;; (1) The captures introduced on the left branch do not have an invalidation scope on
         ;;     the right branch of ordered choice.
         ;;
         (vlet* (((left-root left-branch-capture-set)
                  (add-invalidation-scopes (ND-ORDERED-CHOICE-left root) nil))
                 ((right-root new-capture-set)
                  (add-invalidation-scopes (ND-ORDERED-CHOICE-right root) active-capture-names)))
                (let ((new-left-root (make-ND-LEAVE-INVALIDATION-SCOPE :capture-names active-capture-names
                                                                       :expr left-root))
                      (new-right-root (make-ND-REENTER-INVALIDATION-SCOPE :capture-names active-capture-names
                                                                          :expr right-root)))
                  (make-ND-ORDERED-CHOICE :left new-left-root
                                          :right new-right-root))))
        (ND-RULE ;;
         (values root
                 (add-invalidation-scopes (ND-RULE-expr root)
                                          active-capture-names)))
        (ND-NAME ;;
         (values root
                 active-capture-names))
        (ND-CONCAT ;;
         ;;
         ;; (0) Captures currently within invalidation scope are passed down the left branch.
         ;; (1) New captures may enter the current scope from the left branch so we will
         ;;     receive a (possibly) expanded set of capture names.
         ;; (2) We also receive a new (possibly modified) ldft branch of this operator.
         ;; (3) The new capture set from (1) is passed to the right branch and we also receive a (possibly)
         ;;     even new capture set.
         ;; (4) We join the new left and right branches with a 'concat' opertor and pass the newly created concat node and
         ;;     newer capture set back up to the caller.
         ;;
         (vlet* (((new-left new-capture-set)
                  (add-invalidation-scopes (ND-CONCAT-left root) active-capture-names))
                 ((new-right newer-capture-set)
                  (add-invalidation-scopes (ND-CONCAT-right root) new-capture-set)))
                (values (make-ND-CONCAT :left new-left :right new-right)
                        newer-capture-set)))
        (ND-CAPTURE ;;
         (let ((new-capture-set (adjoin (ND-CAPTURE-capture-name root)
                                        active-capture-names)))
           ;;
           ;; (0) After adding the new capture name to the current capture set, we pass the new set down to the
           ;;     expression being captured.
           ;; (1) We receive a (possibly) enlarged capture set and a (possibly) modified expression.
           ;; (2) Join the capture name and new capture expression with a capture node.
           ;;
           (vlet* (((new-capture-expr newer-capture-set)
                    (add-invalidation-scopes (ND-CAPTURE-capture-expr root) new-capture-set)))
                  (values (make-ND-CAPTURE :capture-name (ND-CAPTURE-capture-name root)
                                           :capture-expr new-capture-expr)
                          newer-capture-set))))
        (ND-0-OR-1  ;;
         ;;
         ;; '<expr>?' never invalidates any captures.  Only new captures introduced in <expr> can be invalidated.
         ;; Therefore, we pass nothing to the expression subordinate to '?'  However, <expr> may introduce new
         ;; capture names into the invalidation scope so we need to pick those up so that they get passed to
         ;; the right branch.
         ;;
         (vlet* (((new-expr new-capture-set)
                  (add-invalidation-scopes (make-ND-0-OR-1-expr :expr root) nil)))
                (values (make-ND-0-OR-1 :expr new-expr)
                        new-capture-set)))
        (ND-0-OR-MANY  ;; a* = (| a | aa | aaa | aaaa | ... )
         ;;
         ;; The situation with ND-0-OR-MANY is exactly equivalent to ND-0-OR-1
         ;;
         (vlet* (((new-expr new-capture-set)
                  (add-invalidation-scopes (make-ND-0-OR-1-expr :expr root) nil)))
                (values (make-ND-0-OR-1 :expr new-expr)
                        new-capture-set)))
        (ND-1-OR-MANY ;;
         ;;
         ;; In 'a+', 'a' must always succeed at least once and therefore it can invalidate any
         ;; currently active captures.  And, any captures introduced can also be invalidated
         ;; further down the line.
         ;;
         (vlet* (((new-expr new-capture-set)
                  (add-invalidation-scopes (ND-1-OR-MANY-expr root) active-capture-names)))
                (make-ND-1-OR-MANY :expr (add-invalidation-scopes new-expr new-capture-set))))
        (ND-NOT
         (values root active-capture-names))
        (ND-TEXT
         (values root active-capture-names))
        (ND-CALL-RULE
         (values root active-capture-names))
        (ND-REENTER-INVALIDATION-SCOPE  ;; Shouldn't happen
         (values root active-capture-names))
        (ND-LEAVE-INVALIDATION-SCOPE ;; Shouldn't happen
         (values root active-capture-names)))))

(defun peg-compile (root)
  (let ((instruction-list nil))
    (if root
        (typecase root
          (ND-CAPTURE
           (add-instruction *instruction-list* (format nil "begin-capture ~a" (ND-CAPTURE-capture-name root)))
           (peg-compile (ND-CAPTURE-capture-expr root))
           (add-instruction *instruction-list* (format nil "end-capture ~a" (ND-CAPTURE-capture-name root))))
          (ND-REENTER-INVALIDATION-SCOPE
           (loop for capture-name in (ND-REENTER-INVALIDATION-SCOPE-capture-names root)
              do (add-instruction *instruction-list* (format nil "reenter-invalidation-scope ~a" capture-name)))
           (peg-compile (ND-REENTER-INVALIDATION-SCOPE-expr root)))
          (ND-LEAVE-INVALIDATION-SCOPE
           (loop for capture-name in (ND-LEAVE-INVALIDATION-SCOPE-capture-names root)
              do (add-instruction *instruction-list* (format nil "leave-invalidation-scope ~a" capture-name)))
           (peg-compile (ND-LEAVE-INVALIDATION-SCOPE-expr root)))
          (ND-CONCAT
           (progn (peg-compile (ND-CONCAT-left root))
                  (peg-compile (ND-CONCAT-right root))))
          (ND-CALL-RULE
           (add-instruction *instruction-list* (format nil "call ~a" (ND-CALL-RULE-rule-name root))))
          (ND-TEXT
           (do ((i 0 (+ i 1))
                (txt (ND-TEXT-text root))
                (len (length (ND-TEXT-text root))))
               ((>= i len))
             (add-instruction *instruction-list* (format nil "char '~a'" (elt txt i)))))
          (ND-RULE
           (progn
             (add-instruction *instruction-list* (format nil "~a:  // Rule" (ND-RULE-name root)))
             (peg-compile (ND-RULE-expr root))))
          (ND-ORDERED-CHOICE
           (let ((L0 (new-label))
                 (L1 (new-label)))
             (add-instruction *instruction-list* (format nil "choice ~a" L0))
             (peg-compile (ND-ORDERED-CHOICE-left root))
             (add-instruction *instruction-list* (format nil "commit ~a" L1))
             (add-instruction *instruction-list* (format nil "~a:" L0))
             (peg-compile (ND-ORDERED-CHOICE-right root))
             (add-instruction *instruction-list* (format nil "~a:" L1))))
          (ND-0-OR-MANY
           (let ((L0 (new-label))
                 (L1 (new-label)))
             (add-instruction *instruction-list* (format nil "choice ~a" L0))
             (add-instruction *instruction-list* (format nil "~a:" L1))
             (peg-compile (ND-0-OR-MANY-expr root))
             (add-instruction *instruction-list* (format nil "partial-commit ~a" L1))
             (add-instruction *instruction-list* (format nil "~a:" L0))))
          (ND-0-OR-1
           (let ((L (new-label)))
             (add-instruction *instruction-list* (format nil "choice ~a" L))
             (peg-compile (ND-0-OR-1-expr root))
             (add-instruction *instruction-list* (format nil "commit ~a" L))
             (add-instruction *instruction-list* (format nil "~a:" L))))
          (ND-1-OR-MANY
           (let ((L0 (new-label))
                 (L1 (new-label)))
             (add-instruction *instruction-list* (format nil "~a:" L0))
             (peg-compile (ND-1-OR-MANY-expr root))
             (add-instruction *instruction-list* (format nil "choice ~a" L1))
             (add-instruction *instruction-list* (format nil "partial-comit ~a" L0))
             (add-instruction *instruction-list* (format nil "~a:" L1))))))))
