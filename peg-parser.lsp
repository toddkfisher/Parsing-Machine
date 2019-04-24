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
                (make-ND-CAPTURE :capture-name nm)))

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
    (or call-r
        text-r
        paren-expr-r)
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
    (and #\'
         string-with-escapes-r
         #\')
  (:destructure (l-quote str r-quote)
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

(defun peg-compile (root)
  (let ((instruction-list nil))
    (if root
        (typecase root
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
