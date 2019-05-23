(defvar *instruction-list* '())
(defvar *label-map* (make-hash-table :test #'equal))
(defvar *ip* 0)
(defvar *prog* nil)

(eval-when (:compile-toplevel)
  (defvar *asm-line-rule-list* nil)
  (setf *asm-line-rule-list* nil))

(defmacro simple-token-r (rule-name token-text)
  `(defrule ,rule-name
       (and ws*-r ,token-text ws*-r)
     (:constant ,token-text)))

(defclass I-INSTRUCTION-WITH-ADDR ()
  ((addr :initarg addr)))

(defclass I-INSTRUCTION-WITH-CAPTURE-IDX ()
  ((capture-idx :initarg capture-idx)))

(defmacro simple-instruction (string-name rule-name class-name)
  (setf *asm-line-rule-list* (cons rule-name *asm-line-rule-list*))
  `(progn (defclass ,class-name () ())
          (defrule ,rule-name
              (and ws*-r ,string-name ws*-r)
            (:destructure (ws0 tk ws1)
                          (make-instance (quote ,class-name))))
          (defmethod print-object ((instr ,class-name) stream)
            (format stream "~a" ,string-name))))

(defmacro instruction-with-addr (string-name rule-name class-name)
  (setf *asm-line-rule-list* (cons rule-name *asm-line-rule-list*))
  `(progn (defclass ,class-name (I-INSTRUCTION-WITH-ADDR) ())
          (defrule ,rule-name
              (and ws*-r ,string-name ws*-r
                   (or number-r
                       identifier-r))
            (:destructure (ws0 tk ws1 addr)
                          (make-instance (quote ,class-name) 'addr addr)))
          (defmethod print-object ((instr ,class-name) stream)
            (format stream "~a(~a)" ,string-name (slot-value instr 'addr)))))

(defmacro instruction-with-capture-idx (string-name rule-name class-name)
  (setf *asm-line-rule-list* (cons rule-name *asm-line-rule-list*))
  `(progn (defclass ,class-name (I-INSTRUCTION-WITH-CAPTURE-IDX) ())
          (defrule ,rule-name
              (and ws*-r ,string-name ws*-r
                   number-r)
            (:destructure (tk capidx)
                          (make-instance (quote ,class-name) 'capture-idx)))
          (defmethod print-object ((instr ,class-name) stream)
            (format stream "~a(~a)" ,string-name (slot-value instr 'capture-idx)))))

(simple-instruction "I_FAIL" fail-r I-FAIL)
(simple-instruction "I_HALT_SUCCESSFULLY" halt-successfully-r I-HALT-SUCCESSFULLY)
(simple-instruction "I_END" end-of-list-r I-END-OF-LIST-OF-LIST)
(simple-instruction "I_RETURN" return-r I-RETURN)
(simple-instruction "I_ANY" any-char-r I-ANY-CHAR-CHAR)
(instruction-with-addr "I_CHOICE" choice-r I-CHOICE)
(instruction-with-addr "I_JUMP" jump-r I-JUMP)
(instruction-with-addr "I_CALL" call-r I-CALL)
(instruction-with-addr "I_COMMIT" commit-r I-COMMIT)
(instruction-with-addr "I_PARTIAL_commit" partial-commit-r I-PARTIAL-COMMIT)
(instruction-with-capture-idx "I_BEGIN_CAPTURE" begin-capture-r  I-BEGIN-CAPTURE)
(instruction-with-capture-idx "I_END_CAPTURE" end-capture-r I-END-CAPTURE)
(instruction-with-capture-idx "I_LEAVE_INVALIDATION_SCOPE" leave-invalidation-scope-r
                              I-LEAVE-INVALIDATAION-SCOPE)

;; I-CHAR and I-CREATE-CAPTURE-SLOTS are two oddballs that don't fit into the
;; macro framework above.
(defclass I-CHAR () ((char :initarg char)))
(eval-when (:compile-toplevel)
  (setf *asm-line-rule-list* (cons 'char-r *asm-line-rule-list*)))
(defrule char-r
    (and ws*-r "I_CHAR" ws*-r "'" character "'" ws*-r)
  (:destructure (ws0 opcode ws1 lquote ch rquote ws2)
                (make-instance 'I-CHAR 'char ch)))
(defmethod print-object ((instr I-CHAR) stream)
      (format stream "I_CHAR('~a')" (slot-value instr 'char)))

(defclass I-CREATE-CAPTURE-SLOTS () ((n-capture-slots :initarg n-capture-slots)))
(eval-when (:compile-toplevel)
  (setf *asm-line-rule-list* (cons 'create-capture-slots-r *asm-line-rule-list*)))
(defrule create-capture-slots-r
    (and ws*-r "I_CREATE_CAPTURE_SLOTS" ws*-r number-r ws*-r)
  (:destructure (ws0 opcode ws1 n ws2)
                (make-instance 'I-CREATE-CAPTURE-SLOTS 'n-capture-slots n)))
(defmethod print-object ((instr I-CREATE-CAPTURE-SLOTS) stream)
  (format stream "I_CREATE_CAPTURE_SLOTS(~a)" (slot-value instr 'n-capture-slots)))

;; Generate rule to recognize assembler instruction lines
(defmacro gen-instruction-line-r ()
  `(defrule instruction-line-r
       (and ws*-r
            (or ,@*asm-line-rule-list*))
     (:destructure (ws instr)
                (setf *instruction-list* (append *instruction-list* `(,instr)))
                (setf *ip* (1+ *ip*)))))

(gen-instruction-line-r)

(defun asm-file (lines)
  (if (null lines)
      nil
      (progn (parse 'asm-line-r (car lines))
             (asm-file (cdr lines)))))

(defrule asm-line-r
    (or (and (? label-r)
             instruction-line-r
             (? comment-r))
        (and label-line-r
             (? comment-r))
        (and blank-line-r
             (? comment-r))))

(defrule number-r
    (and ws*-r
         (? (or "+"  "-"))
         (+ (digit-char-p character))
         ws*-r)
  (:lambda (lst) (read-from-string (text lst))))

(defrule ws*-r
    (* (or #\space #\tab)))

(defrule blank-line-r
    ws*-r)

(defrule any-r character
  (:text t))

(defrule comment-r
    (and ws*-r "//" (* any-r))
  (:text t))

(defrule identifier-r
    (and (or (upper-case-p character)
             (lower-case-p character))
         (* (or (upper-case-p character)
                (lower-case-p character)
                (digit-char-p character)
                #\_)))
  (:text t))

(defrule address-r
    (or number-r
        identifier-r))

(defrule label-r
    (and identifier-r ":")
  (:destructure (id colon)
                (if (gethash id *label-map*)
                    (progn (format t "Attempted redefinition of label ~a to ~a" id *ip*)
                           (format t "Previous address is ~a" (gethash id *label-map*)))
                    (setf (gethash id *label-map*) *ip*))))

(defrule label-line-r
    (and ws*-r label-r ws*-r)
  (:destructure (ws0 label ws1)
                label))

(defun pass0 ()
  (setf *instruction-list* nil)
  (setf *label-map* (make-hash-table :test #'equal))
  (setf *ip* 0)
  (loop for line in *prog* do
       (parse 'asm-line-r line)))

(defgeneric fill-address (instr ip))

(defmethod fill-address ((instr t) ip))

(defmethod fill-address ((instr I-INSTRUCTION-WITH-ADDR) ip)
  (let ((addr (slot-value instr 'addr)))
    (typecase addr
      (string
       (let ((num-addr (gethash addr *label-map*)))
         (if (null num-addr)
             (format t "~a : label not found.~%" addr)
             (setf (slot-value instr 'addr) num-addr))))
      (number  ;; unrelativeize - i don't think this is ever used.
       (setf (slot-value instr 'addr) (+ ip addr))))))

(defun pass1 ()
  (setf *ip* 0)
  (loop for instr in *instruction-list* do
       (fill-address instr *ip*)
       (setf *ip* (1+ *ip*))))

(defun pass2 ()
  (setf *ip* 0)
  (with-open-file (outf "prog.h" :direction :output :if-exists :supersede)
    (loop for instr in *instruction-list* do
         (format outf "/* ~4,'0d : */  ~a~%" *ip* instr)
         (setf *ip* (1+ *ip*)))))

(defun get-file (fname)
  (setf *prog* nil)
  (with-open-file (fs fname :direction :input)
    (do ((line nil))
        ((equal 'EOF (setf line (read-line fs nil 'EOF))))
      (format t "~A~%" line)
      (setf *prog* (append *prog* `(,line))))))
