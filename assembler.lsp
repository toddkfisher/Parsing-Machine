(defstruct I-FAIL)
(defstruct I-HALT-SUCCESSFULLY)
(defstruct I-END-OF-LIST)
(defstruct I-RETURN)
(defstruct I-CHAR char)
(defstruct I-ANY-CHAR)
(defstruct <I-INSTR-WITH-ADDR> addr)
(defstruct (I-CHOICE (:include <I-INSTR-WITH-ADDR>)))
(defstruct (I-JUMP (:include <I-INSTR-WITH-ADDR>)))
(defstruct (I-CALL (:include <I-INSTR-WITH-ADDR>)))
(defstruct (I-COMMIT (:include <I-INSTR-WITH-ADDR>)))
(defstruct (I-PARTIAL-COMMIT (:include <I-INSTR-WITH-ADDR>)))

(defvar *instruction-list* '())
(defvar *label-map* (make-hash-table :test #'equal))
(defvar *ip* 0)

;;
;; S => A | B | C
;; A => 'a'
;; B => 'b'
;; C => 'c'
;;
(defvar *prog* `("S: choice L0"                ;; 0000
                 "call A"                      ;; 0001
                 "commit End"                  ;; 0002
                 "L0: choice L1"               ;; 0003
                 "call B"                      ;; 0004
                 "commit End"                  ;; 0005
                 "L1: choice L2"               ;; 0006
                 "call C"                      ;; 0007
                 "commit End"                  ;; 0008
                 "L2: fail"                    ;; 0009
                 "End: halt-successfully"      ;; 0010
                 "A: char 'a'"                 ;; 0011
                 "return"                      ;; 0012
                 "B: char 'b'"                 ;; 0013
                 "return"                      ;; 0014
                 "C: char 'c'"                 ;; 0015
                 "return"))                    ;; 0016

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

(defrule instruction-line-r
    (and ws*-r
         (or fail-r
             halt-successfully-r
             return-r
             any-char-r
             choice-r
             jump-r
             call-r
             commit-r
             char-r
             partial-commit-r))
  (:destructure (ws instr)
                (setf *instruction-list* (append *instruction-list* `(,instr)))
                (setf *ip* (1+ *ip*))))

(defrule fail-r
    "fail"
  (:constant (make-I-FAIL)))

(defrule halt-successfully-r
    "halt-successfully"
  (:constant (make-I-HALT-SUCCESSFULLY)))

(defrule return-r
    "return"
  (:constant (make-I-RETURN)))

(defrule char-r
    (and "char" ws*-r "'" character "'")
  (:destructure (opcode ws0 lquote ch rquote)
                (make-I-CHAR :char ch)))

(defrule any-char-r
    "any-char"
  (:constant (make-I-ANY-CHAR)))

(defrule jump-r
    (and "jump" ws*-r address-r)
  (:destructure (opcode ws addr)
                (make-I-JUMP :addr addr)))

(defrule call-r
    (and "call" ws*-r address-r)
  (:destructure (opcode ws addr)
                (make-I-CALL :addr addr)))

(defrule choice-r
    (and "choice" ws*-r address-r)
  (:destructure (opcode ws addr)
                (make-I-CHOICE :addr addr)))

(defrule commit-r
    (and "commit" ws*-r address-r)
  (:destructure (opcode ws addr)
                (make-I-COMMIT :addr addr)))

(defrule partial-commit-r
    (and "partial-commit" ws*-r address-r)
  (:destructure (opcode ws addr)
                (make-I-PARTIAL-COMMIT :addr addr)))

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

(defun fill-address (instr ip)
  (let ((addr (<I-INSTR-WITH-ADDR>-addr instr)))
    ;; (format t "(type-of ~A) = ~A~%" addr (type-of addr))
    (typecase addr
      (string
       (let ((num-addr (gethash addr *label-map*)))
         (if (null num-addr)
             (format t "~A : label not found.~%" addr)
             (setf (<I-INSTR-WITH-ADDR>-addr instr) num-addr))))
      (number
       (setf (<I-INSTR-WITH-ADDR>-addr instr) (+ ip addr))))))

(defun pass1 ()
  (setf *ip* 0)
  (loop for instr in *instruction-list* do
       (typecase instr
         (I-CHOICE (fill-address instr *ip*))
         (I-JUMP (fill-address instr *ip*))
         (I-CALL (fill-address instr *ip*))
         (I-COMMIT (fill-address instr *ip*))
         (I-PARTIAL-COMMIT (fill-address instr *ip*)))
       (setf *ip* (1+ *ip*))))

(defun pass2 ()
  (setf *ip* 0)
  (with-open-file (outf "prog.h" :direction :output :if-exists :overwrite)
    (loop for instr in *instruction-list* do
         (format outf "/* ~4,'0d : */  " *ip*)
         (typecase instr
           (I-FAIL
            (format outf "I_FAIL,~%"))
           (I-HALT-SUCCESSFULLY
            (format outf "I_HALT_SUCCESSFULLY,~%"))
           (I-RETURN
            (format outf "I_RETURN,~%"))
           (I-CHAR
            (format outf "I_CHAR('~A'),~%" (I-CHAR-char instr)))
           (I-ANY-CHAR
            (format outf "I_ANY,~%"))
           (I-CHOICE
            (format outf "I_CHOICE(~A),~%" (I-CHOICE-addr instr)))
           (I-JUMP
            (format outf "I_JUMP(~A),~%" (I-JUMP-addr instr)))
           (I-CALL
            (format outf "I_CALL(~A),~%" (I-CALL-addr instr)))
           (I-COMMIT
            (format outf "I_COMMIT(~A),~%" (I-COMMIT-addr instr)))
           (I-PARTIAL-COMMIT
            (format outf "I_PARTIAL_COMMIT(~A),~%" (I-PARTIAL-COMMIT-addr instr))))
         (setf *ip* (1+ *ip*)))))

(defun get-file (fname)
  (setf *prog* nil)
  (with-open-file (fs fname :direction :input)
    (do ((line nil))
        ((equal 'EOF (setf line (read-line fs nil 'EOF))))
      (format t "~A~%" line)
      (setf *prog* (append *prog* `(,line))))))
