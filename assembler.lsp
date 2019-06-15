(defvar *instruction-list* '())
(defvar *label-map* (make-hash-table :test #'equal))
(defvar *ip* 0)
(defvar *prog* nil)

(defconstant PATH-MAX 4096)

(eval-when (:compile-toplevel)
  (defvar *asm-line-rule-list* nil)
  (defvar *opcode-table* (make-hash-table))
  (setf *asm-line-rule-list* nil)
  (setf (gethash 'I-END-OF-LIST *opcode-table*) 0)
  (setf (gethash 'I-FAIL *opcode-table*) 1)
  (setf (gethash 'I-CHAR *opcode-table*) 2)
  (setf (gethash 'I-ANY *opcode-table*) 3)
  (setf (gethash 'I-CHOICE *opcode-table*) 4)
  (setf (gethash 'I-JUMP *opcode-table*) 5)
  (setf (gethash 'I-CALL *opcode-table*) 6)
  (setf (gethash 'I-RETURN *opcode-table*) 7)
  (setf (gethash 'I-COMMIT *opcode-table*) 8)
  (setf (gethash 'I-PARTIAL-COMMIT *opcode-table*) 9)
  (setf (gethash 'I-BEGIN-CAPTURE *opcode-table*) 10)
  (setf (gethash 'I-END-CAPTURE *opcode-table*) 11)
  (setf (gethash 'I-REENTER-INVALIDATION-SCOPE *opcode-table*) 12)
  (setf (gethash 'I-LEAVE-INVALIDATION-SCOPE *opcode-table*) 13)
  (setf (gethash 'I-RUN-SEMANTIC-ACTION *opcode-table*) 14)
  (setf (gethash 'I-CREATE-CAPTURE-SLOTS *opcode-table*) 15)
  (setf (gethash 'I-RANGE *opcode-table*) 16)
  (setf (gethash 'I-HALT-SUCCESSFULLY *opcode-table*) 17))

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
          (defmethod instr-binary-code ((instr ,class-name))
            (values (gethash (quote ,class-name) *opcode-table*)))
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
          (defmethod instr-binary-code ((instr ,class-name))
            (values (gethash (quote ,class-name) *opcode-table*)
                    (slot-value instr 'addr)))
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
          (defmethod instr-binary-code ((instr ,class-name))
            (values (gethash (quote ,class-name) *opcode-table*)
                    (slot-value instr 'capture-idx)))
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
(instruction-with-addr "I_PARTIAL_COMMIT" partial-commit-r I-PARTIAL-COMMIT)
(instruction-with-capture-idx "I_BEGIN_CAPTURE" begin-capture-r  I-BEGIN-CAPTURE)
(instruction-with-capture-idx "I_END_CAPTURE" end-capture-r I-END-CAPTURE)
(instruction-with-capture-idx "I_LEAVE_INVALIDATION_SCOPE" leave-invalidation-scope-r
                              I-LEAVE-INVALIDATAION-SCOPE)

(defrule quoted-char
    (and ws*-r "'" character "'" ws*-r)
  (:destructure (ws0 lquote ch rquote ws1)
                ch))

;; Pack two one-byte chars into a 16 bit value: little-endian the way GWAD intended.
(defun pack-2chars-into-u16 (low-char hi-char)
  (logior (char-code low-char) (ash (char-code hi-char) 8)))

;; GWAD - I hate languages that distinguish between chars/bytes/ints
(defun pack-2bytes-into-u16 (low-byte hi-byte)
  (logior low-byte (ash hi-byte 8)))

(defun low-byte-of (u16)
  (mod u16 256))

(defun hi-byte-of (u16)
  (mod (ash u16 -8) 256))

(defun char-of-low-byte (u16)
  (code-char (low-byte-of u16)))

(defun char-of-hi-byte (u16)
  (code-char (hi-byte-of u16)))

;; I-CHAR, I-RANGE and I-CREATE-CAPTURE-SLOTS are two oddballs that don't fit into the
;; macro framework above.

;;---- begin I-CHAR

(defclass I-CHAR () ((char :initarg char)))

(eval-when (:compile-toplevel)
  (setf *asm-line-rule-list* (cons 'char-r *asm-line-rule-list*)))

(defrule char-r
    (and ws*-r "I_CHAR" quoted-char)
  (:destructure (ws0 opcode ch)
                (make-instance 'I-CHAR 'char ch)))

(defmethod instr-binary-code ((instr I-CHAR))
  (values (gethash 'I-CHAR *opcode-table*)
          (slot-value instr 'char)))

(defmethod print-object ((instr I-CHAR) stream)
  (format stream "I_CHAR('~a')" (slot-value instr 'char)))

;;---- end I-CHAR

;;---- begin I-CREATE-CAPTURE-SLOTS

(defclass I-CREATE-CAPTURE-SLOTS () ((n-capture-slots :initarg n-capture-slots)))

(eval-when (:compile-toplevel)
  (setf *asm-line-rule-list* (cons 'create-capture-slots-r *asm-line-rule-list*)))

(defrule create-capture-slots-r
    (and ws*-r "I_CREATE_CAPTURE_SLOTS" number-r)
  (:destructure (ws0 opcode n)
                (make-instance 'I-CREATE-CAPTURE-SLOTS 'n-capture-slots n)))

(defmethod instr-binary-code ((instr I-CREATE-CAPTURE-SLOTS))
  (values (gethash 'I-CREATE-CAPTURE-SLOTS *opcode-table*)
          (slot-value instr 'n-capture-slots)))

(defmethod print-object ((instr I-CREATE-CAPTURE-SLOTS) stream)
  (format stream "I_CREATE_CAPTURE_SLOTS(~a)" (slot-value instr 'n-capture-slots)))

;;---- end I-CREATE-CAPTURE-SLOTS

;;---- begin I-RANGE

(defclass I-RANGE () ((begin-char :initarg begin-char)
                      (end-char :initarg end-char)))
(eval-when (:compile-toplevel)
  (setf *asm-line-rule-list* (cons 'range-r *asm-line-rule-list*)))

(defmethod instr-binary-code ((instr I-RANGE))
  (values (gethash 'I-RANGE *opcode-table*)
          (pack-2chars-into-u16 (slot-value instr 'begin-char)
                                (slot-value instr 'end-char))))

(defrule comma-tk
    (and ws*-r "," ws*-r)
  (:constant ","))

(defrule range-r
    (and ws*-r "I_RANGE" quoted-char comma-tk quoted-char)
  (:destructure (ws0 instr begin-char comma end-char)
                (make-instance 'I-RANGE
                               'begin-char begin-char
                               'end-char end-char)))

(defmethod print-object ((instr I-RANGE) stream)
  (format stream "I-RANGE('~a', '~a')"
          (slot-value instr 'begin-char)
          (slot-value instr 'end-char)))

;; ---- end I-RANGE

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


(defun write-unsigned-16-bit-in-little-endian (stream u16)
  (write-byte (ldb (byte 8 0) u16) stream)
  (write-byte (ldb (byte 8 8) u16) stream))

(defun copies (x n)
  (if (<= n 0)
      nil
      (cons x (copies x (1- n)))))

(defun group-by-2-adjacent-elements (L)
  (cond ((null L)
         nil)
        ((= 1 (length L))
         (cons (cons (car L) 0)
               nil))
        (t
         (cons (cons (car L) (cadr L))
               (group-by-2-adjacent-elements (cddr L))))))

(defun n-words-required (n)
  (+ (truncate (/ n 2))
     (mod n 2)))

(defun zero-pad-packed-string (str field-width)
  (mapcar (lambda (lo.hi) (pack-2bytes-into-u16 (car lo.hi) (cdr lo.hi)))
                  (group-by-2-adjacent-elements
                   (append (map 'list (lambda (x) (char-code x)) str)
                           (copies 0 (n-words-required (- field-width (length str))))))))

(defun key-value-bytes (k.v)
  (let ((key (car k.v))
        (value (cdr k.v)))
    `(,@(zero-pad-packed-string key 10)
        ;; little-endian: (lobyte hibyte), the way GWAD intended bytes to be ordered.
        ,@(list (mod value 255)
                (mod (ash value -8) 255)))))

(defun symbol-table-bytes ()
  (let ((sorted-label-map-as-alist
        (sort (alexandria:hash-table-alist *label-map*)
          (lambda (k0.v0 k1.v1) (string<= (car k0.v0) (car k1.v1))))))
    (reduce #'append (mapcar #'key-value-bytes sorted-label-map-as-alist) :initial-value nil)))

(defun dump-word-list (L)
  (let ((column 0)
        (ofs 0))
    (loop for word in L
       do (progn (if (= 0 (mod column 16))
                     (progn (if (> ofs 0)
                                (format t "~%"))
                            (format t "~4,'0X : " ofs)
                            (setf column 0)))
                 (format t "~4,'0X " word)
                 (setf column (1+ column))
                 (setf ofs (1+ ofs))))))

;; left off here
(defun pass2-binary (output-file-name)
  (setf *ip* 0)
  (with-open-file (outf output-file-name
                        :direction :output
                        :if-exists :supersede
                        :element-type 'unsigned-byte)
    (let ((the-big-giant-output-buffer
           `(0 ; placeholder for file size until its known
             ,@(zero-pad-string "/home/todd/Projects/Parsing-Machine/prog.so" 100) ; PATH-MAX)
               ,(length *instruction-list*)
               ,(hash-table-count *label-map*)
               ,@(mapcar #'instr-binary-code *instruction-list*)
               ,@(symbol-table-bytes))))
      (setf the-big-giant-output-buffer
            (cons (* 2 (mod (length the-big-giant-output-buffer)
                            65536))
                  (cdr the-big-giant-output-buffer)))
      (loop for word in the-big-giant-output-buffer
         do (let ((lo-byte (mod word 256))
                  (hi-byte (mod (ash word -8) 256)))
              (write-byte lo-byte outf)
              (write-byte hi-byte outf)))
      the-big-giant-output-buffer)))

(defun pass2-text-compilable ()
  (setf *ip* 0)
  (with-open-file (outf "prog.h" :direction :output :if-exists :supersede)
    (loop for instr in *instruction-list* do
         (format outf "/* ~4,'0d : */  ~a~%," *ip* instr)
         (setf *ip* (1+ *ip*)))))

(defun get-file (fname)
  (setf *prog* nil)
  (with-open-file (fs fname :direction :input)
    (do ((line nil))
        ((equal 'EOF (setf line (read-line fs nil 'EOF))))
      (format t "~A~%" line)
      (setf *prog* (append *prog* `(,line))))))
