;;; ~/dotfiles/config/.config/doom/test.el -*- lexical-binding: t; -*-
;; 匿名函數
(lambda (x) (* x 3))

;; 有名字的函數
(defun triple (x)
   (* x 3))

;; 匿名函數
((lambda (x) (* x 3)) 7)
;; => 得到 21

;; 有名字的函數
(triple 7)
;; => 一樣也是 21
 ;; 有名字的函數
(let ((a 3))
  (defun triple (x)
    (* a x))
  (triple 7))  ;; 這個 (triple 7) 是在 let 中呼叫，可以正常運作，得到 21

(triple 7)  ;; 炸掉啦！Debugger entered--Lisp error: (void-variable a)

(?A)

(eq ?A ())

(let ((birch 3)
      pine
      fir
      (oak 'some))
  (message
  "Here are %d variables with %s, %s, and %s value."
  birch pine fir oak))


(setq trees '((pine . cones) (oak . acorns) (maple . seeds)))

(seq-elt [1 2 3 4] 3)

(setq primes [2 3 5 7 11 13])

(elt primes 3)

(recordp #s(a))

(record 'foo 23 [bar baz] "rats")

(defvar *var* 1)

(boundp '*var*) ; 返回真

(fboundp '*var*) ; 返回假

(defun *var* (x) x) ; 定义一个名为*var*的函数，返回值即为参数

(fboundp '*var*) ; 返回真

(*var* *var*)

(symbol-value *var*) ; wrong


(symbol-value '*var*) ; right

(defun foo (j k) (eq j k))

(defun search-foo ()
  (interactive)
  (debug)
  (catch 'loop
    (let ((i 0))
      (while (< i 10)
        (let ((j 0))
          (while (< j 10)
            (if (foo i j)
                (throw 'loop (list i j)))
            (setq j (1+ j))))
        (setq i (1+ i))))))

(defvar x -99)      ; x receives an initial value of −99.

(defun addx ()
  (setq x (1+ x)))  ; Add 1 to x and return its new value.

(let ((x 1))
  (addx)
  (addx))

(lambda (x) (* x x))

(display-buffer)


(let (result)
  (dolist (elt '(- 0 1 2 3 4) result)
    (push elt result)))


(defmacro ulss (cond &rest body)
  "If COND yields nil, do BODY, else return nil.
When COND yields nil, eval BODY forms sequentially and return
value of last one, or nil if there are none.

\(fn COND BODY...)"
  (declare (indent 1) (debug t))
  (list 'if cond nil body))

;; (setq org-reveal-klipsify-src t)

(ulss (eq 2 3) message "2")

(macroexpand-all '( when (eq 2 2) ( message "2" ) ))


(defun inc (var)
    (list 'setq var (list '1+ var)))

'(1 21 ,@(2 3))


(defvar x 0)

(inc 'x)

(eval (inc 'x))

(progn)

(defmacro create-inc ()
    `(defmacro inc (var)
        `(setq ,var (1+ ,var))))

(create-inc)    ; 定义了inc
(defvar x 0)

(inc x)

(defmacro create-inc-n (num)
    `(defmacro inc-n (var)
        `(setq ,var (+ ,',num ,var))))

(macroexpand-all '(create-inc-n 3))

(create-inc-n 3)

(inc-n x)


(--> "def" (concat "abc" it "ghi") (upcase it))
(--all? (= 0 (% it 2)) '(2 4 6))

(defmacro alambda (parms &rest body)
  `(labels ((self ,parms ,@body))
     #'self))


(--annotate (< 1 it) '(0 1 2 3))
