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
