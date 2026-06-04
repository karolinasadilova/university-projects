;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 10_lisp_exercises.lisp -- řešení úloh z cvičení 10 -- Lisp
;;;

;1:
;<
(λ (a b)
    (if (zero? b)
        false
      (or (= a (pred b))
          (< a (pred b)))))
;2:
;<=
(λ (a b)
   (or (= a b)
       (< a b)))
;*
(λ (a b)
   (if (zero? b) 0
     (+ a (* a (pred b)))))
;3:
;empty:
(λ (x) true)
;null?;
(λ (p) (p (λ (x y) false)))
;null? empty=true
;null? p = false
;4
;len
(λ (list)
   (if (null? l) 0
     (1+ (len (cdr list)))))
(λ (list)
   (if (null? list) zero
     ()))

;5
