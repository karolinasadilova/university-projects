;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 11_exercises.lisp -- řešení úloh z cvičení 11
;;;
(defprim :pow
(let ((x (pop *rslt*)))
  (push  (* x x) *rslt*)))
(defprim :sqrt 
  (push  (sqrt (pop *rslt*)) *rslt*))

(defmacword :hyp
          :store :dup :* :rstor :dup :* :+ :sqrt)
(defmacword :discriminant
  c :bind 
  b :bind 
  a :bind 
  b :val :pow 
  4 
  a :val :* 
  c :val :* :- 
   :unbind 
   :unbind 
   :unbind)
(defprim :and
  (let ((b (pop *rslt*))
        (a (pop *rslt*)))
    (push (and a b) *rslt*)))
(defprim :not
  (let ((x (pop *rslt*)))
    (push (not x) *rslt*)))
(defprim :or
  (let ((b (pop *rslt*))
        (a (pop *rslt*)))
    (push  (or a b) *rslt*)))
;(defmacword :power
;  :lbl b :bind a :bind b :val 0 := :if a :val b :val 1 :- :power :* :else 1 :cjmp :then)
(execute '(
           b :bind
              a :bind
              b :val
              0
              :=
              :if
              ;1
              ;:else
              a :val
              a :val
              b :val
              1
              :-
              power
              :val
              :call
              :*
              :else
              1
              :then
              :ret)
         
              'power
              :bind
              nil)
              
(execute '(n :bind
             n
             :val
             0
             =

             :if

             :store
             n
             :val
             1
             :-
             n
             :set
             dig
             :val
             :call
             :else
             n
             :val
             :dup
             :then
             :ret)
         'dig
         :bind
nil)
(execute '(n :bind
             n
             :val 
             1
             :=
             :if
             n :val
             n :val
             1 :-
             +n 
             :val
             :call
             :+
             :else
             
             
             :then 
 :unbind :ret)
         '+n :bind nil)
(execute '(n :bind
             n :val
             +n
             :val
             :call
             n :val
             :/ :unbind :ret )
         'avrg
         :bind nil)
         
