;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 06_exercises.lisp -- řešení úloh z cvičení 6
;;;
(defun *& (a b cont)
  (funcall cont (* a b)))
(defun -& (a b cont)
  (funcall cont (- a b)))
(defun discriminant (a b c cont)
  (*& b b (lambda (c1)
            (*& a c (lambda (c2)
                      (*& 4 c2 (lambda (c3)
                                 (-& c1 c3 cont))))))))
(defun null& (list cont)
  (funcall cont (null list)))

(defun cdr& (list cont)
  (funcall cont (cdr list)))
(defun car& (list cont)
  (funcall cont (car list)))
(defun cons& (a b cont)
  (funcall cont (cons a b)))
#|(defun reverse-list (list)
  (m-revappend list nil))
(defun m-revappend (list1 list2)
  (if (null list1)  list2
    (m-revappend (cdr list1) (cons (car list1) list2))))
|#

(defun reverse& (list cont)
  (revappend& list '() cont))
(defun revappend& (list1 list2 cont)
  (null& list1 (lambda (c1)
                 (if c1 
                     (funcall cont list2)
                    (cdr& list1 (lambda (c2)
                                  (car& list1 (lambda (c3)
                                                (cons& c3 list2 (lambda (c4)                                                      (revappend& c2 c4 cont)))))))))))

(defun my-app2 (l1 l2)
  (if (null l1) l2
    (cons (car l1) 
          (my-app2 (cdr l1)
                   l2))))
(defun append2& (l1 l2 cont)
  (null& l1 (lambda (c1)
              (if c1
                  (funcall cont l2)
                (car& l1 (lambda (c2)
                           (cdr& l1 (lambda (c3)
                                           (append2& c3 l2 (lambda (c4)
                                                             (cons& c2 c4 cont)))))))))))


                                                            
(defun struct-find-if-2 (pred struct)
  (cond ((atom struct) 
         (when (funcall pred struct) struct))
        ((consp struct)
           (or (struct-find-if-2 pred (car struct))
               (struct-find-if-2 pred (cdr struct))))))



(defun struct-find-if-help (node value pred visited cont)
  (cond (value value)
        ((find node visited)
         (funcall cont nil visited))
        ((atom node)
         (if (funcall pred node)
             (funcall cont node visited)
           (funcall cont nil visited)))
        (t (struct-find-if-help (car node)
                           value
                           pred
                           visited
                           (lambda (value2 visited2)
                             (struct-find-if-help (cdr node)
                                             value2
                                             pred
                                             visited2
                                             cont))))))
        
(defun struct-find-if (node pred)
  (struct-find-if-help node nil pred nil #'proj))


        
(defun disc (a b c cont)
  (*& b b (lambda (c1)
            (*& 4 a (lambda (c2)
                      (*& c2 c (lambda (c3)
                                 (-& c1 c3 cont))))))))
(defun revapp& (list1 list2 cont)
  (null& list1 (lambda (c1)
                (if c1 (funcall cont list2)
                  (cdr& list1 (lambda (c2)
                                (car& list1 (lambda (c3)
                                              (cons& c3 list2 (lambda (c4) (revapp& c2 c4 cont)))))))))))
                                                                
                                                                
        
(defun rever& (list cont)
  (revapp& list nil cont))
(defun app& (l1 l2 cont)
  (null& l1 
         (lambda (c1)
           (if c1 (funcall cont l2)
             (cdr& l1 (lambda (c2)
                        (car& l1 (lambda (c3)
                                   (app& c2 l2 (lambda (c4)
                                                  (cons& c3 c4 cont)))))))))))

(defun remove& (el list cont)
  (null& list (lambda (c1)
                (if c1 (funcall cont nil)
                  (car& list (lambda (c2)
                               (cdr& list (lambda (c4)
                                            (eql& c2 el (lambda (c3)
                                             (if c3 (remove& el c4 cont)
                                               (remove& el c4 (lambda (c5)
                                                                (cons& c2 c5 cont))))))))))))))
                                                 
                               
;;vzdy si to napsat normalne a pak prepisovat do cps


(defun eql& (a b cont)
  (funcall cont (eql a b)))
(defun str-find-if& (struct pred cont)
  (null& struct (lambda (c1)
                  (if c1 (funcall cont nil)
                    (atom& struct (lambda (c2)
                                    (if c2 (if (funcall pred struct) (funcall cont struct) (funcall cont nil))
                                      (car& struct (lambda (c3)
                                                     (str-find-if& c3 pred (lambda (c5) 
                                                                        (if c5 (funcall cont c5) 
                                                                          (cdr& struct (lambda (c6)
                                                                                         (str-find-if& c6 pred cont)))))))))))))))
                                                           
                    

(defun stfif (s pred)
  (cond ((null s) nil)
        ((atom s)
         (if (funcall pred s) s nil))
        (t (or (stfif (car s) pred)
               (stfif (cdr s) pred)))))

;(defun consp (s)
 ; (not (atom s)))
(defun atom& (el cont)
  (funcall cont (atom el)))
(defun remove& (list el cont)
  (null& list (lambda (c1)
                (if c1 (funcall cont nil)
                  (car& list (lambda (c2)
                               (cdr& list (lambda (c3)
                                            (eql& c2 el (lambda (c4)
                                                          (if c4 
                                                            (remove& c3 el cont)
                                                            (remove& c3 el (lambda (c5)
                                                                             (cons& c2 c5 cont))))))))))))))

;proud 0 a 1
;gentos
(defun st01 ()
  (cons-stream 0
               (cons-stream 1 (st01))))
(defun gentos (gen)
  (let ((x (next gen)))
    (when x
    (cons-stream x (gentos gen)))))
(defmacro lazypoint (x y)
  `((delay x) (delay y)))
(defun lazy-x (list)
  (force first list))
(defun lazy-y (list)
  (force second list))
(defun gentostream (gen)
  (let ((x (next gen)))
    (when x (cons-stream x (gentostream gen)))))
(defun sttogen (stream)
  (lambda ()
(let ((n (stream-car stream)))
    (when n (prog1 n
              (setf stream (stream-cdr stream)))))))
(defun streamtogen(stream)
  (lambda ()
    (let ((x (stream-car stream)))
      (when x 
        (prog1 x
          (setf stream (stream-cdr stream)))))))
(defun gentostream (gen)
  (let ((x (next gen)))
    (when x 
      (cons-stream x (gentostream gen)))))
(defmacro awhen (test &body body)
  
    `(let ((it ,test))
       (when it (progn ,@body)
         )))
(defmacro set-q2 (num a b)
  (let ((n (gensym)))
    `(let ((,n ,num))
       (setf ,a ,num)
       (setf ,b (+ ,num 1)))))
;______________________________
;cond:
(defmacro cond* (&rest branches);tenhle cond bere rovnou branches
      `(progn ,@(mapcar (lambda (branch) `(when ,(car branch) ,@(cdr branch))) branches)))
(defmacro mycond* (bindings);tenhle bere seznam jako normalni cond
  (when bindings
    `(progn ,@(mapcar (lambda (branch) `(when ,(car branch) ,@(cdr branch))) bindings))))

;nilf
(defmacro nilf (&rest args)
  (when args
    `(progn (setf ,(car args) nil)
       (nilf ,@(cdr args)))))
(defmacro nilf2 (&rest args)
    `(progn ,@(mapcar (lambda (arg) `(setf ,arg nil)) args)))

;my-case
(defmacro my-case (num &rest branches)
  (when branches
    (let ((n (gensym)))
      `(let ((,n ,num))
         (if (eql ,n ,(caar branches))
             (progn ,@(cdar branches))
           (my-case ,n ,@(cdr branches)))))))

(defmacro my-case2 (num &rest branches)
  (let ((n (gensym)))
    `(let ((,n ,num))
       (progn ,@(mapcar (lambda (branch) `(when (eql ,(car branch) ,n) (progn ,@(cdr branch)))) branches))))) 

    
;in-intervals
(defmacro in-intervals (number &rest intervals)
  (when intervals
    (let ((num (gensym)))
      `(let ((,num ,number))
         (if (and (>= ,num ,(car intervals))
                  (<= ,num ,(cadr intervals)))
             t
           (in-intervals ,num ,@(cddr intervals)))))))

;defun s maxem
(defmacro defunmax (name λ &body body)
  (let ((max (gensym))
        (value (gensym)))
    `(let ((,max nil))
       (defun ,name ,λ
         (let ((,value (progn ,@body)))
           (if (or (null ,max) (> ,value ,max)) (setf ,max ,value) ,value)
           ,value))
       (defun ,(symbol (stra name "-max")) () ,max))))

;defun s countem
(defmacro defuncount (name λ &body body)
  (let ((count (gensym)))
      `(let ((,count 0))
         (defun ,name ,λ
           (incf ,count)
           (progn ,@body))
         (defun ,(symbol (stra name "-count")) nil ,count))))

;defun s memoizaci
(defmacro meindefmemfun (name λ &body body)
  (let ((remember (gensym)) (found (gensym))(new (gensym)))
   ` (let ((,remember nil))
       (defun ,name ,λ
         (let ((,found (find ,(car λ) ,remember :key #'car)))
           (if ,found (cdr ,found);tady musi byt ,found a ne ,(cdr found)!
             (let ((,new (progn ,@body)))
               (push (cons ,(car λ) ,new) ,remember)
               ,new)))))))

(defmacro prog1-8 (form1 &body body)
  `(let ((result nil)
         (fun (lambda () (progn ,@body))))
     (setf result ,form1)
     (funcall fun)
     result))
(let ((result 10))
  (prog1-8 (+ result 5) (print "Ahoj")))
;zabrani symbolu, Error in function +: Elements of (NIL 5) are not NUMBERs. 
(defmacro case3 (num minus zero plus)
  (let ((n (gensym)))
    `(let ((,n ,num))
       (if (minusp ,n) ,minus
         (if (zerop ,n) ,zero
           ,plus)))))
(defmacro while (test &body body)
  `(do-while (lambda () ,test) (lambda () ,@body)))
(defun do-while (test-fun bd-fun)
  (when (funcall test-fun) 
    (funcall bd-fun)
    (do-while test-fun bd-fun)))
(defmacro allset (num &rest places)
  (when places
    (let ((n (gensym)))
      `(let ((,n ,num))
          (setf ,(car places) ,n)
               (allset ,n ,@(cdr places))))))
(defmacro swap2 (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (setf ,a ,b)
       (setf ,b ,tmp))))
(defmacro defunlist (name llist &body body)
  (let ((results (gensym))
        (res (gensym)))
    `(let ((,results nil))
       (defun ,name ,llist
         (let ((,res (progn ,@body)))
           (push ,res ,results)
           ,res))
       (defun ,(symbol (stra name "-list")) nil ,results))))
(defmacro casen (num &body body)
  (when body
  (let ((n (gensym)))
    `(let ((,n ,num))
       (if (zerop ,n)
           ,(car body)
         (casen (1- ,n) ,@cdr body))))))
(defun tst (a)
  (setf a 2))
(let ((val nil))
  (defun val (&optional (x nil))
    (if x (progn (setf val x)
            val)
      val)))
(defun streamtgen (stream)
  (lambda ()
    (let ((x (stream-car stream)))
   ( when x 
    (prog1 x
      (setf stream (stream-cdr stream)))))))
(defun gentostream (gen)
  (let ((x (next gen)))
    (when x
  (cons-stream x (gentostream gen)))))
(defun fibgen ()
  (let ((f 0)
        (s 1)
        (tmp nil))
    (lambda ()
      (prog1 f
        (setf tmp f)
        (setf f s)
        (setf s (+ tmp s))))))
(defun factgen ()
  (let ((n 0)
        (acc 1))
(lambda ()
  (prog1 acc 
             (setf n (1+ n))
             (setf acc (* n acc))))))
(defun gentos (gen)
  (let ((x (next gen)))
    (when x
      (cons-stream x (gentos gen)))))
(defun stg (stream)
  (lambda ()
    (let ((x (stream-car)))
      (when x
        (prog1 x
          (setf stream (stream-cdr stream)))))))
(defun createcirclist (&rest nums)
  (let ((c (copy-list nums)))
    (setf (cdr (last c)) c)))
(defun uncircle (circle)
  (let ((visited nil))
    (labels ((iter (node)
               (cond ((null node) nil)
                     ((find node visited) nil)
                     (t (progn

                          (push node visited)
                          (cons (car node) (iter (cdr node))))))))
      (iter circle)))) 
(let ((s nil))
  (setf s (cons-stream 1 (cons-stream 0 s))))
(defun copy-l (list)
  (let ((vis nil))
    (labels ((iter (node)
               (cond ((null node) nil)
                     ((find node vis) node)
                     ((atom node) node)
                     (t (progn (push node vis)
                          (cons (iter (car node))
                                (iter (cdr node))))))))
      (iter list))))
               
(defun circfind (circle el)
  (let ((visited nil))
    (labels ((iter (node)
               (cond ((null node) nil)
                     ((find node visited) nil)
                     ((atom node)
                      (if (eql node el) node nil))
                     (t (progn (push node visited)
                          (or (iter (car node))
                              (iter (cdr node))))))))
      (iter circle))))
(defun circlep (list)
  (labels ((iter (slow fast)
             (cond ((null fast) nil)
                   ((eql slow fast) t)
                   (t (iter (cdr slow) (cddr fast))))))
    (and (not (null list))
         (not (null (cdr list)))
         (iter (cdr list) (cddr list)))))
(defun countatoms(s)
  (let ((vis nil))
    (labels ((iter (node)
               (cond ((null node) 0)
                     ((find node vis) 0)
                     ((atom node) 1)
                     (t (+ 1 (iter (car node)) (iter (cdr node)))))))
      (iter s))))
(defun struct-find-if(s pred)
  (let ((vis nil))
    (labels ((iter (node)
               (cond ((null node) nil)
                     ((find node vis) nil)
                     ((atom node)
                      (if (funcall pred node) node nil))
                     (t (or (iter (car node))
                            (iter (cdr node)))))))
      (iter s))))
(defun countcons (s)
  (let ((vis nil))
    (labels ((iter (node)
               (cond ((null node) 0)
                     ((atom node) 0)
                     ((find node vis) 0)
                     (t (+ 1 (iter (car node)) (iter (cdr node)))))))
      (iter s))))
                     
(defun structapp (s)
  (let ((vis nil))
    (labels ((iter (node)
               (cond ((null s) nil)
                     ((find node vis) nil)
                     ((atom node) (list node))
                     (t (progn (push node vis))
                        (append (iter (car node))
                                (iter (cdr node)))))))
      (iter s))))

(defmacro cond* (&rest branches)
  (when branches
    `(if ,(caar branches)
         (progn ,(cadar branches)
           (cond* ,@(cdr branches)))
       (cond* ,@(cdr branches)))))
}defun structatoms (s]
(let ((vis nil))
  (lables ((iter (node)
                 ((atom node) (list atom))
                 ((find node visited) nil)
                 (t (progn (push (node vis)
                                 (append (iyer (car node))
                                       (iter (cdr node))
                                             ))))))
          (iter s)))
       
