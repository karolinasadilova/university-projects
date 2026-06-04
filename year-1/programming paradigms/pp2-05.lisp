;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 05_exercises.lisp -- řešení úloh z cvičení 5
;;;
(let ((count 0))
  (defun counting-fib (n)
  (setf count (1+ count))
  (if (<= n 1) 1
    (+ (counting-fib (1- n))
       (counting-fib (- n 2)))))
    

(defun fib-count ()
  count))

(defmacro defcfun (function l-list &body body)
  (let ((count (gensym "count")))
    `(let ((,count 0))
       (defun ,function ,l-list  
         (setf ,count (1+ ,count))
         ,@body)
       (defun ,(symbol (stra function "-cc")) ()
         ,count))))

(defun 0and1 ()
  (cons-stream 0
               (cons-stream 1
                            (0and1))))
      
;verze1:
(defun stream-ref (stream index)
  (cond ((null stream) nil)
        ((zerop index) (stream-car stream))
        (t (stream-ref (stream-cdr stream) (1- index)))))

;verze2:
(defun stream-ref (stream index)
  (when stream
        (if (zerop index)
            (stream-car stream)
          (stream-ref (stream-cdr stream) (1- index)))))


(defun stream-heads (stream)
  (let ((car (stream-car stream)))   
    (labels ((iter (car acc stream)
              (when stream
                (setf acc (+ acc car))
                (cons-stream acc (iter (stream-car (stream-cdr stream)) acc (stream-cdr stream))))))
      (iter car 0 stream))))

(defun stream-append2 (stream1 stream2)
  (if (null stream1) 
      stream2
    (cons-stream (stream-car stream1)
                    (stream-append2 (stream-cdr stream1) stream2))))


#|
(defun stream-append (stream &rest streams)
  (foldr #'stream-append2 streams stream))
|#


(defun stream-append-1 (stream &rest streams)
  (if (null streams) stream
    (stream-append2 stream 
                    (apply #'stream-append-1 streams))))
                      

(defmacro stream-append (stream &rest streams)
  `(if (null ,streams) ,stream
     (stream-append2 ,stream
                     (stream-append ,@streams))))

(defun prime-twins ()
  (labels ((iter (stream)
             (let ((car (stream-car stream))
                   (cdr (stream-cdr stream)))
               (cond ((= (- (car cdr) car) 2) 
                      (cons-stream (cons car (car cdr)) (iter cdr)))
                     (t (iter cdr))))))
    (iter (eratosthenes))))
(defun power3 (element)
  (* element element element))

(defun newstream (stream)
  (let ((stream1 (stream-each-other stream)))
    (let ((stream2 (stream-mapcar-1 #'power3 stream1)))
      (stream-heads stream2))))

(defun naturals-frm-1 ()
  (stream-cdr (naturals)))
(defun fib-list (n)
  (labels ((iter  (stream index)
             (let ((current (stream-car stream)))
             (if (= n index) nil
                   (cons current (iter (stream-cdr stream) (1+ index)))))))
    (iter (fib2) 0)))

(defun fib-gen ()
  (let ((stream (fib2)))
    (lambda () 
      (let ((current (stream-car stream)))
        (setf stream (stream-cdr stream))
        current))))


#|(defun fib2 () 
  (let ((result (cons-stream
                 0
                 (cons-stream
                  1
                  (stream-mapcar-2 #'+
                                   16
                                   result
                                   (stream-cdr result))))))))

;syntakticky špatně:chybí tělo letu
;se setf to funguje protože si let vytvoří result 
;resultu pak přiřadí stream který už existuje
;v letu se nejřív vyhodnotí všechno napravo a pak se to naváže nalevo ale v ten moment ještě result neexistuje 

|#
(defun next-row-for-pascal (row)
  (labels ((iter (row)
             (if (null (cdr row)) (cons 1 nil)
                   (cons (+ (car row) (car (cdr row)))
                                (iter (cdr row))))))
  (cond ((null row) nil)
        (t (cons 1 (iter row))))))

(defun pascal-stream ()
  (labels ((iter (row)
             (cons-stream row
                          (iter (next-row-for-pascal row)))))
    (iter (cons-stream 1 nil))))



(defun pascal-stream nil
  (let ((result nil))
    (setf result (cons-stream (list 1)
                              (stream-mapcar-1 (lambda (row)
                                                 (mapcar #'+ (append row (list 0))
                                                         (cons 0 row)))
                                               result)))))
               
(defun prefix-to-infix (list)
  (cond ((atom list) list)
        ((null (cddr list)) list)
        (t (let ((op (car list))
                 (args (cdr list)))
             (apply #'append (cons (list (car args))
                                 (mapcar (lambda (arg) 
                                           (list op arg))
                                         (cdr args))))))))



(let ((count 0))
  (defun counting-fib (n)
    (setf count (1+ count))
    (cond ((zerop n) 0)
          ((= n 1) 1)
          (t (+ (counting-fib (1- n))
             (counting-fib (- n 2))))))
  (defun fib-count ()
    count)
  (defun resetfib ()
    (setf count 0)))
(defmacro defcfun (name llist &body body)
  (let ((count (gensym)))
    `(let ((,count 0))
       (defun ,name ,llist 
         (incf ,count)
         ,@body)
       (defun ,(symbol (stra name "-cc")) nil
              ,count))))

(let ((res nil))
 (setf res (cons-stream 0 (cons-stream 1 res)))
 res)
(defun 0a1 ()
  (cons-stream 0 (cons-stream 1 (0a1))))
(defun str-ref (s i)
  (when s
    (if (zerop i) (stream-car s)
      (str-ref (stream-cdr s) (1- i)))))
(defun stream-each-other (stream)
  (when stream
    (let ((car (stream-car stream)))
      (cons-stream car (let ((cdr (stream-cdr stream)))
                         (when cdr
                           (stream-each-other
                            (stream-cdr cdr))))))))
(defun stream-each-other2 (stream)
  (when stream
    (let ((car (stream-car stream))
          (cdr (stream-cdr stream)))
      (cons-stream
       car
       (when cdr (stream-each-other2 (stream-cdr cdr)))))))
;;tohle vyhodnocuje hned i cdr a to je blbe testovat se to ma s vedlejsim efektem tjj tisk

(defun stream-heads2 (s)
      (labels ((it (acc s )
                 (when s
                   (let ((new (+ acc (stream-car s))))
                   (cons-stream new
                                (it new (stream-cdr s)))))))
        (it 0 s)))
(defun stream-append2 (s1 s2)
  (if (null s1) s2
        (cons-stream (stream-car s1)
                      (stream-append2 (stream-cdr s1) s2))))
(defun stream-app (&rest s)
  (when s (stream-append2 (car s) (apply #'stream-app (cdr s)))))
(defmacro streamapp (&rest s)
  (cond ((null s) nil)
        ((null (cdr s)) (car s))
        (t `(stream-append2 ,(car s)
                         (streamapp ,@(cdr s))))))
(defun primtwins()
  (labels ((iter (s)
             (let ((car (stream-car s))
                   (cdr (stream-cdr s)))
               (if (= 2 (- car (car cdr)))
                      (stream-cons (cons car (car cdr))
                                   (iter cdr))
                      (iter cdr)))))
    (iter (eratosthenes))))
(defun perfectstream (s)
  (stream-heads2 (stream-mapcar-1 #'power3 (stream-each-other s))))
(defun power3 (n)
  (* n n n))

(defun stream-to-gen (stream)
  (lambda ()
  (when stream
    (prog1 (stream-car stream)
      (setf stream (stream-cdr stream))))))
(defun gen-to-stream (gen)
  (let ((x (next gen))) 
    (when x
      (cons-stream (x)
                 (gen-to-stream gen)))))

(defun stream-to-gen (stream)
  (lambda ()
    (when stream
      (prog1 (stream-car stream)
        (setf stream (stream-cdr stream))))))
(defun gen-to-stream (gen)
  (let ((x (next gen)))
    (when x
      (cons-stream x gen))))
(defun stg (stream)
  (lambda ()
    (when stream
    (prog1 (stream-car stream)
    (setf stread (stream-cdr))))))

           
(defun uncircle (list)
  (let ((visited nil))
    (labels ((iter (node)
               (cond ((find node visited) nil)
                     ((null node) nil)
                     ((atom node) node)((null node) nil)
                     (t (push node visited)
                        (cons (iter (car node))
                              (iter (cdr node)))))))
      (iter list))))
(defun str-cpy (s)
  (let ((visited nil))
    (labels ((cp (node)
               (cond ((null node) nil)
                     ((atom node) (list node))
                     ((find node visited) node)
                     (t (cons (cp (car node))
                                (cp (cdr node)))))))
      (cp s)
                              
)))
(defun gm (fun gen)
  (lambda ()(funcall fun (next gen))))
(defmacro allc (bindings &body body)
  `(progn
     ,@(mapcar (lambda (branch) `(when ,(car branch) ,@(cdr branch))) body)))
(defmacro nilf (&rest args)
  (when args
    `(progn (setf ,(car args) nil)
       (nilf ,@(cdr args)))))
(defmacro my-case (el &body body)
  (when body
  (let ((ele (gensym)))
    `(let ((,ele el))
       (if (eql ,ele ,(caar body)) (progn ,@(cdr (car body))
         (my-case ,ele ,@(cdr body))))))))
(defmacro in-intervals (n &rest intervals)
  (when intervals
    (let ((num (gensym)))
      `(let ((,num ,n))
         (if (and (>= ,num ,(car intervals))
                  (<= ,num ,(cadr intervals)))
                  t
                  (in-intervals ,num ,@(cddr intervals)))))))
(defmacro mdefun (name llist &body body)
  (let ((m (gensym))
        (max (gensym)))
    `(let ((,max nil))
       (defun ,name ,llist
         (let ((,m (progn ,@body)))
           (if (or (null ,max)(> ,m ,max)) (setf ,max ,m)
                 ,m)
         ,m))
       (defun ,(symbol (stra name "-max")) ()
         ,max))))
               
(defmacro defcount (name llist &body body)
  (let ((count (gensym)))
    `(let ((,count 0))
       (defun ,name ,llist
         (incf ,count)
         (progn ,@body))
       (defun ,(symbol (stra name "-count")) ()
         ,count))))

               
(defmacro defmemf (name llist &body body)
  (let ((remember (gensym))
        (new (gensym))
        (found (gensym)))
    `(let ((,remember nil))
       (defun ,name ,llist
         (let ((,found (find ,(car llist) ,remember :key #'car)))
           (if ,found (cdr ,found)
             (let ((,new (progn ,@body)))
               (progn (push (cons ,(car llist) ,new) ,remember)
                 ,new))))))))
(defun my-case3 (num plus zero minus)
  (let ((n gensym))
    `(let ((,n ,num))
       (if (zerop ,n) ,zero
         (if (plusp ,n) ,plus
           ,minus)))))
             
(defmacro while (test &body body)
  `(dowhile (lambda () ,test) (lambda () ,@body)))
(defun dowhile (cond-fun bd-fun)
  (when (funcall cond-fun)
      (progn (funcall bd-fun)
        (dowhile cond-fun bd-fun))))
(defmacro mm (x y)
  `(print ,x)
  `,y)
(defun x (d e)
  (mm d e))
(defmacro mysetf (num &rest args)
  ;(when args
    (let ((n (gensym)))
      `(let ((,n ,num))
          ,@(mapcar (lambda (a) `(setf ,a ,n)) args))))
           
(defmacro mysetf2 (num &rest args)
(when args
  (let ((n (gensym)))
   ` (let ((,n ,num))
       (setf ,(car args) ,n)
         (mysetf2 ,n ,@(cdr args))))))
(defmacro sw (a b)
  (let ((tmp (gensym)))
    `(let ((,tmp ,a))
       (setf ,a ,b)
       (setf ,b ,tmp))))
(defmacro mdefun (name llist &body body)
  (let ((res (gensym))
        (new (gensym)))
    `(let ((,res nil))
       (defun ,name ,llist
         (let ((,new (progn ,@body)))
           (push ,new ,res)
           ,new))
       (defun ,(symbol (stra name "-resuls")) 
              nil
         ,res))))
(defmacro casen (n &rest branches)
  (when branches
    (let ((num (gensym)))
      `(let ((,num ,n))
         (if (zerop ,num) ,(car branches)
           (casen (1- ,num) ,@(cdr branches)))))))
(defun ts (a)
  (setf a 2))
(let ((remember nil))
  (defun val (&optional (x nil))
    (if x
        (setf remember x)
      remember)
    remember))
(defmacro mac1 (x)
  `(print ,x)
  `(1+ ,x))
(defmacro mac2 (x)
  `(progn ,(print x)
     (+ ,x 1)))
(defun fbgen()
  (let ((f 0)
        (s 1)
        (tmp nil))
  (lambda ()
    (prog1 f
     (setf tmp f)
     (setf f s)
     (setf s (+ s tmp))))))
(defun strtgen (stream)
    (lambda ()
      (when stream 
      (prog1 (stream-car stream)
        (setf stream (stream-cdr stream)))))))
(defun fctgen ()
(let ((curr 1)
  (n 0))
  (lambda ()
    (cond ((or (= n 1) (zerop n)) (progn (setf n (1+ n)) curr))
         (t  (progn (setf curr (* curr n))(setf n (1+ n)) curr))))))
      
(defun circlist(&rest args)
  (let ((cp (copy-list args)))
    (setf (cdr (last cp)) cp)))
(defun uncircle(circle)
  (let ((visited nil))
    (labels ((iter (node)
               (cond ((find node visited) nil)
                     ((atom node) node)
                     (t (progn (push node visited)
                          (cons (iter (car node))
                                (iter (cdr node))))))))
      (iter circle))))

(defun cp-list (list)
  (let ((visited nil))
    (labels ((cp (node)
               (let ((n (find node visited :key #'car)))
               (cond ((null node) nil)
                     ((atom node) node)
                     (n (cdr n))
                     (t (let ((new (cons nil nil)))
                          (push (cons node new) visited)
                          (setf (car new) (cp (car node)))
                          (setf (cdr new) (cp (cdr node)))
                          new))))))
      (cp list))))
(defun circfind (s el)
  (let ((visited nil))
    (labels ((iter (node)
               (cond ((null node) nil)
                     ((find node visited) nil)
                     ((atom node)
                      (if (eql node el) node
                        nil))
                     
                     (t (progn (push node visited)
                          (or (iter (car node))
                              (iter (cdr node))))))))
      (iter s))))
(defun circlep3 (s)
  (labels ((cp (slow fast)
             (cond ((null fast) nil)
                   ((eql slow fast) t)
                   (t (cp (cdr slow) (cddr fast))))))
    (and (not (null s))
         (not (null (cdr s)))
      (cp (cdr s) (cddr s) ))))
(defun structsum (s)
  (let ((visited nil))
    (labels ((suma (node)
               (cond ((null node) 0)
                     ((find node visited) 0)
                     ((atom node) (if (numberp node) node 0))
                     (t (progn (push node visited)
                                     (+ (suma (car node))
                                        (suma (cdr node))))))))
      (suma s))))
(defun countatoms (s)
  (let ((vis nil))
    (labels ((c (node)
               (cond ((null node) 0)
                     ((find node vis) 0)
                     ((atom node) 1)
                     (t (progn (push node vis)
                          (+ (c (car node))
                             (c (cdr node))))))))
      (c s))))
(defun strfindif (s pred)
  (let ((visited nil))
    (labels ((f (node)
               (cond ((null node) nil)
                     ((find node visited) nil)
                     ((atom node)
                      (if (funcall pred node) node nil))
                     (t (progn (push node visited)
                          (or (f (car node))
                              (f (cdr node))))))))
      (f s))))

(defun strcountcons (s)
  (let ((visited nil))
    (labels ((iter (node)
               (cond ((null node) 0)
                     ((find node visited) 0)
                     ((atom node) 0)
                     (t (progn (push node visited)
                          (+ 1 (iter (car node))
                              (iter (cdr node))))))))
      (iter s))))

(defun strapp (s)
  (let ((visited nil))
    (labels ((iter (node)
               (cond ((null node) nil)
                     ((find node visited) nil)
                     ((atom node) (list node))
                     (t (progn (push node visited)
                          (append (iter (car node))
                                  (iter (cdr node))))))))
      (iter s))))
    
(defun twotimesnode (s)
  (let ((visited nil))
    (labels ((iter (node)
               (cond ((null node) nil)
                     ((find node visited) node)
                     ((atom node) nil)
                     (t (progn (push node visited)
                          (or (iter (car node))
                              (iter (cdr node))))))))
      (iter s))))
(defun stcpy (s)
  (let ((pairs nil))
    (labels ((iter (node)
              
                     (cond ((null node) nil)
                           ((atom node) node)
                           (t (let ((found (find node pairs :key #'car)))
                                (if found (cdr found)
                                  (let ((new (cons nil nil)))
                                    (push (cons node new) pairs)
                                    (setf (car new) (iter (car node)))
                                    (setf (cdr new) (iter (cdr node)))
                                    new))))))))
      (iter s))))
