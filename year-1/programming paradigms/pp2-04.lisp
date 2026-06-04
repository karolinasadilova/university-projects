;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 04_exercises.lisp -- řešení úloh z cvičení 4
;;;
;;priklady z konzultace:
(defun power-set (s n p)
  (if (>= n (length s)) 
      (list p)
    (append (power-set s (1+ n) (append p (list (nth n s))))
            (power-set s (1+ n) p))))
(defun power-set2 (set)
  (if (null set) (list nil)
    (let ((subsets (power-set2 (cdr set))))
      (append subsets
              (add-element (car set) subsets)))))

(defun add-element (element list)
  (if (null list) nil
    (cons (cons element (car list))
          (add-element element (cdr list)))))
;př notes:

(defmacro my-setf-2 (place value)
  `(funcall (place-accessors ,place) ,value))

(defmacro my-setf (place value &rest rest-of-places-and-values)
  (if (null rest-of-places-and-values) 
      `(my-setf-2 ,place ,value)
    `(progn (my-setf-2 ,place ,value)
       (my-setf ,@rest-of-places-and-values))))


;priklady ze cvičení:
;1:
;(let ((x 1))
;(let ((x x))
;(setf x 2)
;(print x))
;x)


;2
;1


;
;x se vrací z prvního prostředí , printuje v prostředí letu

(defmacro swap (place1 place2)
  (let ((p1 (gensym "place1"))
        (p2 (gensym "place2"))
        (tmp (gensym "tmp")))
    `(let ((,p1 (place-accessors ,place1))
           (,p2 (place-accessors ,place2)))
       (bind-list (sel1 mut1) ,p1
       (bind-list (sel2 mut2) ,p2
       (let ((,tmp (funcall sel2)))
         (funcall mut1 (funcall sel2))
         (funcall mut2 (funcall ,tmp))))))))


;;swap
(defmacro swap (place1 place2)
  `(bind-list (sel1 mut1) (place-accessors ,place1)
              (bind-list (sel2 mut2) (place-accessors ,place2)
              (let ((tmp (funcall sel1)))
                (funcall mut1 (funcall sel2))
                (funcall mut2 tmp)))))
(defmacro swap1 (pl1 pl2)
  (let ((tmp (gensym)))
    `(bind-list (sel1 mut1) (place-accessors ,pl1)
                (bind-list (sel2 mut2) (place-accessors ,pl2)
                           (let ((,tmp (funcall sel1)))
                             (funcall mut1 (funcall sel2))
                             (funcall mut2 ,tmp))))))
    
                    
                         
  






;3:
;(defmacro swap (a b)
;  (let ((place-sym (gensym "place")))
;    `(let* ((,place-sym ,a))
;       (setf ,a ,b
;             ,b ,place-sym))))

;bude místa vyhodnocovat několikrát např. dlouhý seznam a poslední prvek


(let ((x 0))
  (defun f ()
    (lambda () (incf x))))

(defun test1 (a)
  (setf a 2))
(defun test2 (a)
  (test1 a)
  a)

(defun test3 (a)
  (setf (car a) 2))
(defun test4 (a)
  (test3 a)
  a)
(defun last-el(list)
  (labels ((iter (list)
             (cond ((null list) nil)
                   ((null (cdr list)) (car list))
                  (t (iter (cdr list))))))
    (iter list)))

(defmacro last-el-accessors (listexpr)
  (let ((val (gensym)))
    `(let ((,val (last-element ,listexpr)))
       (list (lambda () (car ,val))
             (lambda (val2) (setf (car ,val) val2))))))


(defun last-element (list)
  (cond ((null list) nil)
        ((null (cdr list)) list)
        (t (last-element (cdr list)))))


(defun pw2even ()
  (let ((n 0))
    (gen-map #'power2 (gen-filter #'evenp (lambda () (setf n (1+ n)))))))
(defun pw2oddp ()
  (gen-map #'power2 (gen-filter #'oddp (gen-naturals))))
(defun each2 (gen)
  (lambda ()
    (let ((n (next gen)))
      (setf n (next gen))
      n)))
(defun gensec (gen)
  (lambda ()
    (next gen)
    (next gen)))
(defmacro last-element-accessors (list-expr)
  (let ((last (gensym "last")))
    (let ((l (gensym "list")))
   `(let ((,l ,list-expr))
      (let ((,last (last-element ,l)))
        (list (lambda () (car ,last))
              (lambda (value) (set-car ,last value) value)))))))

(defun gen-oddp-squares ()
  (gen-map #'power2 (gen-filter #'oddp (gen-naturals))))


(defun gen-second (gen)
  (lambda ()
    (let ((value (next gen)))
      (next gen)
      value)))

(defun gen+ (gen1 gen2)
  (lambda ()
    (+ (next gen1) (next gen2))))

(defun circlist (&rest args)
    (let ((newlist (copy-list args)))
      (if (null newlist) nil
        (progn (set-cdr (last-element newlist) newlist)
          newlist))))


; (let ((pair (cons 1 nil)))
;  (set-cdr pair pair) pair)
;#1=(1 . #1#)


(defun circ-find (element list)
  (cond ((null list) nil)
        ((eql (car list) element) (car list))
        ((not (circlep list)) (find element list))
        (t (labels ((iter (slow fast)
                      (cond ((eql element (car slow)) (car slow))
                            ((eql slow fast) nil)
                            (t (iter (cdr slow) (cddr fast))))))
             (iter (cdr list) (cddr list))))))

(defun struct-find (element struct)
    (let ((visited '()))
      (labels ((iter (node)
                 (cond ((atomp node) (if (eql node element) node nil))
                       ((find node visited) nil )
                       (t (progn (push node visited) (or (iter (car node)) (iter (cdr node))))))))
        (iter struct))))

(defun struct-sum (struct)
  (let ((visited '()))
    (labels((iter  (node)
            (cond ((atomp node) (if (numberp node) node 0))
                  ((not (find node  visited)) (progn (push node visited) (+ (iter (car node)) (iter (cdr node)))))
                  (t 0))))
      (iter struct))))
                       
(defun circle-struct-p (struct)
  (let ((visited '()))
    (labels ((iter (node)
               (cond ((atomp node) nil)
                     ((find node visited) t)
                     (t (progn (push node visited)
                          (or (iter (car node)) (iter (cdr node))))))))
      (iter struct))))


(defun struct-copy (struct)
  (let ((visited '()))
    (labels ((iter (node)
               (cond ((find node struct) node)
                     ((atomp node) node)
                     (t (progn (push node visited)
                          (cons (iter (car node))
                                (iter (cdr node))))))))
      (iter struct))))
(defun circle-list(&rest nums)
  (let ((ls (copy-list nums)))
    (setf (cdr (last-element ls)) ls)))
(defun circfind (struct el)
  (let ((visited nil))
           (labels ((iter (node)
                      (cond ((find node visited) nil)
                            ((atom node) (if (eql node el) node nil))
                            (t(progn (push node visited)
                                (or (iter (car node))
                                    (iter (cdr node))))))))
             (iter struct))))
(defun structfind (el struct)
  (cond ((null struct) nil)
        ((atom struct) (if (eql struct el) struct nil))
        ((consp struct)
         (let ((visited nil))
           (labels ((fn (node)
                      (cond ((find node visited) nil)
                            ((atom node) (if (eql node el) node nil))
                            (t (progn (push node visited)
                                 (or (fn (car node))
                                     (fn (cdr node))))))))
             (fn struct))))))
(defun structsum (struct)
  (let ((visited nil)
        (acc 0))
    (labels ((f (node )
               (cond ((null node) acc)
                     ((find node visited) acc)
                     ((numberp node) (setf acc (+ acc node)))
                     ((atom node)   acc)
               (t (progn (push node visited)
                     (f (car node))
                     (f (cdr node)))))))
      (f struct)
      acc)))
         
(defun structsum2 (s)
  (let ((vis nil))
    (labels ((fn (node)
               (cond ((find node vis) 0)
                     ((atom node) (if (numberp node) node 0))
                    ((consp node) (progn (push node vis)(+ (fn (car node)) (fn (cdr node))))))))
      (fn s))))

        
(defun circlesp (s)
  (let ((visited nil))
    (labels ((iter (node )
               (cond ((find node visited) t)
                     ((atom node) nil)
                     (t (progn (push node visited)
                          (or (iter (car node)) 
                              (iter (cdr node))))))))
      (iter s))))
(defun struct-cp (s)
  (let ((visited nil))
    (labels ((iter (node)
               (cond ((find node visited) nil)
                     ((atom node) node)
                     (t
                      (cons (iter (car node)) (iter (cdr node)))))))
      (iter s))))
        
                                         
   ;______________________________makra:
(defmacro nilf (&rest args)
  `(progn ,@(mapcar (lambda (a) `(setf ,a nil)) args)))
    
           
(defmacro mynilf (&rest args)
  `(progn ,@(mapcar (lambda (a) `(setf ,a nil)) args)))

(defmacro allc (&rest bindings)
  `(progn ,@(mapcar (lambda (branch) `(when ,(car branch) ,@(cdr branch))) bindings)))
(defmacro allc2 (bindings)
  `(progn ,@(mapcar (lambda (br) `(when ,(car br) ,@(cdr br))) bindings)))
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
         (progn ,@body))
       (defun (symbol ',name "-cc")))))
