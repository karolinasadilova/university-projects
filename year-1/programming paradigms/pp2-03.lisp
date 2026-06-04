;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 03_exercises.lisp -- řešení úloh z cvičení 3
;;;

(defmacro test-number (number minus zero plus)
  `(if (minusp ,number)
       ,minus
     (if (zerop ,number)
         ,zero
       ,plus)))
(defmacro while (test &body body)
  `(do-while (lambda () ,test)
             (lambda () ,@body)))
(defun do-while (test-fun body-fun)
  (when (funcall test-fun)
    (funcall body-fun)
    (do-while test-fun body-fun)))
;;;špatně/problem zabrání symbolu:
(defmacro whenv1 (test &body body)
  `(let ((result ,test))
     (when result ,@body result)))

;dobře:
(defmacro whenv (test &body body)
  (let ((res-symbol (gensym)))
    `(let ((,res-symbol ,test))
       (when ,res-symbol
          ,@body
          ,res-symbol))))
;;ukoly:
;1.awhen
(defmacro awhen (condition &body body)
    `(let ((it ,condition))
       (when it
         ,@body
      )))
(defmacro awhen2 (condition &body body)
  `(awhen-help ,condition 
               (lambda () ,@body)))


(defun awhen-help (condition body-fun)
  (let ((it condition))
    (when it
      (funcall body-fun)
      it)))

(defmacro my-awhen (condition &body body)
    `(let ((it ,condition))
       (when it ,@body)))
;;2.setq-seq-2
(defmacro setq-seq-2 (expr sym1 sym2)
  (let ((value (gensym "value")))
    `(let ((,value ,expr))
       (setq ,sym1 ,value)
       (setq ,sym2 (+ 1 ,value))
       ,value)))
(defmacro setq-seq-2-2 (expr sym1 sym2)
  `(setq-seq-2-2-help ,expr ',sym1 ',sym2))
(defun setq-seq-2-2-help (val sym1 sym2)
  (setq sym1 val)
  (setq sym2 (+ val 1))
        val)
    
(defmacro set-seq-2-my (arg symbol1 symbol2)
  (let ((tmp (gensym "tmp")))
    `(let ((,tmp ,arg))
       (progn (setf ,symbol1 ,tmp)
         (setf ,symbol2 (1+ ,tmp))))))
        
;error-if-nil
(defmacro error-if-nil (expr)
  `(error-help ,expr ',expr))
(defun error-help (value expr)
  (if (eql value nil)
      (error "Error value of expression " expr " is NIL")
    value))


(defmacro my-error-if-nil (expr)
  (let ((value (gensym "value")))
    `(let ((,value ,expr))
       (if (null ,value)
           (error "Error expression: " ',expr "is NIL")
         ,value))))

;4.do-interval
(defmacro do-interval (binding &body body)
  `(do-interval-help ,(car (cdr binding))  ,(car (cdr (cdr binding))) (lambda (,(car binding)) ,@body)))

(defun do-interval-help (x upper body-fun)
  (if (> x upper)
      nil
    (progn 
      (funcall body-fun x)
      (do-interval-help (1+ x)  upper body-fun))))

(defmacro my-do-interval (binding &body body)
  `(my-do-interval-help ,(cadr binding)
                        ,(car (cdr (cdr binding)))
                        (lambda (,(car binding)) ,@body)))

(defun my-do-interval-help (current upper func)
  (if (> current upper) nil
    (progn (funcall func current)
      (my-do-interval-help (1+ current) upper func))))
;whenv:
(defun whenv-help (cond-val body-fun)
  (when cond-val
    (funcall body-fun)
    cond-val))
(defmacro whenv (test &body body)
  `(whenv-help ,test (lambda () ,@body)))

;whenv obsahuje test a telo, makro expanduje a zavola se expanzni funkce tj pomocna funkce whenv-help ktera dostane testovaci funkci a telo, pokud pojde test zavola pomoci funcall body-fun. jinak vrati cond-val coz bude nil v druhem pripade. 

(defmacro test-number (number minus zero plus)
  `(test-number-help ,number ,minus ,zero ,plus))
(defun test-number-help (number minus zero plus)
  (if (minusp number) minus
    (if (zerop number) zero
      plus)))
;(defmacro while (cond &body body)
;  `(when ,cond ,@body 
;     (while cond &body body)))
(defun test ()
  (let ((n 10))
    (while (> n 0)
      (print n)
      (setf n (- n 1)))))
;;while je napsané špatně, při expanzi se bude pořád expandovat a zacyklí se


(defmacro my-test-number (number minus zero plus)
  `(my-test-number-help ,number ,minus ,zero ,plus))
(defun my-test-number-help (number minus zero plus)
  (if (minusp number) minus
    (if (zerop number) zero
      plus)))
;7:
(defmacro prog1-1 (first &body body)
  `(let ((result ,first)
         (fun (lambda () ,@body)))
     (funcall fun)
     result))

(defmacro prog1-2 (first &body body)
  `(let ((result ,first)
         (bresult (progn ,@body)))
     result))

(defmacro prog1-3 (first &body body)
  `(let ((result ,first))
     ,@body
     result))

;(let ((result 100))
;  (let ((result 1))
;   (setq result 999)
;  result))

;by měl vratit 1:
;(let ((result 100))
; (prog1-3 1
;  (setq result 999)))
;999


;(let ((result 100))
;(prog1-3 1 2 3
;  (print result )))
;1
;1


;vnější result je 100 a result z makra je 1, správně by mělo makro vrátit 1 ale vrátí 999

(defmacro prog1 (first &body body)
  (let ((x (gensym "first")))
    `(let ((,x ,first))
       ,@body 
       ,x)))
(defmacro and (&rest forms)
  (cond 
   ((null forms) t)
   ((null (cdr forms)) (car forms))
   (t `(when ,(car forms) 
        (and ,@(cdr forms))))))

(defmacro my-prog1 (first &body body)
  (let ((tmp (gensym "tmp")))
   `(let ((,tmp ,first))
      ,@body
      ,tmp)))
;(defun and-helper (&optional first &rest forms)
;  (if (null first) 
;      (if (eql first nil) nil t)
;    (if first 
;        (if (null forms) first
 ;         (if (car forms) (apply and (cdr forms)))
  ;    nil))))
(defmacro my-and (&rest args)
  `(cond ((null',args) t)
       (,(car args) (my-and ,@(cdr args)))
       (t nil)))
          
          
         
(defmacro and-3 (&rest args)
 (if (null args) t
   (if (null (cdr args)) (car args)
    `(if ,(car args) (and-3 ,@(cdr args))
           nil))))
;;pri expanzi se vyhodnocuji vyrazy s carkou ale u (my-and) se vyhodnoti car nilu coz nejde a cdr nilu taky nejde takze pokud je args nil tak to vzdy spadne

;(and) vrací T"všechny výrazy jsou T"...nic to neporušuje
;(or) vrací NIL"stačí jeden T"...není nic co by bylo T
(defmacro my-or (&rest args)
  (if (null args) nil
    (if (null (cdr args)) (car args)
      `(if ,(car args) ,(car args)
        (my-or ,@(cdr args))))))

;;problem vicenasobneho vyhodnoceni

(defmacro my-or-correct (&rest args)
(let ((tmp (gensym "tmp")))
  (if (null args) nil
    (if (null (cdr args)) (car args)
      `(let ((,tmp ,(car args)))
         (if ,tmp ,tmp (my-or-correct ,@(cdr args))))))))
        
 
(defmacro or (&rest args)
  (cond ((null args) nil)
        ((null (cdr args)) (car args))
        (t (let ((s (gensym "or")))
            `(let ((,s ,(car args)))
                 (if ,s ,s (or ,@(cdr args))))))))

(defmacro or-not-full (&rest args)
  (cond ((null args) nil)
        ((null (cdr args)) `(if ,(car args) t nil))
        (t (let ((g (gensym)))
             `(let ((,g ,(car args))) 
                (if ,g t (or-not-full ,@(cdr args))))))))

(defmacro or-not-full-2 (&rest args)
  (cond ((null args) nil)
        ((null (cdr args)) `(if ,(car args) t nil))
        (t `(if ,(car args) 
                t 
              (or-not-full-2 ,@(cdr args))))))
;defun-doc:
;co ma delat:
;1. definovat funkci defunem
;2. vypsat info
;3. vratit funkci

(defmacro defun-doc (name lambda-list documentation &body body)
  `(progn (defun ,name ,lambda-list ,documentation ,@body)
     (print (list "name: " ',name))
     (print (list "lambda-list: " ',lambda-list))
     (print (list "documentation: " ',documentation))
     ',name))
(defmacro setq-seq (expr &rest symbols)
  (if (null symbols) expr
    (let ((value (gensym)))
      `(let ((,value ,expr))
         ,@(setq-seq-expander value symbols 0)
         ,value))))

            
(defun setq-seq-expander (value symbols i)
  (if (null symbols) nil
    (cons 
     `(setq ,(car symbols) (+ ,value ,i))
          (setq-seq-expander value (cdr symbols) (1+ i)))))
    
(defmacro my-setq-seq (expr &rest symbols)
(if (null symbols) nil
  (let ((value (gensym "value")))
    `(let ((,value ,expr))
      ,@(my-setq-seq-expander value symbols 0)
      ,value))))

(defun my-setq-seq-expander (value symbols index)
  (if (null symbols) nil
   (cons `(setf ,(car symbols) (+ ,index ,value))
     (my-setq-seq-expander value (cdr symbols) (1+ index)))))
;let*
;navazuje hodnotu na symbol
;;let: funcall (lambda (a) body) expr
;let* == několik vnořených letu

(defmacro let* (bindings &body body)
  (if (null bindings) `(progn ,@body)
    `(let (,(car bindings))
       (let* ,(cdr bindings) ,@body))))

;conds ((< 1 2) (print 1)) ((> 1 2) (print 2))
(defmacro all-cond (&rest conds)
  (if (null conds) nil
    `(progn (when ,(car (car conds)) ,@(cdr (car conds)))
      (all-cond ,@(cdr conds)))))


(defmacro all-cond-mapcar (&rest conds)
 `(progn
    ,@(mapcar (lambda (branch) 
                `(if ,(car branch) (progn ,@(cdr branch)) nil)) conds)))

(defmacro mylet* (bindings &body body)
  (if (null bindngs) `(progn ,@body)
    `(let ((,(car bindings)))
       (mylet* ,@(cdr bindings) ,@body))))

;;musi byt zabackquotovane progn body !!

(defmacro op-alias (old new)
  `(defmacro ,new (&rest args)
    (cons ',old args)))
  
(defmacro fc (&rest args)
  `(funcall ,(car args) ,@(cdr args)))

;(with-gensym (a 1) 'a)


;______________________________________________
(defmacro my-or-2 (a b)
  `(if ,a ,a ,b))

(defmacro my-unless (condition &rest body)
  `(if ,condition nil (progn ,@body)))

(defmacro my-if-zero (num branch1 branch2)
  `(if (zerop ,num) ,branch1 ,branch2))
(defmacro my-whenb (binding &rest body)
  (let ((symbol (car binding)))
    `(let ((,symbol ,@(cdr binding)))
        (when ,symbol ,@body))))
(defmacro test (num min zero plus)
  `(if (minusp ,num)
       ,min
     (if (zerop ,num)
         ,zero
       ,plus)))
(defmacro test-when (test &rest expressions)
(out "Expanding TEST-WHEN with test: "
(strs test)
" and expressions: "
(strs expressions))
`(if ,test (progn ,@expressions) nil))
(defun compilation-test ()
(test-when (< 0 1)'ano))

;awhen
(defmacro my-awhen (test &body body)
  `(let ((it ,test))
    (when it ,@body)))
;setq-seq-2
(defmacro my-setq-seq-2(a b c &body body)
  (let ((tmp (gensym)))
   `(let ((,tmp ,a))
      (setf ,b ,tmp)
      (setf ,c (1+ ,tmp))
      ,@body)))
;error if nil
(defmacro my-error-if-nil (expr)
  (let ((value (gensym)))
    `(let ((,value ,expr))
      (if ,value ,value (error "value of: " (strs ',expr) "is nil")))))

;do interval
(defun do-int-help (symbol lower upper body-fun)
  (let ((symbol lower))
    (while (< lower upper) (progn (funcall body-fun)
                          (do-int-help symbol (1+ lower) upper body fun)))))
(defmacro my-do-interval(binding &body body)
  `(do-int-help ,(car binding) ,(cadr binding) ,(car (cddr binding)) (lambda () ,@body)))


(defmacro myyy-whenv (test &body body)
  (let ((res-symbol (gensym)))
    `(let ((,res-symbol ,test))
       (when ,res-symbol
         ,@body
         ,res-symbol))))
;let vytvori unikatni symbol a navaze ho na res-symbol
;pro hpdnotu podminky..tu navaze na res-symbol
;let vyhodnoti res-symbol tj dosadi se unikatni symbol vytvoreny gensymem
(defmacro test-num (num min zero plus)
  (let ((tmp (gensym)))
    `(let ((,tmp ,num))
       (if (minusp ,tmp) ,min
         (if (zerop ,tmp) ,zero
           ,plus)))))
(defmacro test-num-fce (num min zero plus)
  `(test-num-fce-help ,num 
                      (lambda () ,min)
                      (lambda () ,zero)
                      (lambda () ,plus)))
(defun test-num-fce-help (num min-fun zero-fun plus-fun)
  (if (minusp num) (funcall min-fun)
    (if (zerop num) (funcall zero-fun)
      (funcall plus-fun))))
(defmacro prog1-1 (first &body body)
  `(let ((result ,first)
         (fun (lambda () ,@body)))
     (funcall fun)
     result))
;body je uvnitr lambdy
(let ((result (cadr '(1 10))))
  (prog1-1 (+ 1 3) (print (+ result 1))))
;print je uvnitr lambdy ona si pamtuje result kvuli lexikalnimu uzaveru
(let ((fun 100))
  (prog1-2 4 (print fun)))
(defmacro prog1-2 (first &body body)
  `(let ((result ,first)
         (bresult (progn ,@body)))
     result))
;result je mimo body zadane uzivatelem takze nedojde k poruseni
(defmacro prog1-3 (first &body body)
  `(let ((result ,first))
     ,@body
     result))
;body muze zmenit result 
(defmacro my-own-progn1 (first &body body)
  (let ((tmp (gensym)))
    `(let ((,tmp ,first))
       (progn ,@body ,tmp))))
(defmacro and (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(when ,(car args) (and ,@(cdr args))))))
(defmacro and2 (&rest args)
  (if args
      `(when ,(car args) (and2 ,@(cdr args)))
    t))
(defmacro my-and (&rest args)
  (if (null args) t
    (if (null (cdr args)) (car args)
      `(when ,(car args) (my-and ,@(cdr args))))))
(defmacro rebinding (x &body body)
  (let ((tmp (gensym)))
    `(let ((,tmp ,x))
    ,@body)))
   
(defmacro test-w2 (test &body body)
    (out "expand" (strs test) "bod" (strs body)) `(if ,test (progn ,@body) nil))
(test-w2 (> 1 0) (+ 1 1))
(test-w2 (> 1 0) (+ 1 1))
(defun pokus ()
  (test-w2 (> 1 0) (+ 1 1)))
(defmacro awhen3 (test &body body)
  `(let ((it ,test))
     (when it ,@body)))
(defmacro setq-seq-2 (arg s1 s2 &body body)
  (let ((val (gensym)))
    `(let ((,val ,arg))
       (setq ,s1 ,val)
       (setq ,s2 (1+ ,val))
       ,@body)))
(defmacro error-if-nil (exp)
  (let ((val (gensym)))
    `(let ((,val ,exp))
       (if ,val ,val (error "value: " (strs ',exp) "is nil")))))
(defmacro do-in-int (binding &body body)
  `(do-in-i-help ,(cadr binding) ,(car (cdr (cdr binding))) (lambda (,(car binding)) ,@body)))
(defun do-in-i-help (low upp fun)
  (if (> low upp) nil
    (progn (funcall fun low)
      (do-in-i-help (1+ low) upp fun))))
(defmacro progn1 (first &body body)
  (let ((x (gensym)))
    `(let ((,x ,first))
       (progn ,@body ,x))))
(defmacro do-interval (binding &body body)
  `(helper ,(cadr binding) ,(car (cdr (cdr binding))) (lambda (,(car binding)) ,@body)))
(defun helper (low upp fun)
  (if (> low upp) nil
    (progn (funcall fun low)
      (helper (1+ low) upp fun))))
(defmacro and (&rest args)
  (if args
      `(if ,(car args) (and ,@(cdr args)) nil)
    t))
(defmacro and (&rest args)
  (cond ((null args) t)
        ((null (cdr args)) (car args))
        (t `(if ,(car args) (and ,@(cdr args)) nil))))
(defmacro or (&rest args)
  (cond ((null args) nil)
        ((null (cdr args)) (car args))
        (t (let ((x (gensym)))
             `(let ((,x ,(car args)))
                (if ,x ,x (or ,@(cdr args))))))))

(defmacro defun-doc (name lambda-list doc &body body)
  `(progn (out "name: " (strs ',name) )
    (out "lambda-list: " (strs ',lambda-list))
    (out "documentation: " (strs ',doc))
    (defun ,name ,lambda-list ,@body)))
(defmacro set-seq (val &rest symbols)
  (if (null symbols) nil
    (let ((x (gensym)))
      `(let ((,x ,val))
         (setq ,(car symbols) ,x)
         (set-seq (1+ ,x) ,@(cdr symbols))))))
(defun set-seq-ex (val symbols)
  (if (null symbols) nil
    (let ((x (car symbols)))
    (progn (setq x val)
      (set-seq-ex (1+ val) (cdr symbols))))))
(defmacro set-seq (val &rest syms)
  `(set-seq-ex ,val ,syms))
(defmacro let* (bindings &body body)
  (if (null bindings) `(progn ,@body)
    `(let (,(car bindings))
       (let* ,(cdr bindings) ,@body))))
(defun let*exp (bindings body)
  (if (null bindings)
      `(progn ,@body)
    `(let (,(car bindings))
      ,(let*exp (cdr bindings) body))))
(defmacro mylet* (bindings &body body)
  (let*exp bindings body))
(defmacro all-cond (&rest branches)
  `(progn 
     ,@(mapcar (lambda (branch) `(if ,(car branch) (progn ,@(cdr branch)) nil)) branches)))
(defmacro in-ints (n &rest i)
  (if (null i) nil
    (let ((num (gensym)))
      `(let ((,num ,n))
         (or (and (>= ,num ,(car i))
                  (<= ,num ,(cadr i)))
             (in-ints ,num ,@(cddr i)))))))
(defmacro mdefun (name llist &body body)
  (let ((res (gensym))
        (val (gensym))
        (res (gensym)))
    `(let ((,res nil))
       (defun ,name ,llist 
         (let ((,val (progn ,@body)))
           (cond ((null ,res) (setf ,res ,val) ,val)
                 ((> ,val ,res) (setf ,res ,val) ,val)
                 (t ,res))))
       (defun ,(symbol (stra name "-max")) ()
         ,res))))

(defmacro m (a b)
  `(print ,a)
  `,b)
           
        

    
   
