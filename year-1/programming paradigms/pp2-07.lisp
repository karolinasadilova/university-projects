;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 07_exercises.lisp -- řešení úloh z cvičení 7
;;;
;tsafe=nejdriv zkontrolovat typy a pak vyraz vyhodnotit
(defmacro tsafe (expr)
  `(progn  
     (infer-type *global-context* ',expr)
     ,expr))

(decl-op-type 1+ number number)

(tdefun 1+ (number) (num)
  (+ num 1))
        
(decl-op-type pow2 number number)
(tdefun pow2 (number) (num)
  (* num num))
(decl-op-type number-null (list number) bool)
(defun number-null (list)
  (null list))
     
(decl-op-type number-null-2 (list number) bool)

(defun number-null-2 (list)
  (eql number-empty list))

(decl-op-type number-list-length (list number) number)
(defun number-list-length (list)
            (if (null list) 0
              (1+ (number-list-length (cdr list)))))
(decl-op-type build-number-list number (-> number number) (list number))
(defun build-number-list (len fun)
  (labels ((iter (len fun i)
             (cond ((or (= len 0)(= i len)) number-empty)
                   (t (number-cons (funcall fun i)
                                   (iter (- len 1) fun (1+ i)))))))
    (iter len fun 0)))

(decl-op-type number-list-append (list number) (list number) (list number))
(tdefun number-list-append ((list number)(list number)) (list1 list2)
  (if (number-null list1) list2
    (number-cons (number-car list1)
                 (number-list-append (number-cdr list1) list2))))




(decl-op-type number-list-filter (list number) (-> number bool) (list number))
(tdefun number-list-filter ((list number) (-> number bool)) (lst pred)
  (if (number-null lst) number-empty
        (if (funcall pred (number-car lst))
            (number-cons (number-car lst)
                      (number-list-filter (number-cdr lst) pred))
          (number-list-filter (number-cdr lst) pred))))

;co system umi:
;konstanty, proměnné, funcall,let,if&bezne fce pres tdefun
;chci pridat anonymní fci tlambda

;(defun infer-type (context expr)-vraci typ vyrazu expr v danem kontextu
;(tlambda: (-> number number);tlambda neresi typy 
;fce musi zkontrolovat delky listů, rozšíří kontext, spočítá typ těla a vrátí funkční typ
(defmacro tlambda (param-types params &body body)
  `(lambda ,params ,@body))

(defun infer-type-lambda (context param-types params body)
  (unless (= (length param-types) (length params))
          (error "diff length" (strs param-types) "and " (strs params) ))
  (let* ((new-context (context-prepend context params param-types))
         (body-type (infer-type new-context body)))
    (append (list '->)
                  param-types
                  (list body-type))))

(defun infer-type-compound (context op &rest rest)
  "Odvození typu a kontrola konzistence speciálního výrazu v zadaném kontextu."
  (cond
    ((eql op 'quote) (infer-type-const (car rest)))
    ((eql op 'function) (infer-type-op (car rest)))
    ((eql op 'if) (apply #'infer-type-if context rest))
    ((eql op 'let) (apply #'infer-type-let context rest))
    ((eql op 'funcall) (apply #'infer-type-funcall context rest))
    ((eql op 'tlambda) (apply #'infer-type-lambda context rest))
    (t (apply #'infer-type-funcall 
              context
              `#',op 
              rest))))
                      
(defun tlambda-expr-p (expr)
  (and (consp expr)
      (eql 'tlambda (car expr))))

(defun infer-type (context expr)
  "Odvodí typ výrazu v zadaném kontextu. Současně zkontroluje typovou konzistenci a případně vyvolá chybu."
  ;; Nejprve expandujeme makro. let-výrazy ale neexpandujeme.
 (let ((snode (if (or (tlambda-expr-p expr)
                      (let-expr-p expr)) expr (macroexpand-1 expr))))
    (cond ((not (eql snode expr)) (infer-type context snode))
          ;; Dále rozcestník (dispatch) podle druhu uzlu.
          ((literal-expr-p snode) (infer-type-const snode))
          ((var-expr-p snode) (infer-type-var context snode))
          ((consp snode) (apply #'infer-type-compound context snode))
          (t (error "Cannot infer type of " (strs expr) ".")))))
(defmacro tsafe2 (exp)
  `(progn
     (infer-type *global-context* ',exp)
     ,exp))
(tdefun addone (number) (x)
  (+ 1 x))
(decl-op-type addone number number)
(decl-op-type pw2 number number)
(tdefun pw2 (number) (x) (* x x))
(decl-op-type numnull (list number) bool)
(defun numnull (lst)
  (eql number-empty lst))

(tdefun nm-l-len ((list number)) (list)
  (if (numnull list) 0
    (1+ (nm-l-len (cdr list)))))
(defun build-num-list  (len fun)
  (labels ((iter (i fun len)
             (cond ((= len 0) number-empty)
                   ((= i len) number-empty)
                   (t(number-cons (funcall fun i)
                                 (iter (1+ i) fun (1- len)))))))
    (iter 0 fun len)))
(decl-op-type build-num-list number (-> number number) (list number))
(decl-op-type n-list-ap (list number) (list number) (list number))
(tdefun n-list-ap ((list number)(list number)) (list1 l2)
  (if (number-null list1) l2
    (number-cons (number-car list1) (n-list-ap (number-cdr list1) l2))))
(decl-op-type number-list-filter (-> number bool) (list number) (list number))
(tdefun number-list-filter ((-> number bool) (list number )) (pred list)
  (if (number-null list) number-empty
    (if (funcall pred (number-car list)) (number-list-filter pred (number-cdr list))
      (number-cons (number-car list) (number-list-filter pred (number-cdr list))))))
                   
