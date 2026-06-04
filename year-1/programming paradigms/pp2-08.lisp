;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 08_exercises.lisp -- řešení úloh z cvičení 8
;;;


;1. Napište substituci, která bude výsledkem unifikace typů
;      (-> (-> (list a) (list b)) c a)
;      (-> (-> d (list number)) b c)
;Popište unifikaci po jednotlivých krocích.

;výsledný typ : (-> (-> (list num) (list num)) num num)
;S=((a.number) (b.number) (c.number) (d.(list number)))





;2. Napište typ funkce f v našem statickém typovém systému:
;      (tdefun f (a b c d e)
;      (cons a (cons (if b (+ c 1) d) e)))
;Popište odvození typu po jednotlivých krocích.

;výsledek je (list number)
;(-> number bool number number (list number) (list number))
;a=num, d= num, c=num, b= bool, e=(list num)
;



;if potrebuje (-> bool a a )
;a=nil...bool
;a'=cons...výsledek je (list bool) 
;a=!a'
;if nemá stejné typy pro a





;4:
;    (tdefun f (a)
;         (if (= 1 0) a (cons a empty)))

; a=! (list a)...v ifu (-> bool a a)...cons vrati (list a) a to neni stejneho typu jako a



;5 Zaveďte typovou funkci cons pro použití v polytypu ∀a∀b (cons a b), jehož
;prvky budou páry s libovolnou hodnotou car a cdr. Deklarujte pokusně pro
;funkce cons, car, cdr typy s použitím těchto typů.

;cons : ∀a ∀b (-> a b (cons a b))
(defun scons (a b)
  (cons a b))
;car  : ∀a ∀b (-> (cons a b) a)
(defun scar (list)
  (car list))
;cdr  : ∀a ∀b (-> (cons a b) b)
(defun scdr (list)
  (cdr list))
(decl-op-type scons a b c)
(decl-op-type scar (list a) a)
(decl-op-type scdr (list a) (list b))


;(tdefun cons (a b)
;  (append (list a) b))




#|6. Lze polytyp ∀a∀b (cons a b) používat k práci s obecnými seznamy, jejichž
prvky mohou být různých typů? Zkuste například definovat vlastní funkci na
zjištění délky seznamu složeného z párů obecného typu. Jako prázdný seznam
můžete použít cokoliv.
|#
#|(tdefun len (consvyraz)
      (if (eql empty consvyraz)
          0
        (1+ (len (scdr consvyraz)))))

|#

#|7. Určete, jakých typů budou následující dvě funkce, a své řešení ověřte.
(tdefun empty-twice-1 ()
   (cons empty empty))
                  ...empty=(list a)
                   ..empty=(list b)
                         (cons (list a) (list b))
                         cons: (-> t (list t) (list t))
                         t=list a
                         (list t)=list b
                         t=b
                         b=list a
                         
                         (-> (list a) (list (list a) (list (list a))))
                         výsledek: (list (list a))



(tdefun empty-twice-2 ()
    (let ((x empty))
      (cons x x)))
                     ...x pokazde to same
                     cons (-> t (list t) (list t))
                     t=x...x=(list h)
                     List t= x= (list h)....(list (list h))
                     výsledek: ERROR bcs (list h)!=(list (list h))
                           (list x) & x ...různé typy, nejosu stejné...nemohou se rovnat dle prom x


                           problém=typovani v letu x=(list x) empty je (list j)...prvni argument cons je list a druhy arg je stejny list->PROBLÉM
                           x=(list x)...ERROR

                           
                     

|#

(setf *type-functions* '(-> list vector maybe))

(defmacro twhen (condition body)
  `(if ,condition (just ,body) nothing))

(decl-op-type twhen bool a (maybe a))


;just:
;∀a (-> a (maybe a))
(decl-op-type just a (maybe a))

(defun just (val)
   val)

;nothing:
;∀a (maybe a)

(tdefvar nothing (maybe a) (gensym))





;eliminate
;∀a (maybe a)
(decl-op-type eliminate (maybe a) a)

(defun eliminate (a f g)
  (if (eql a 'nothing) 
    (funcall g)
    (funcall f a)))



;(-> a (list a) (maybe (list a)))

(defun member2 (el list)
  (if (emptyp list) nothing
    (if (eql (scar list) el) (just list)
      (member2 el (scdr list)))))

(defun infer-type (context sub cfun expr)
  "Odvodí typ výrazu v zadaném kontextu. Současně zkontroluje typovou konzistenci a případně vyvolá chybu."
  ;; Nejprve expandujeme makro. let-výrazy ale neexpandujeme.
  (let ((snode (if (let-expr-p expr) expr (macroexpand-1 expr))))
    (cond ((not (eql snode expr)) (infer-type context sub cfun snode))
          ;; Dále rozcestník (dispatch) podle druhu uzlu.
          ((literal-expr-p snode) 
           (infer-type-const sub cfun snode))
          ((var-expr-p snode) 
           (infer-type-var context sub cfun snode))
          ((consp snode) 
           (apply #'infer-type-compound context sub cfun snode))
          (t (error "Cannot infer type of " expr ".")))))




(defun infer-type-compound (context sub cfun op &rest rest)
  "Odvození typu a kontrola konzistence speciálního výrazu v zadaném kontextu."
  (cond
    ((eql op 'quote) (infer-type-const sub  cfun (car rest)))
    ((eql op 'function) (infer-type-op context sub cfun (car rest)))
    ((eql op 'let) (apply #'infer-type-let context sub cfun rest))
    ((eql op 'funcall) (apply #'infer-type-funcall context sub cfun rest))
    ((eql op 'lambda) (funcall #'infer-type-lambda context (first rest) (second rest) sub cfun ))
    (t (apply #'infer-type-funcall context sub cfun `#',op rest))))


(defun infer-type-lambda (context params body sub cfun)
  (infer-fun-type params body context sub cfun))


(defun x (a)
        (lambda (a) (+ 1 a)))

  
