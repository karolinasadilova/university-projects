;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 10_excercises.lisp -- řešení úloh k cvičení 10
;;;

#|
Do tohoto souboru pište řešení úloh z cvičení. Soubor se bude při každém 
spuštění aplikace načítat, takže všechny definice budou k dispozici.

Pokud je v některé definici chyba, soubor se nenačte a aplikace chybu
oznámí. Pak je možné ji opravit a soubor načíst ručně z menu (Evaluate
All Expressions) nebo klávesovou zkratkou.

Nově vytvořené definice vždy hned vyhodnoťte (menu Evaluate Expression
nebo Evaluate All Expressions) a otestujte v Listeneru.

Chybné a rozpracované definice můžete uzavírat do komentářů tak, jak to
je uděláno s tímto textem (víceřádkový komentář) nebo s nadpisem souboru
nahoře (jednořádkový komentář).
|#
(defun my-test-structure (struct test)
  (if (atom struct) 
      (if (funcall test struct) struct nil)
    (let ((res (my-test-structure (car struc) test)))
          (if (null res) (my-test-structure (cdr struc) test) res))))
(defun my-struct-find-if (test struc)
  (if (atom struc) (and (funcall test struc) struc)
    (or (struc-find-if test (car struc))
        (struc-find-if test (cdr struc)))))
(defun my-struc-count (struc el)
  (cond ((cosnp struc) (+ (my-struc-count (car struc))
                          (my-struc-count (cdr struc))))
        ((eql struc el) 1)
        (t 0)))
(defun my-reverse (list)
  (foldr (lambda (element acc) (append acc (list element)))
         list nil))
          


(defun list-to-number (list)
  (labels ((iter (acc  list)
             (if (null list) acc
                   (iter (+ (car list) (* 10 acc)) (cdr list)))))
    (iter 0 list)))


(defun tree-height (tree)
  (if (null (node-children tree)) 0
    (1+ (find-longest-list (node-children tree)))))
  
(defun find-longest-list (roots)
  (if (null roots) 0
    (max (tree-height (car roots)) (find-longest-list (cdr roots)))))

(defun tree-remove (node pred)
  (if (funcall pred
 (node-value node)) 
      (tree-remove-help (node-children node) pred)
    (cons (node-value node)
          (tree-remove-help (node-children node) pred))))
(defun tree-remove-help (children pred)
  (if (null children) nil
    (let ((res (tree-remove (car children) pred)))
            (if (null res)
                (tree-remove-help (cdr children) pred)
              (cons res (tree-remove-help (cdr children) pred))))))


(defun tree-level (node n)
  (if (= n 0)
      (list (node-value node))
    (tree-level-help (node-children node) (- n 1))))
(defun tree-level-help (children level)
  (if (null children) nil
      
   (append (tree-level (node-value children) level)
           (tree-level-help (node-children children) level))))



(defun find-path (node n)
  (if (eql n (node-value node)) (list (node-value node))
    (let ((subpath (find-path-help (node-children node) n)))
      (if (null subpath) nil
    (cons (node-value node) subpath)))))
(defun find-path-help (children n)
  (if (null children) nil
    (let ((subpath (find-path (car children) n)))
      (if (null subpath) (find-path-help (cdr children) n)
        subpath))))

;;u sublistu nestaci elementp nebo checkovat jestli jsou eql protoze to by nevyresilo treba (1 2 3 4) (2 3)
(defun sublistp (list sublist)
  (helper list sublist))
        
(defun helper (list sublist)
  (cond ((null sublist) t)
        ((null list) nil)
        ((eql (car sublist) (car list))
         (helper (cdr list) (cdr sublist)))
        (t (helper (cdr list) sublist))))
(defun struct-count1 (elem struct)
  (cond ((consp struct) (+ (struct-count1 elem (car struct))
                           (struct-count1 elem (cdr struct))))
         ((eql elem struct) 1)
         (t 0)))
(defun struct-sum1 (struct)
  (cond ((numberp struct) struct)
        ((atom struct) 0)
        ((consp struct) (+ (struct-sum1 (car struct))
                             (struct-sum1 (cdr struct))))))
(defun struct-sum (struct)
  (labels((iter (struct res)
            (cond ((numberp struct) (+ res struct))
                  ((atom struct) res)
                  (t (iter (car struct)
                           (iter (cdr struct) res))))))
    (iter struct 0)))
(defun struct-to-list (struct)
  (if (atom struct) (list struct)
    (append (struct-to-list (car struct))
            (struct-to-list (cdr struct)))))
(defun struct-to-list2 (struct)
  (labels((iter (struct res)
            (cond ((atom struct) (cons struct res))
                  (t (struct-to-list2 (car struct)
                                      (struct-to-list2 (cdr struct) res))))))
    (iter struct nil)))
                     


;;zk:
(defun trianglep (a b c)
  (let ((ab (+ a b))
        (bc (+ b c))
        (ac (+ a c)))
    (labels ((compare (a b)
               (if (> a b) t nil)))
      (and (compare (+ a b) c)
           (compare (+ b c) a)
           (compare (+ a c) b)))))
(defun digit-sum(number)
  (labels ((iter (i acc)
             (if (< i 0) acc
               (iter (1- i) (+ (digit number i) acc)))))
    (if (< number 10)
        number
      (iter (- (digit-count number) 1) 0))))
(defun append2(l1 l2)
  (if (null l1) 
      l2
    (cons (car l1)
          (append2 (cdr l1) l2))))
;'#'()
;(FUNCTION NIL)
                  
(defun selfreturn ()
  #'selfreturn)
(defun sum-of-squares-pred (n)
  (labels ((iter (i n acc)
             (cond ((= n 0) t)
                   ((= acc n) t)
                   ((> i (sqrt n)) nil)
                   ((> acc n) nil)
                   (t (or (iter (1+ i) n (+ acc (power2 i)))
                          (iter (1+ i) n acc))))))
    (iter 0 n 0)))
;cisla pridava postupne pro 8= pw2+pw2 nefunguje


(defun fibb-rec (n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (t (+ (fibb-rec (1- n ))
              (fibb-rec (- n 2))))))
(defun fib-tail (n)
  (labels((iter (n i curr prev)
            (cond ((= n 0) 0)
                  ((= n 1) 1)
                  ((> i n) curr)
                  (t (iter n (1+ i) (+ curr prev) curr)))))
    (iter n 2 1 0)))

;tbl reprez jako szm sznmů vrací seznam prvku daneho columnu
(defun column (n tbl)
  (if (= n 0) (mapcar #'car tbl)
    (column (1- n) (mapcar #'cdr tbl))))
(defun column2 (n tbl)
  (mapcar (lambda (list) (nth n list)) tbl))

(defun intersect (set1 set2)
  (cond ((null set1) nil)
        ((find (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
        (t (intersect (cdr set1) set2))))
;pro setřízené množiny:
(defun intersect2 (s1 s2)
  (cond ((or (null s1) (null s2)) nil)
        ((eql (car s1) (car s2)) (cons (car s1) (intersect2 (cdr s1) (cdr s2))))
        ((> (car s1) (car s2)) (intersect2 s1 (cdr s2)))
        (t (intersect2 (cdr s1) s2))))
       
(defun next-seq(seq)
  (lambda (n) (* (mem seq n)
                 (mem seq (1+ n)))))
;deepsum na sumu vsech uzlů ve stromu
(defun deepsum (node)
  (if (null (node-children node))
      (node-value node)
    (+ (node-value node)
       (deepsum-list (node-children node)))))
(defun deepsum-list (subtrees)
  (if (null subtrees) 0
    (+ (deepsum (car subtrees))
       (deepsum-list (cdr subtrees)))))

;apply #'list 'list '())?
;(list)
;ne listnil cuz nil se nepise


(defun list-to-number (list)
  (labels((iter (list acc)
            (if (null list) acc
                  (iter (cdr list) (+ (car list) (* acc 10))))))
    (iter list 0)))


(defun subintervals (low high)
  (if (> low high) nil
    (append (subintervals-hlp low low high)
          (subintervals (1+ low) high))))

(defun subintervals-hlp (start curr end)
  (if (> curr end) nil
    (cons (cons start curr)
          (subintervals-hlp start (1+ curr) end))))
(defun every (list pred)
  (or (null list)
      (and (funcall pred (car list))
           (every (cdr list) pred))))

(defun last-n (list n)
  (let ((len (length list)))
 (labels ((iter (i list)
            (if (= i 0) list
              (iter (1- i) (cdr list)))))
   (if (> n len) (- n len)
     (iter (- len n)
         list)))))
(defun tree-level (node level)
  (if (= level 0) (list (node-value node))
    (tree-level-list (node-children node) (1- level))))
(defun tree-level-list (subtrees level)
  (if (null subtrees) nil
        (append (tree-level (car subtrees) level)
                (tree-level-list (cdr subtrees) level))))
;speciální operátor or-zkrácené vyhodnocování

        
(defun divby9 (num)
  (labels ((digitsum (num i acc)
             (if (< i 0) acc
                   (digitsum num (1- i) (+ (digit num i) acc)))))
    (cond((= num 9) t)
         ((< num 10) nil)
         (t (divby9 (digitsum num (- (digit-count num) 1) 0))))))
;pokudje jednociferne a neni to 9 nil
;(cdr (car (cdr (car ((a ((b) c)) ((d (e) f)))))))
;(c)

(defun seq-find (seq pred low hig)
  (cond ((> low hig) nil)
        ((funcall pred (mem seq low)) low)
        (t (seq-find seq pred (1+ low) hig))))
(defun make-counter (num)
  (lambda (list)
    (count num list)))
;(funcall (make-counter 5) '(2 4 5 7 8 6 5 5 4 3 5))
;4

(defun my-reverse (list)
  (labels ((revappend (l1 l2)
             (if (null l1) l2
               (revappend (cdr l1) (cons (car l1) l2)))))
    (revappend list nil)))
(defun mlist (list)
  (if (null list) nil
    (cons (mapcar #'* list)
          (mlist (cdr list)))))
(defun tree-to-list (node)
  (if (null (node-children node))
      (list (node-value node))
    (cons (node-value node)
          (tree-to-list-children (node-children node)))))

(defun tree-to-list-children (subtrees)
  (if (null subtrees) nil
    (append (tree-to-list (car subtrees))
          (tree-to-list-children (cdr subtrees)))))
(defun same-sums-p (list)
  (apply #'= (mapcar (lambda (sublist) (apply #'+ sublist)) list)))
(defun prefix-to-infix (list)
    (cond ((or (atom list)(null (cdr list))) list)
          (t (let ((op (car list))
                   (args (cdr list)))
              (apply #'append (list (car args)) (mapcar (lambda (arg) (list op arg)) (cdr args)))))))

;max-list protoze normalni max porovnava A a B ne 2 listy
(defun max-list (l1 l2)
  (let ((len1 (length l1))
        (len2 (length l2)))
    (if (> len1 len2) l1 l2)))
(defun max-path(node)
  (if (null (node-children node))
      (list (node-value node))
    (cons (node-value node) (max-path-list (node-children node)))))
(defun max-path-list (subtrees)
  (if (null subtrees) nil
    (max-list (max-path (car subtrees))
              (max-path-list (cdr subtrees)))))
(defun seq-shift (shift seq)
  (lambda (n)
    (if (< (- n shift) 0) 0 (mem seq (- n shift)))))
(defun intersection-general (set1 &rest sets)
  (foldr #'intersect sets set1))
(defun find-path-to-value (node value)
  (if (null (node-children node))
      (if (eql (node-value node) value) (list (node-value node)) nil)
    (cons (node-value node)
          (find-path-to-value-to-list (node-children node) value))))
(defun find-path-to-value-to-list (subtrees value)
  (if (null subtrees) nil
    (or (find-path-to-value (car subtrees) value)
        (find-path-to-value-to-list (cdr subtrees) value))))

(defun neco (x)
    (if (consp x)
             (cons (neco (cdr x))
                         (neco (car x)))
        x))
;> (neco '(1 2 3))
;(((NIL . 3) . 2) . 1)

(defun tree-hei (node)
  (if (null (node-children node)) 0
    (1+ (tree-hei-list-max (node-children node)))))
(defun tree-hei-list-max (subtrees)
  (if (null subtrees) 0
    (max (tree-hei (car subtrees))
         (tree-hei-list-max (cdr subtrees)))))


(defun my-last-n (list n)
  (let ((i (- n (length list))))
    (labels ((iter (list i)
               (if (= i 0) list
                 (iter (cdr list) (1- i)))))
      (cond ((= n 0) nil)
            ((null list) n)
            ((> i 0) i)
            (t (iter list i))))))
;(car ''())
;QUOTE

;Rozdíl mezi if a my-if
;if=specialni op, zkracene vyhodnocovani my-if je funkce vyhodnoti vsechny sve argumenty a nasledne navaze na parametry fce

(defun interval (upper lower)
  (cons upper lower))
(defun upper (interval)
  (car interval))
(defun lower (interval)
  (cdr interval))
(defun intersection-interval (lower upper)
  (if (> lower upper)
      nil
    (cons (lower 
           (intersection-interval (1+ lower) upper)))))
;;tabulka 10x10 
(defun zero-row-tbl (tbl row)
  (labels ((iter (column)
             (cond ((> column 9) t)
                   ((zerop (funcall tbl row column)) (iter (1+ column)))
                   (t nil))))
    (iter 0)))
(defun interleave (l1 l2)
  (cond ((null l1) l2)
        ((null l2) l1)
        (t (cons (car l1) 
                 (cons (car l2)
                 (interleave (cdr l1) (cdr l2)))))))
#|(defun pascal-row (row)
  (labels ((iter (row col)
             (cond ((= row col) nil)
                   (t (+ (iter (1- row) col)
                         (iter (1- row) (1- col)))))))
    (iter row 0)))
|#

(defun first-less (seq1 seq2)
  (labels ((iter (i)
             (if (< (mem seq1 i) (mem seq2 i)) i
                 (iter (1+ i)))))
    (iter 0)))

;factorial lmbda
(let ((f (lambda (fun n)
           (if (= n 0) 1
             (* n (funcall fun fun (1- n)))))))
  (funcall f f 3))

(lambda (n)
  (funcall (lambda (fun n) 
             (if (= n 0) 1
               (* n (funcall fun fun (1- n)))))
               (lambda (fun n)
                 (if (= n 0) 1
                   (* n (funcall fun fun (1- n))))) n))


;(let ((x a)) tělo)
;(funcall (lambda (x) (body)) a)

(defun sums (list num)
  (labels ((iter (acc list num)
             (cond ((zerop num) 1)
                   ((null list) 0)
                   (t (+ (iter (+ acc (car list)) (cdr list) (- num (car list)))
                         (iter acc (cdr list) num))))))
    (iter 0 list num)))
             

(defun count (list)
  (cond ((consp list) (+ (count (car list))
                         (count (cdr list))))
        ((null list) 1)
        (t 0)))

;(count '(1 (2 3) 4) )
;0+0+1+1=2

(defun each-nd (list)
  (if (null list)
      nil
    (cons (car list)
          (each-nd-help (cdr list)))))
(defun each-nd-help (list)
  (if (null list)
      nil
    (each-nd (cdr list))))

(defun seq-count-if (seq pred n)
  (labels ((iter (i)
             (cond ((> i n) 0)
                   ((funcall pred (mem seq i)) (1+ (iter (1+ i))))
                   (t (iter (1+ i))))))
    (iter 0)))

(defun pref-to-inf (list)
  (cond ((atom list) list)
        ((null (cdr list)) list)
        (t (let ((op (car list))
                 (args (cdr list)))
             (apply #'append (if (consp (car args))
                                 (pref-to-inf (car args))
                               (list (car args)))
                    (mapcar (lambda (arg) (if (consp arg)
                                              (append (list op) (pref-to-inf arg))
                                            (list op arg))) (cdr args)))))))




;apply #'list 'list '())
;(list)

;(car (cdr ((1 . 2) (3 . 4))))
;(1.2) neni symbol

;cdr (car (cdr (car ()))))
;car nilu nejde
(defun copy-tree (node)
  (cond ((atom node) node)
        (t (cons (copy-tree (node-value node))
                 (copy-tree (node-children node))))))
        

(defun tree-rem (node el)
  (cond ((atom node) node)
        ((eql el (node-value node)) (tree-rem (node-children node) el)) 
        (t (cons (tree-rem (node-value node) el)
                 (tree-rem (node-children node) el)))))




(defun tree-remove (pred node)
  (if (funcall pred (node-value node))
      (tree-remove-list pred (node-children node))
    (cons (node-value node)
          (tree-remove-list pred (node-children node)))))
(defun tree-remove-list (pred subtrees)
  (if (null subtrees) nil
    (let ((first (tree-remove pred (car subtrees)))) 
      (if first (cons (tree-remove pred (car subtrees)) 
                      (tree-remove-list pred (cdr subtrees)))
        (tree-remove-list pred (cdr subtrees))))))
(defun pascal-row (n)
  (if (= n 0)
      '(1)
    (let ((prev (pascal-row (- n 1))))
      (cons 1 (mapcar '+ prev
                      (append (cdr prev) '(0)))))))

;4:
(defun fast_power (a n)
  (cond((= n 0) 1)
       ((evenp n) (power2 (fast_power a (/ n 2))))
       (t (* a (fast_power a (- n 1))))))
#|
(defun fast_power2 (a n)
  (cond ((= n 0) 1)
        ((evenp n)(fast_power2 (fast_power2 a (/ n 2)) 2))
        (t (* a (fast_power2  a (- n 1))))))

|#
;fce se zacykly porad bude sude n jelikoz se to porad volá dokola s 2

   
(defun fast-power-tail (a n)
  (labels ((iter (a n acc)
             (if (= n 0) acc
                   (iter a (1- n) (* acc a)))))
    (iter a n 1)))



    
(defun dividesp (a b)
  (zerop (rem b a)))

(defun primep (n)
  (and (> n 1)
       (labels ((iter (i n)
                  (cond ((= i n) t)
                        ((zerop (rem n i)) nil)
                        (t (iter (1+ i) n)))))
         (iter 2 n))))
(defun perfectp (num)
  (labels ((iter (acc num i)
             (cond ((primep num) nil)
                   ((> acc num) nil)
                   ((and (= i num) (= acc num)) t)
                   ((= i num) nil)
                   ((zerop (rem num i)) (iter (+ acc i) num (1+ i)))
                   (t (iter acc num (1+ i))))))
    (iter 0 num 1)))
(defun fib-tail2 (n)
  (labels ((iter (index n prev curr )
             (cond ((= n 0) 0)
                   ((= n 1) 1)
                   ((= index n) curr)
                   (t (iter (1+ index) n curr (+ curr prev))))))
    (iter 2 n 0 1)))


(defun pascal-row-col (row col)
  (cond ((> col row) nil)
        ((or (zerop row) (zerop col) (= row col)) 1)        
        (t (+ (pascal-row-col (1- row) col)
            (pascal-row-col (1- row) (1- col))))))
(defun sms-of-sq-p (n)
  (labels ((iter (n i acc)
             (cond ((= n acc) t)
                   ((> (power2 i) n) nil)
                   ((> acc n) nil)
                   (t (or (iter n (1+ i) (+ acc (power2 i)))
                          (iter n (1+ i) acc))))))
    (iter n 0 0)))
;odstraní podstrom ktery splnuje predikat a root predikat nesplnuje

;rekurzivne kontroluje podstromy

(defun tree-remove-subtree-if-help (subtrees pred)
  (cond ((null subtrees) nil)
        ((funcall pred (node-value (car subtrees))) (tree-remove-subtree-if-help (cdr subtrees) pred))
        (t (cons (tree-remove-subtree-if (car subtrees) pred)
                 (tree-remove-subtree-if-help (cdr subtrees) pred)))))

(defun tree-remove-subtree-if (node pred)
  (cons (node-value node)
        (tree-remove-subtree-if-help (node-children node) pred)))

(defun tree-remove-if (node pred)
  (if (funcall pred (node-value node))
      (tree-remove-if-list (node-children node) pred)
    (cons (node-value node)
          (remove-nil-from-list (mapcar (lambda (child) (tree-remove-if child pred)) (node-children node))))))

(defun remove-nil-from-list (list)
  (cond ((null list) nil)
        (t (if (eql (car list) nil)
               (remove-nil-from-list (cdr list))
             (cons (car list) (remove-nil-from-list (cdr list)))))))

(defun each-nth (list n)
  (labels ((iter (list n index)
             (cond ((null list) nil)
                   ((= n index) (cons (car list)
                                      (iter (cdr list) n 0)))
                   (t (iter (cdr list) n (1+ index))))))
    (cons (car list)
          (iter list n 0))))
(defun rig-tri-p (a b c)
  (let ((c (point-d-2 a b))
        (a (point-d-2 b c))
        (b (point-d-2 a c)))
    (or (= c (sqrt (+ (power2 a) (power2 b))))
        (= a (sqrt (+ (power2 b) (power2 c))))
        (= b (sqrt (+ (power2 a) (power2 c)))))))


(defun point-d-2 (a b)
  (sqrt (+ (expt (- (point-x A) (point-x B)) 2)
           (expt (- (point-y A) (point-y B)) 2))))

(defun op-vertex2 (a b)
  (point (+ (point-x b) (- (point-x a) (point-x b)))
         (+ (point-y b) (- (point-y a) (point-y a)))))
(defun div-frac (frac1 frac2)
  (fraction (* (numer frac1) (denom frac2))
            (* (denom frac1) (numer frac2))))
(defun m-interval (lower upper)
  (cons lower upper))
(defun m-lower (interval)
  (car interval))
(defun m-upper (interval)
  (cdr interval))
(defun m-num-in-int (el interval)
  (cond ((null interval) nil)
        ((eql (car interval) el) el)
        (t (m-num-in-int el (cdr interval)))))

(defun m-intersection-interval (interval1 interval2)
  (cond ((or (null interval1) (null interval2)) nil)
        ((m-num-in-int (lower interval1) interval2) (cons (lower interval1) (m-intersection-interval (upper interval1) interval2)))
        (t (m-intersection-interval (upper interval1) interval2))))

(defun pos (el list)
  (labels ((iter (el list i)
             (cond ((null list) nil)
                   ((eql el (car list)) i)
                   (t (iter el (cdr list) (1+ i))))))
    (iter el list 0)))
(defun eql-lst-p (lst1 lst2)
  (cond ((not (= (length lst1) (length lst2))) nil)
        ((and (null lst1) (null lst2)) t)
        ((eql (car lst1) (car lst2)) (eql-lst-p (cdr lst1) (cdr lst2)))
        (t nil)))

(defun m-mismatch (list1 list2)
    (labels ((iter (pos list1 list2)
               (cond ((eql-lst-p list1 list2) nil)
                     ((not (eql (car list1) (car list2))) pos)

                     (t (iter (1+ pos) (cdr list1) (cdr list2))))))
      (iter 0 list1 list2)))
       
(defun m-last (list len)
  (labels ((iter (list i)
             (cond ((null list) i)
                   ((zerop i) list)
                   (t (iter (cdr list) (1- i)))))
           (iter2 (slow fast)
             (if (null fast) slow
               (iter2 (cdr slow) (cdr fast)))))
    (let ((res (iter list len)))
      (if (numberp res)
          res
        (iter2 list res)))))

(defun last-5 (list n)
  (labels ((iter (list n)
             (cond ((null list) n)
                   ((zerop n) list)
                   (t (iter (cdr list) (1- n)))))
           (iter2 (slow fast)
             (if (null fast) slow
               (iter2 (cdr slow) (cdr fast)))))
    (let ((res (iter list n)))
      (if (numberp res)
          res
        (iter2 list res)))))
          
          
          

;__________________________________________________________________________
;ex6:
(defun make-ar-seq-list (a1 d n)
  (if (zerop n) nil
        (cons a1 (make-ar-seq-list (+ a1 d) d (1- n)))))
(defun make-ar-seq-list-tail (a1 d n)
  (labels ((iter (a1 d n acc)
             (if (zerop n) (reverse acc)
                   (iter (+ a1 d) d (1- n) (cons a1 acc)))))
    (iter a1 d n nil)))
(defun make-geom-seq-list (a1 q n)
  (if (zerop n) nil
    (cons a1 (make-geom-seq-list (* a1 q) q (1- n)))))
(defun make-geom-seq-list-tail (a1 q n)
  (labels ((iter (a1 q n acc)
             (if (zerop n) (reverse acc)
               (iter (* q a1) q (1- n) (cons a1 acc)))))
    (iter a1 q n nil)))
(defun cpy-list (list)
  (if (null list) nil
    (cons (car list)
          (cpy-list (cdr list)))))
(defun remove (el list)
  (cond ((null list) nil)
        ((eql (car list) el) (remove el (cdr list)))
        (t (cons (car list)
                 (remove el (cdr list))))))
(defun tailp (list1 list2)
  (cond ((null list1) t)
        ((eql list1 list2) t)
        ((null list2) nil)
        (t (tailp list1 (cdr list2)))))
(defun ldiff-2 (list1 list2)
  (labels ((iter (l1 l2)
             (cond ((or (null l2)(null l1)) nil)
                   ((eql (car l1) (car l2)) nil)
              (t (cons (car l1)
                     (iter (cdr l1) (cdr l2)))))))
    (if (tailp list2 list1)
        (iter list1 list2)
      (cpy-list list1))))
(defun factorials-list (len)
  (labels ((iter (ir acc len)
             (cond ((= ir len) nil)
                   ((= ir 0) (cons 1 (iter  (1+ ir) acc len)))
                   (t (cons (* ir acc) (iter (1+ ir) (* acc ir) len))))))
    (iter 0 1 len)))
(defun fib-to-list2 (len)
  (labels ((iter (ir prev curr len)
             (cond ((= ir len) nil)
                   ((= ir 0) (cons 0 (iter (1+ ir) prev curr len)))
                   ((= ir 1) (cons 1 (iter (1+ ir) prev curr len)))
                   (t (cons (+ prev curr) (iter (1+ ir) curr (+ curr prev) len))))))
    (iter 0 0 1 len))) 
(defun lst-tails (list)
  (cond ((null list) (list nil))
        (t(cons list
              (lst-tails (cdr list))))))
#|

(defun my-sum-help (list n len)
  (if (>= n len)
      0
    (+ (nth n list) (my-sum-help list (+ n 1) len)))))
(defun my-sum (list)
(my-sum-help list 0 (length list))))


Kolikrát se během vyhodnocování výrazu 
(my-sum (list 1 2 3 4 5 6 7 8 9 10)) zavolá funkce cdr?

nth vola cdr:pro kazde n nkrat
lenght vola cdr :9

9+9+8+7+6+5+4+3+2+1=54

|#
                         
          
(defun subtract-lst-2 (l1 l2)
  (if (null (or l2 l1)) nil
        (cons (- (car l1) (car l2))
              (substract-lst-2 (cdr l1) (cdr l2)))))

(defun sclr-product (l1 l2)
  (apply '+ (mapcar #'* l1 l2)))

(defun vctr-len (vector)
  (apply #'sqrt (mapcar #'power2 vector)))

(defun union-2 (set1 set2)
  (cond ((null set1) set2)
        ((elementp (car set1) set2) (union-2 (cdr set1) set2))
        (t (cons (car set1)
                 (union-2 (cdr set1) set2)))))
(defun rem-dupl (list)
  (cond ((null list) nil)
        ((elementp (car list) (cdr list))
         (rem-dupl (cdr list)))
        (t (cons (car list)
                 (rem-dupl (cdr list))))))
(defun eql-sets-p (set1 set2)
  (and (= (length set1) (length set2))
              (subsetp set1 set2)))


;8:
(defun member (el list)
  (cond ((null list) nil)
        ((eql (car list) el) list)
        (t (member el (cdr list)))))

(defun my-member-t (el list test)
  (cond ((null list) nil)
        ((funcall test el (car list)) list)
        (t (my-member-t el (cdr list) test))))
(defun my-member-if (test list)
  (cond ((null list) nil)
        ((funcall test (car list)) list)
        (t (my-member-if test (cdr list)))))
(defun countif (list test)
  (cond ((null list) 0)
        ((funcall test (car list)) (1+ (count-if (cdr list) test)))
        (t (count-if (crd list) test))))
(defun countt (el list test)
  (cond ((null list) 0)
        ((funcall test el (car list)) (1+ (countt el (cdr list) test)))
        (t (countt el (crd list) test))))


(defun merge-sort-algo (list pred)
  (if (or (null list) (null (cdr list))) list
        (merge-procedure pred 
                         (merge-sort-algo (even-conses list) pred)
                         (merge-sort-algo (odd-conses list) pred))))
(defun merge-procedure (pred list1 list2 )
  (cond ((null list1) list2)
        ((null list2) list1)
        ((funcall pred (car list1) (car list2))
         (cons (car list1) (merge-procedure pred (cdr list1) list2)))
        (t (cons (car list2)
                 (merge-procedure pred list1 (cdr list2))))))
  
(defun arit-mean (n &rest numbers)
        (/ (foldr #'+ numbers n) (1+ (length numbers))))

(defun eql-lists-p-rest (list1 &rest lists)
  (every lists (lambda (sublist) (eql-lst-p list1 sublist))))
(defun lenght-2 (list)
  (foldr (lambda (el acc) (1+ acc)) list 0))
(defun my-mapcar-1 (fun list)
  (if (null list) nil
  (foldr (lambda (el acc) (cons (funcall fun el) acc)) list nil)))
  
(defun my-foldl (fun list init)
  (labels ((iter (acc list)
             (if (null list) acc
               (iter (funcall fun acc (car list)) (cdr list)))))
    (iter init list)))


            
;(1 2 3)
;(funcall f 1(funcall f 2(funcall f 3 init)))-foldr
;(f (f (f init 1) 2) 3)-foldl


(defun my-foldr (fun list init)
  (if (null list) init
    (funcall fun (car list) (my-foldr fun (cdr list) init))))


;ex9:
;(funcall (lambda (x) (funcall x 10)) (lambda (x) 20))



; x v prni lambda je = (lambda (x) 20)

;(funcall (lambda (x) 20) 10)
;20


;(let ((x a)) tělo )
;(funcall (lambda (x) (telo)) a)

;rekurze bez jmena fce:
;fce bere sama sebe jako arg

(let ((f (lambda (fun n)
           (if (= n 0) 1
             (* n (funcall fun fun (1- n)))))))
  (funcall f f 3))
(defun my-const-seq-p (k seq)
  (labels ((iter (ptr k seq)
             (cond ((= (1- k) ptr) t)
                   ((eql (mem seq ptr) (mem seq (1+ ptr))) 
                    (iter (1+ ptr) k seq))
                   (t nil))))
    (iter 0 k seq)))
(defun my-increasing-seq-p (seq k)
  (labels ((iter (i seq k)
             (cond ((= (1- k) i) t)
                   ((> (mem seq i) (mem seq (1+ i))) (iter (1+ i) seq k))
                   (t nil))))
    (iter 0 seq k)))
(defun my-from-nth (seq n)
  (lambda (k)
    (mem seq (+ n k))))

(defun my-even-members (seq)
  (lambda (n) (mem seq (* 2 n))))
(defun my-zero-row-p (tbl row)
  (labels ((iter (col)
             (cond ((> col 9) t)
                   ((zerop (funcall tbl row col)) (iter (1+ col)))
                   (t nil))))
    (iter 0)))
(defun my-transpose-table (row col)
  (lambda (row column) (tbl col row)))

;10:
(defun flatten (struct)
  (cond ((null struct) nil)
        ((consp struct) 
         (append (flatten (car struct))
               (flatten (cdr struct))))
        (t (list struct))))
(defun flatt (struct)
  (labels ((iter (struct res)
             (cond ((null struct) res)
                   ((atom struct) (cons struct res))
               (t (iter (car struct) (iter (cdr struct) res)))
                   )))
    (iter struct nil)))


(defun deep-rev (struct)
  (cond ((null struct) nil)
        ((atom struct) struct)
        (t (deep-rev-list struct))))


(defun deep-rev-list(subtrees)
  (if (null subtrees) nil
    (append (deep-rev-list (cdr subtrees))
          (list (deep-rev (car subtrees))))))
(defun deepr (struct)
(cond ((null struct) nil)
      ((atom struct)  struct)
      (t (deeprh struct))))
(defun deeprh (substruct)
  (if (null substruct) nil
    (append (deeprh (cdr substruct))
            (list (deepr (car substruct))))))

#|4. Funkce count je napsána takto:
(defun count (x)
  (cond ((consp x) (+ 1 (count (car x)) (count (cdr x))))
        ((null x) 100)
        (t 0)))
Jakou hodnotu má výraz (count'(1 (2 3) 4 (5 . 6)))?
 |#
 
#|       
(count'(1 (2 3) 4 (5 . 6)))
1+((2 3) 4 (5. 6))
1+(2 3) + (4 (5.6))
1+ 0 +1+ 100+ 1+0+1+1+100
207
|#

(defun cpy-str (struct)
  (cond ((null struct) nil)
        ((atom struct) struct)
        (t (cons (cpy-str (car struct))
                 (cpy-str (cdr struct))))))
(defun deep-reverse-tr (struct)
  (cond ((null struct) nil)
        ((atom struct) struct)
        (t (deep-reverse-tr-hlp struct))))
(defun deep-reverse-tr-hlp (substruct)
  (if (null substruct) nil
    (append (deep-reverse-tr-hlp (cdr substruct))
            (list (deep-reverse-tr (car substruct))))))

;(a (c (b) (d)) (f (g (h)) (i)))

#|
(tree-node 'a
           (list (tree-node 'c
                      (list (tree-node 'b nil)
                            (tree-node 'd nil)))
                      (tree-node 'f
                                (list  (tree-node 'g 
                                            (tree-node 'h nil))
                                 (tree-node 'i
                                            nil)))))
|#


(defun tree-maximal-paths (node)
  (if (null (node-children node)) (list (list (node-value node)))
    (mapcar 
     (lambda (path)
       (cons (node-value node) path))
       (foldr #'append 
              (mapcar #'tree-maximal-paths (node-children node))
              ;kazdy podstrom cesta
nil))))
(defun tree-height-4 (node)
  (if (null (node-children node)) 0
    (1+ (tree-height-4-hlp (cdr (node-children node))))))
(defun tree-height-4-hlp (subtrees)
  (if (null subtrees) 0
    (max (tree-height-4 (car subtrees))
         (tree-height-4-hlp (cdr subtrees)))))
(defun treeh (node)
  (if (null (node-children node))
      0
    (1+ (treehch (node-children node)))))
(defun treech (children)
  (if (null children) nil
    (max (treeh (car children))
         (treehch (cdr children)))))
(defun struc-to-list1 (struc)
  (if (atom struc)
      (list struc)
    (append22 (struc-to-list1 (car struc))
            (struc-to-list1 (cdr struc)))))
(defun append22(l1 l2)
  (if (null l1) l2
    (cons (car l1) (append22 (cdr l1) l2))))

(defun struc-to-list2 (struc)
  (labels ((iter (struc res)
             (if (atom struc)
                 (cons struc res)
               (iter (car struc)
                     (iter (cdr struc) res)))))
    (iter struc'())))
(defun cpst (struct)
  (cond ((atom struct) struct)
        ((and (atom (car struct)) (atom (cdr struct))) (cons (car struct) (cdr struct)))
        (t (cons (cpst (car struct))
                 (cpst (cdr struct))))))

(defun cps2 (struct)
  (if (atom struct) struct
    (cons (cps2 (car struct))
          (cps2 (cdr struct)))))
(defun mxp (node)
  (if (null (node-children node)) (list (list (node-value node)));list
        (let* ((res (mapcar #'mxp (node-children node)));naletovane listy
               (res2 (foldr #'append res '())));spojene do seznamu 
          (mapcar (lambda (path) (cons (node-value node) path)) res2))));pridavam rodice

(defun trh (node)
  (if (null (node-children node)) 0
    (1+ (trhl (node-children node)))))

(defun trhl (children)
  (if (null children) 0
    (max (trh (car children))
         (trhl (cdr children)))))
(defun trh2 (node)
  (if (null (node-children node)) 0
    (let* ((res (mapcar #'trh2 (node-children node)))
          (res2 (foldr #'max res 0)))
      (1+ res2))))
