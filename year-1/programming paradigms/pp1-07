;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;; PP1 -- 07_excercises.lisp -- řešení úloh k cvičení 7
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

(defun f (symbol)
  (list symbol))
(defun fact-tail (n)
  (labels ((iter (n acc)
             (if (= n 0) acc
               (iter (- n 1) (* n acc)))))
    (iter n 1)))
(defun test-rest (a b &rest letters)
  (list a b letters))
(defun my-append (&rest lists)
  (labels ((app-2 (l1 l2)
             (if (null l1)
                 l2
               (cons (car l1) (app-2 (cdr l1) l2))))
           (app (lists)
             (if (null lists) '()
               (app-2 (car lists) (app (cdr lists))))))
    (app lists)))
(defun my-append-repte (&rest lists)
  (labels ((app-2 (l1 l2)
             (if (null l1)
                 l2
               (cons (car l1) (app-2 (cdr l1) l2))))
           (app (lists)
             (if (null lists) '()
               (app-2 (car lists) (app (cdr lists))))))
    (app lists)))
;______
;(sqrt (- (power2 (cos x)) (power2 (sin x))))
;(power2 (cos x)) = cos²x

(defun sum-d (num)
  (sum-dh num (1- (digit-count num))))
(defun sum-dh (num pt)
  (cond ((< num 10) n)
        ((< pt 0) 0)
        (t (+ (digit num pt) (sum-dh num (1- pt))))))
(defun 1- (n)
  (- n 1))
(defun appp2 (l1 l2)
  (if (null l1) l2
    (cons (car l1) (appp2 (cdr l1) l2))))
(defun ltn (list)
  (labels ((iter (acc list)
             (if (null list) acc
                   (iter  (+ (car list) (* 10 acc)) (cdr list)))))
    (iter 0 list)))
(defun deeps (list pred)
  (cond ((null list) 0)
        ((funcall pred (car list)) (1+ (deepsh (cdr list))))
        (t (deepsh (cdr list)))))
(defun deepsh (sublist)
  (if (null sublist) 0
        (+ (deeps (car sublist))
           (deepsh (cdr sublist)))))
(defun blabla (x)
  (if (consp x)
      (cons (blabla (cdr x))
            (blabla (car x)))
    x))
(defun ltwon (list)
  (if (null (cdr list)) nil
    (cons (cdr list)
          (ltwon (cdr list)))))
