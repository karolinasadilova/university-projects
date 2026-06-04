;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP1 -- 02_excercises.lisp -- řešení úloh k cvičení 2
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
(defun my-if (a b c)
  (if a b c))

(defun <= (a b)
  (or (< a b) (= a b)))
(defun rem (a b)
  (-  a (* b (div a b))))
(defun digit (n i)
  (rem (div n (expt 10 i))10 ))
(defun power2 (n)
  (* n n))
(defun power3 (n)
  (* n n n))
(defun power4 (n)
(* n n n n))
(defun power5 (n)
  (* n n n n n))
(defun hypotenuse (leg1 leg2)
  (sqrt (+ (power2 leg1) (power2 leg2))))
(defun minusp (n)
  (> 0 n))
(defun abs (n)
  (if (> n 0) n (- n)))
(defun min (a b)
  (if (> a b) b a))
(defun max (a b)
  (if (> a b) a b))
(defun point-distance (a-x a-y b-x b-y)
  (let ((x-leg (- a-x b-x))
        (y-leg (- a-y b-y)))
    (hypotenuse x-leg y-leg)))
(defun trianglep (a b c)
  (and (> (+ a b) c)
       (> (+ b c) a)
       (> (+ a c) b)))
(defun trianglp (a b c)
  (labels ((bigger (x y)
             (> x y)))
    (and (bigger (+ a b) c)
         (bigger (+ a c) b)
         (bigger (+ b c) a))))

(defun != (a b)
  (not (= a b)))
(defun heron (a b c)
  (let ((s (/ (+ a b c) 2)))
    (sqrt (* s (- s a) (- s b) (- s c)))))
(defun heron-cart (a-x a-y b-x b-y c-x c-y)
  (let ((ac (point-distance a-x a-y c-x c-y))
        (ab (point-distance a-x a-y b-x b-y))
        (bc (point-distance b-x b-y c-x c-y)))
    (heron ab bc ac)))

;DEFUN REM & DIGITT
(defun digit2 (n i)
  (rem (div n (expt 10 i))10))
(defun rem2 (a b)
  (- a (* b (div a b))))
    
            
;___________________________________________________
(defun myif (a b c)
  (if a b c))
(defun <= (a b)
  (or (< a b)
      (= a b)))
(defun mrem (a b)
  (- a (* b (div a b))))
(defun mdigit (n d)
  (rem (div n (expt 10 d)) 10))
(defun mpow2(n)
  (* n n))
(defun mpow3(n)
  (* n(mpow2 n)))
(defun mpow4(n)
  (* n(mpow3 n)))
(defun mpow5(n)
  (* n(mpow4 n)))
(defun mhypo (a b)
  (sqrt (+ (power2 a) (power2 b))))

(defun mminusp (n)
    (< n 0))
(defun mabs (n)
  (if (plusp n)
      n
    (if (minusp n)
        (- n)
      0)))
(defun mmin (a b)
  (if (< a b) a b))
(defun mmax (a b)
  (if (> a b) a b))
(defun biggerr (a b)
(> a b))
(defun trianglepp (a b c)
  (let ((ab (+ a b))
        (bc (+ b c))
        (ac (+ a c)))
    (and (biggerr ab c)
         (biggerr ac b )
         (biggerr bc a))))
(defun heronn (a b c)
  (let ((s (/ (+ a b c) 2)))
    (sqrt (* s (- s a) (- s b) (- s c)))))
(defun heronn-cart (a-x a-y b-x b-y c-x c-y)
  (let ((c (point-distance a-x a-y b-x b-y))
        (a (point-distance b-x b-y c-x c-y))
        (b (point-distance a-x a-y c-x c-y)))
    (heronn a b c)))
