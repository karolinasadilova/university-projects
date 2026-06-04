;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 09_lisp_exercises.lisp -- řešení úloh z cvičení 9 -- Lisp
;;;


;; Testy lazy-scheme

#|
(ls-reduce 'a)
(ls-reduce '(a b))
(ls-reduce '((λ (a) (a b)) c))
(ls-reduce '((λ (a) (a b)) a))
(ls-reduce '((λ (a) (a b)) (a b)))
(ls-reduce '((λ (a b) (a b)) u v))

;; přetížená aplikace
(ls-reduce '((λ (a b) (a b)) u v w))
(ls-reduce '((λ (a b) (a b)) u v w c))

;; parciální aplikace a currying
(ls-reduce '((λ (a b) (a b)) u))
(ls-reduce '((λ (a b c) (c a b)) u v))

;; s přejmenováním
(ls-reduce '((λ (a b) (a b)) (a b)))


;; Bylo nutné zredukovat hlavu, ale výsledek (u u) není abstrakce,
;; takže další redukce se neprovedla:
(ls-reduce '(((λ (a) (a a)) u)
             ((λ (a) (a a)) u)))


|#

;; Testy logických hodnot a větvení

#|
(ls-eval 'true)
(ls-eval 'false)
(ls-eval 'if)

;; Větvení
;; if jsme definovali jako funkci!
(ls-eval '(if true a b))
(ls-eval '(if false a b))

|#

;; Externí hodnoty (lispové funkce). Názvy přidáváme
;; funkcí ls-add-name
;; Funkce balíme do abstrakcí, aby fungoval currying
;; (později vyzkoušíme)
(ls-add-name '+ `(λ (x y) (,#'+ x y)))
(ls-add-name '- `(λ (x y) (,#'- x y)))
(ls-add-name '* `(λ (x y) (,#'* x y)))
(ls-add-name '/ `(λ (x y) (,#'/ x y)))
;; Takto složitě, protože predikát musí vracet naše true a false,
;; nikoli lispové t a nil:
(ls-add-name '= `(lambda (x y)
                   (,(lambda (x y tr fa)
                       (if (= x y) tr fa))
                    x
                    y
                    true
                    false)))
(ls-add-name '> `(lambda (x y)
                   (,(lambda (x y tr fa)
                       (if (> x y) tr fa)
                       x
                       y
                       true 
                       false))))

#|
(ls-eval '(+ 1 2))
;; currying, parciální aplikace
(ls-eval '(+ 1))
(ls-eval '((+ 1) 2))
;; aplikace funkce na více argumentů, než kolik má parametrů
;; ("přetížená aplikace")
(ls-eval '(+ 1 2 3))
;; demonstrace líného vyhodnocování:
(ls-eval '(if (= 1 1) (* 1 0) (/ 1 0)))

|#

;;1:
#|
a)x
b)(x y)
c)(λ (x) x)
d)(λ (x) u)
e)(u (λ (x) x))
f)u
g)(u v)
h)(λ (y) (y u))
i)(v u)
j)(v u)
k)(λ (a) v)
l)(λ (B) (U V))
m)(u u)
n)(λ (Y) (Y ((λ (Z) (Z Z)) U)))
o)(λ (Z) Z)



;2:
a)(λ (x) (+ x 1))
(lambda (a b) (#'+ a b)
  ((lambda (a) (lambda (b) (#'+ a b))) x 1)
 
     ...(λ (x) (+ x 1))
                   
b)(+ u)
   ...(+ u)
(lambda (a b) (#'+ a b))
((lambda (a) (lambda (b) (#'+ a b))) u)...[a:-u]

c)(+ u v)
(lambda (a b) (#'+ a b))
((lambda (a) (lambda (b) (#'+ a b))) u v)
[a:-u; b:-v]
...ERROR(+ u v)
...u&&v not numberp


;3:
definujte op >



(ls-add-name '> `(λ (x y) (,#'> x y)))...popsane nahore

;4:
((λ (fib) (fib 20 0 1))
 (λ (n a0 a1)
    (if (= n 0)
        a0
      (fib (- n 1) a1 (+ a0 a1)))))

((lambda (n a0 a1)
  (if (= n 0)
        a0
      (fib (- n 1) a1 (+ a0 a1)))) 20 0 1)

=...(lambda (a b) (#'= a b))
+...(lambda (a b) (#'+ a b))
-...(lambda (a b) (#'- a b))


  (if (= 20 0)
      0
    (fib (- 20 1) 1 (+ 0 1)))

(if false 0 (fib (- 20 1) 1 (+ 0 1)))
...
if
(lambda (test a b) (test a b))
true: (lambda (a b) a)
false: (lambda (a b) b)


5.Pomocí operace if definujte v našem jazyce operaci not na výpočet negace:
|#
(lambda (a) (if a false true)
#|

6.Udělejte to znovu co nejjednodušeji bez operace if (stačí jedna abstrakce):
|#
(lambda (a) (a false true))
#|
7.Vede vyhodnocování podle normálního a aplikativního modelu vždy ke stej-
ným výsledkům?

normalni~beta redukce, zkracene vyhodnocovani...line, pocita argumenty az je potrebuje, nevyhodnocuje zbytecne argumenty


aplikacni~vyhodnoti vsechny args najednou a az potom aplikuje funkci takze to muze vest k cykleni nebo chybam

takkkze ne:))

|#



