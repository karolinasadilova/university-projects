;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; PP2 -- 08_lastovickova_exercises.lisp -- řešení úloh k automatům
;;;


;finite automata:

;automaton = {states}, alphabet, next-state function, start, {final states}

;constructor
(defun make-automaton (states alphabet transitions start final-states)
  (list states alphabet transitions start final-states))



;transitions '(state symbol new-state)


;selectors
(defun states (automaten)
  (first automaten))

(defun alphabet (automaten)
  (second automaten))

(defun transitions (automaten) 
  (third automaten))
  
(defun start (automaten)
  (fourth automaten))

(defun final-states (automaten)
  (fifth automaten))

;fnc 4 transitions
;representing transitions '(state symbol new-state)

(defun next-state (automaton state symbol)
  (labels((iter (transitions state symbol)
            (if (null transitions) nil
              (let ((trans1 (first transitions)))
                (if (and (eql state (first trans1))
                         (eql symbol (second trans1)))
                    (third trans1)
                  (iter (cdr transitions) state symbol))))))
    (iter (transitions automaton) state symbol)))

;string representing '(a a) or '()~epsilon~null string
;function 4 processing string
(defun processing-string (string automaton)
  (labels ((iter (current-state string)
             (if (null string) current-state
               (iter (cdr string)
                     (next-state automaton current-state (first string))))))
    (iter string (start automaton))))
      

(defun accept-string-p (string automaton)
 (find (processing-string string automaton) (final-states automaton)))


;fnc 4 creating strings


(defun string-of-len (alphabet len)
;list of epsilon && epsilon ist '()
  (cond ((zerop len) '(()))
        (t (let ((rest (string-of-len alphabet (1- len))))
;must be appended cuz (((A (A)) (A (B))) ((B (A)) (B (B))))->(A A) (A B) (B A) (B B))
             (apply #'append 
                    (mapcar (lambda (symbol) (add-to-all symbol rest)) alphabet))))))

(defun add-to-all (symbol strings)
  (mapcar (lambda (string) (cons symbol string)) strings))


(defun stream-alphabet (alphabet)
  (labels ((iter (len strings)
             (if (null strings) (iter (1+ len) (string-of-len alphabet (1+ len)))
               (cons-stream (first strings) (iter len (cdr strings))))))
    (iter 0 (string-of-len alphabet 0))))
(defun list-to-stream (list)
  (if (null list) nil
    (cons-stream (car list) (list-to-stream (cdr list)))))

(defun accept-stream (stream automaton)
  (labels ((iter (stream)
             (let ((first (stream-car stream)))
             (if (accept-string-p first automaton)
                 (cons first
                       (iter (stream-cdr stream)))
               (iter (stream-cdr stream))))))
    (iter stream))))
