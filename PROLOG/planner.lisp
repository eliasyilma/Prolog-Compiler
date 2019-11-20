;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;								PLANNER FUNCTIONS
;						copyright (c) 2014-2015 ELIAS YILMA
;		date modified			mod. by					modifications
;		=============			=======					==============
;		12/29/2014				elias					ORIGINAL
;		12/30/2014 				ELIAS					added true-consp
;														modified bindings
;++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

(format t "~&loading theorem eval utilities.....")

(defun prove-theorem(expr1 &optional (binding-list no-bindings))
"evaluates expr1 against known facts and theorems and returns a binding list"
(cond 
;(thand expr1 expr2)
((endp expr1) nil)

((eql (car expr1) 'thand) 
(prove-conjunction (first (cdr expr1)) (second (cdr expr1)) binding-list))
;(thor expr1 expr2)
((eql (car expr1) 'thor) 
(prove-disjunction (first (cdr expr1)) (second (cdr expr1)) binding-list))
;(thnot expr1 expr2)
((eql (car expr1) 'thnot)
(modified-not (prove-theorem (cdr expr1))))

;(thprog (?x) expr1)
((eql (car expr1) 'thprog)
(prove-thprog (second expr1) (cddr expr1)))

;(thfind num expr2)
((eql (car expr1) 'thfind)
(prove-thfind (second expr1) (car (last expr1))))

;(thand expr1 expr2)
((eql (car expr1) 'thgoal)
(prove-thgoal (cdr expr1)))

((eql (car expr1) 'thassert)
(prove-thassert (second expr1)))

;evaluates lists of expressions in case they are not connected by 
;operators 
((and (not (symbolp (car expr1))) (> (length expr1) 1))
(prove-conjunction (first expr1) (rest expr1) binding-list))

((symbolp expr1) 
(prove-theorem (get 'theorem expr1)))

((= (length expr1) 1) (prove-theorem (car expr1)))

;if everything fails return error
(t (format t "error::illegal expression ~&"))))



(defun prove-conjunction(expr1 expr2 &optional binding-list)
"proves conjunctive(thand) expressions and on the first failed fact returns
nil,else return bindings"
(cond 
((and (equal expr2 t) (equal expr1 t)) no-bindings)
((equal expr1 t) (prove-theorem expr2))
((equal expr2 t) (prove-theorem expr1))
(t (mapcan #'(lambda (bindings) 
(cond ((eql bindings nil) nil)
((equal bindings (car no-bindings)) (prove-theorem expr2))
((or (null expr1) (null expr2)) nil)
(t (let ((bind2 (prove-theorem (subst-bindings (true-consp bindings) expr2))))
(if (not (null bind2)) 
(if (eql bind2 no-bindings) bindings (list bindings bind2))))))) (mapcar #'true-consp (prove-theorem expr1))))))

(defun modified-not(x)
"a modified version of not except if x is nil,
returns ((t . t)) instead of t."
(if (null x) 
no-bindings
nil))

(defun prove-thgoal(expr1)
"if a simple fact is supplied,prove it directly.
if it fails else get the theorems supplied within the thuse
 and use them to prove the expression."
(cond 
((equal (length expr1) 1) (test (car expr1)))
((eql (car (second expr1)) 'thuse)
(let ((binds (test (car expr1))))
(if (null binds)
(prove-theorem (car (get 'theorem (second (second expr1)))))
binds)))))

(defun prove-thassert(expr)
(if (equal (car expr) 'isa)
(eval expr)
(let ((predicate (car expr))
	  (inst (second expr))
	  (value (third expr)))
(setf (get predicate inst) value)
t)))


(defun prove-disjunction(expr1 expr2 &optional binding-list)
"proves disjunctive(thand) expressions and returns
bindings if the first evaluated expr in the list succeeds"
(mapcan #'(lambda (bindings) 
(cond ((eql bindings nil) (prove-theorem expr2))
((equal bindings (car no-bindings)) (list bindings))
((and (null expr1) (null expr2)) nil)
(t (let ((bind2 (prove-theorem (subst-bindings bindings expr2))))
(if (not (null bind2)) 
(if (eql bind2 no-bindings) bindings (list bindings bind2)) (list bindings)))))) 
;so that mapcan can use the '(nil) retruned by answer instead of
;just using nil (which always retruns nil)
(let ((answer	
	(prove-theorem expr1)))
(if (equal answer nil) (list answer) answer))))

(setf *theorems* nil)

(defun deftheorem (name  &rest body)
"defines theorem and stores it within the property list of the theorem's
name"
(setf (get 'theorem name) body)
(setf *theorems* (cons name *theorems*)))



(defun prove-thprog (var-list &rest body)
"tries to evaluate the body and returns bindings only associated with
the variables in var-list"
(setf thprog-ans nil)
(let ((x (prove-theorem body)))
(cond ((equal x no-bindings)
(setf thprog-ans x))
((= (length (true-consp x)) 1) (format t "err")(setf thprog-ans 
(remove-if #'(lambda (binding)
(equal (subst-bindings2 (true-consp binding) var-list) var-list))  
(mapcar #'true-consp x))))


(t (format t "err")(loop for x-item in x do
(setf thprog-ans (cons  (remove-if #'(lambda (binding)
(equal (subst-bindings2 (true-consp binding) var-list) var-list))  
(if (symbolp (car x-item)) (true-consp x-item) x-item))
thprog-ans))))))
(remove nil (remove-duplicates thprog-ans :test #'equal)))

(defun subst-bindings2(x y)
"same function as subst-bindings but 
used for tracing binding substitutions easily"
(subst-bindings x y))


(defun true-consp(x)
(if (symbolp (car x)) (list x)
(true-consp (car x))))

(defun find-few(x expr1)
"get x number of solutions for simple facts"
(eval `(?#- ,x ,expr1))
ans)

(defun prove-thfind(num-arg expr1)
"finds num-arg number of solutions for complex expressions, 
if num-arg=0 then it will find all solutions for expr1"
(prog (x y z a b ans)
(setq x num-arg)
(cond 
((equal x 0) (setf ans (prove-theorem expr1))(go display1))

((and (> (length expr1) 1) (not (symbolp (car expr1))))
;like thand or thor
(setq y (prove-theorem (car expr1))))
((member (car expr1) '(thor thand thprog thfind thgoal)) 
(setq y (prove-theorem expr1))
;this if tests if the num of solutions for the first 
;expr is greater than the req'd number. if not return nil,
;otherwise continue.
(if (< (length y) x) (go display2) (return y)))
(t (setq y (find-few x expr1))
;this if tests if the num of solutions for the first 
;expr is greater than the req'd number. if not return nil,
;otherwise continue. 
(if (< (length y) x) (go display2) (return y))))
;initialize a 
(setq a 1)
;this if tests if the num of solutions for the first 
;expr is greater than the req'd number. if not return nil,
;otherwise continue.
(if (> x (length y))
(go display2))
loop1 
	;try the nth solution of the first expr in expr1
	(setq b (nth (- a 1) y))
	;if we've reached the end of y,then test if the number of actual 
	;solutions found are equal or greater than the number of sols req'd
	;if so display ans else display nil
	(if (null b)
		(if (>= (length ans) x) 
			(go display1) (go display2))
	
	(setq z (prove-theorem (subst-bindings b (cdr expr1)))))
	(cond ((null z) 	
	;if the rest of the expression evaluates nil for the nth solution
	;of the first expression,then increment a by 1 and loop
	(setq a (+ 1 a)) (go loop1) )
	;otherwise cons the current solution to ans,which is the 
	;list of solutions found so far
	(t (setq ans (cons b ans))))
	(if (>= a x)
	;if num of iterations exceeds the number of sol req'd
	;goto displaying the result
	(go display1))
	;otherwise increment a by 1 and loop
	(setq a (+ 1 a))
(go loop1)
display1 (return ans)
display2 (return nil)))



(defun test(expr)
"proves expr using indexed facts and returns is nil
if nothing matches"
(let ((pred (first expr))
(arg1 (if (symbolp (second expr)) (second expr) (eval  (second expr))))  
(arg2 (if (symbolp (third expr)) (third expr)   (eval  (third expr)))))
(if (null (third expr))
(eval (cons '!- (list (list pred arg1))))
(eval (cons '!- (list (list pred arg1 arg2)))))
)ans)  


(defun do-thassert(fact)
"indexes the fact and stores it in the database,
and if there are antecedent theorems for the fact,
asserts the facts with in the theorem"
(if (not (member (car fact) *theorems*))
(eval `(f ,fact))
(let ((potential-antecedent (get 'theorem (car fact))))
(if (equal (car potential-antecedent) 'thante)
(let ((assertions (subst-bindings 
(unify fact (second potential-antecedent)) (cddr potential-antecedent))))
(loop for facts in assertions do
(eval facts))
(eval `(f ,fact)))
(eval `(f ,fact))
)))) 

(defmacro thassert(fact)
"a top-level for do-thassert"
`(do-thassert ',fact))

(defun simplify-thand2(expr1 expr2)
(cond
((equal expr1 t) expr2)
((equal expr2 t) expr1)
((and (equal expr1 t) (equal expr1 t)) t)
(t (list 'thand expr1 expr2)))) 

;define threfer
;define thcreate
(defun entity-p(x)
"CHECK IF X STARTS WITH ?"
(if (and (symbolp x) (equal (char (symbol-name x) 0) #\$))
x))

(setf *properties* '($length $size $width $mass $height))
(defun property-p(x)
(member x *properties*))


(defun prove-thfind(num-arg expr)
(prog (solution expr1 arg conj1)
(setq expr1 expr)
(setq solution nil)
	(setq counter 1)
(setq conj1 nil)
;dispatch table for the number argument
(cond 
((numberp num-arg) (setq arg 'num) (go num))
((equal num-arg 'all) (go all))
((and (listp num-arg) (equal (car num-arg) '>)) (setq arg 'least)(setq num-arg (second num-arg)))
((and (listp num-arg) (equal (car num-arg) '<)) (setq arg 'most)(setq num-arg (second num-arg)))
((and (listp num-arg) (equal (car num-arg) 'exactly)) (setq arg 'exact)(setq num-arg (second num-arg))))

;planner primitive handling
NUM
(cond ((equal (car expr1) 'thgoal) 
(and (setq solution (find-few num-arg (second expr1))) 
	 (go ret)))
((equal (car expr1) 'thand)
(setq conj1 (prove-theorem (second expr1)))
(go loop1))
((equal (car expr1) 'thor)
(setq conj1 (prove-theorem (second expr1)))
(cond ((>= (length conj1) num-arg)
(and (setq solution (find-few num-arg (second (second expr1))))
(go ret)))
(t (setq expr1 (third expr1)) (go num))))
((equal (car expr1) 'thprog)
(setq conj1 (prove-theorem (third expr1)))
(setq expr1 (member (third expr1) expr1 :test #'equal))
(go loop1))
)


;this loop will try to find num no of solutions and either succeeds
;fails based on the type of number argument it is given.
loop1 

	;try the nth solution of the first expr in expr1
		;if we've reached the end of conj1,then test if the number of actual 
	;solutions found are equal or greater than the number of sols req'd
	;if so display ans else display nil
	
	(setq sol-tobe-tested (nth (- counter 1) conj1))
(if (null sol-tobe-tested)
		(if (= (length solution) num-arg) 
(cond ((equal arg 'num) (go ret))
	  ((equal arg 'least) (go ret-least))
	  ((equal arg 'most)  (go ret-most))
	  ((equal arg 'exact) (go ret-exact)))
	  (go fail)))
	(setq proof (prove-theorem (subst-bindings sol-tobe-tested (cdr expr1))))
	(cond ((null proof) 	
	;if the rest of the expression evaluates nil for the nth solution
	;of the first expression,then increment a by 1 and go back and 
	;check the rest of the solutions
	(setq counter (+ 1 counter)) (go loop1))
	;otherwise cons the current solution to ans,which is the 
	;list of solutions found so far
	(t (setq solution (cons sol-tobe-tested solution))
(if (= (length solution) num-arg)
(cond ((equal arg 'num) (go ret))
	  ((equal arg 'least) (go ret-least))
	  ((equal arg 'most)  (go ret-most))
	  ((equal arg 'exact) (go ret-exact)))
(and (setq counter (+ 1 counter)) (go loop1)))))
ALL
;find all the solutions to the expression
(setq solution (prove-theorem expr))
(go ret)


RET-LEAST
;after finding num-arg solutions, try to find another one.
;if you succeed in finding one the the whole  goal succeeds
;because there are at least num-arg+1 number of solutions.
(if (and 	(setq sol-tobe-tested (nth (- counter 1) conj1))
			(not sol-tobe-tested)
		(setq proof (prove-theorem (subst-bindings sol-tobe-tested (cdr expr1)))))
(go ret)
(go fail))

RET-MOST
;after finding num-arg solutions, try to find another one.
;if you fail in finding one then the whole  goal succeeds
;because there are  no further solutions to the expr.
(if (and 	(setq sol-tobe-tested (nth (- counter 1) conj1))
			(not sol-tobe-tested)
		(setq proof (prove-theorem (subst-bindings sol-tobe-tested (cdr expr1)))))
(go fail)
(go ret))			


RET-EXACT
;after finding num-arg solutions, try to find another one.
;if you fail in finding one then the whole  goal succeeds
;because there are exactly num-arg number of solutions.
(if (and 	(setq sol-tobe-tested (nth (- counter 1) conj1))
			(not  sol-tobe-tested)
		(setq proof (prove-theorem (subst-bindings sol-tobe-tested (cdr expr1)))))
(go fail)
(go ret))
FAIL (return nil)

RET (return solution)
))
