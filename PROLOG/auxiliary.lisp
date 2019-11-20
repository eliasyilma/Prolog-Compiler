;++++++++++++++++++++++++++auxiliary functions+++++++++++++++++++++++++++++++++
 

(defun proper-listp (x)
"Is x a proper (non-dotted) list ?"
(or (null x)
(and (consp x) (proper-listp (rest x)))))

(defun append-binds(the-list the-set)
"just for the sake of being here. may be important for later. 
APPENDS CONSED LISTS WITHOUT ALTERING THE SET OF THE BINDING"
(cond ((null the-list) the-set)
	  ((and (not (symbolp the-list))(not (proper-listp the-list))) (cons the-list the-set))
	  ((not (proper-listp (car the-list))) 
	(cons (car the-list) (append-binds (cdr the-list) the-set)))
	  ((consp (car the-list)) (append-binds (cdr the-list) (append-binds (car the-list) the-set)))
	(t (append-binds (cdr the-list) the-set)))) 

(defun catch-vari (&rest args)
"CATCHES VARIABLES FROM AN EXPRESSION 
INPUT => (A ?B C ?D E) : OUTPUT => (?B ?D)"
(remove-if #'(lambda(arg) (not (variable-p arg))) args))


(defun catch-vari2 (&rest args)
(let ((x (flatten (deep-remove-vars args)))) (if (equal x '(nil)) nil x)))

(defun deep-remove-vars( list1)
"A VERSION OF REMOVE THAT TAKES DEEPLY NESTED LISTS INTO ACCOUNT"
(cond
((endp list1) nil)
((and (atom (first list1)) (not (variable-p (first list1)))) (deep-remove-vars  (rest list1)))
((variable-p (first list1)) (cons (first list1) (deep-remove-vars  (rest list1))))
(t (cons (deep-remove-vars  (first list1)) (deep-remove-vars  (rest list1)))))) 

(defun continue-p()
"SHOULD THE USER CONTINUE TO GET MORE SOLUTIONS OR TERMINATE 
THE PROCESS BY RETURNING SUCCESS?" 
(case (read-char)
(#\; t)
(#\. nil)
(#\newline (continue-p))
(otherwise (format t "type ; or . ")
(continue-p))))


(defun reuse-cons (x y x-y)
"Return (cons x y) , or just x-y if it is equal to (cons x y)."
( if (and (eql x (car x-y ) ) (eql y (cdr x-y )))
x-y
(cons x y))) 

(defun extend-bindings2(val bindings)
"i have no idea why I wrote this function"
(cons val
(if (and (eq bindings no-bindings))
nil bindings)))

(defun unify+(arg1 arg2)
"UNIFIES ARG1 & ARG2 AND SETS THIS VALUE TO 'BINDS'"
(setf binds (unify arg1 arg2 binds)))

(defun append-bindings(bin-val)
"again for the sake of being here"
(if (null bindings) (setf bindings (append-binds bin-val bindings))))

(defun deep-replace(replacer replacee list1)
"A VERSION OF REPLACE THAT TAKES DEEPLY NESTED LISTS INTO ACCOUNT"
(cond
((endp list1) nil)
((equal replacee (first list1)) (cons replacer (deep-replace replacer replacee (rest list1))))
((atom (first list1)) (cons (first list1) (deep-replace replacer replacee (rest list1))))
(t (cons (deep-replace replacer replacee (first list1)) (deep-replace replacer replacee (rest list1))))))  


(defun deep-remove(item list1)
"A VERSION OF REMOVE THAT TAKES DEEPLY NESTED LISTS INTO ACCOUNT"
(cond
((endp list1) nil)
((equal item (first list1)) (deep-remove item (rest list1)))
((atom (first list1)) (cons (first list1) (deep-remove item (rest list1))))
(t (cons (deep-remove item (first list1)) (deep-remove item (rest list1)))))) 
 
(defun flatten(list)
"FLATTEN ANY TYPE OF NESTED LISTS (EXCEPT PERHAPS CONSED LISTS)"
(cond
((endp list) nil)
((atom (first list)) (cons (first list) (flatten (rest list))))
(t (append (flatten (first list)) (flatten (rest list))))))

(defun variable-p(x)
"CHECK IF X STARTS WITH ?"
(and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun new-symbol(&rest args)
"CREATES A NEW SYMBOL BY JOINING THE VALUES OF THE ARGS"
(intern (format nil "~{~a~}" args)))

(defun create-var(arg)
"CREATES A RANDOM VARIABLE (EG : ?X ==> ?X2376)"
(gensym (string arg)))

(defun starts-with(list x)
"IS X THE FIRST ELEMENT IN LIST?"
(and (consp list) (eql (first list) x)))

(defun new-symbol(&rest args)
"CREATES A NEW SYMBOL BY JOINING THE VALUES OF THE ARGS"
(intern (format nil "~{~a~}" args)))

(defun var-in-expr(expr)
"filter all varaibles from an expression"
(cond 
((endp expr) nil)
((and (atom expr) (variable-p expr)) expr)
		((and (atom (first expr))(variable-p (first expr))) (adjoin (first expr) (var-in-expr (rest expr)))) 
((and (atom (first expr)) (not (variable-p (first expr)))) (var-in-expr (rest expr)))	  
(t (adjoin (var-in-expr (first expr)) (var-in-expr (rest expr)))))) 

(defun dummy-var-in-expr(expr)
"filter and return all dummy variables (of the form _) from an expression"
(cond 
((endp expr) nil)
((and (atom expr) (dummy-variable-p expr)) expr)
((and (eql expr '()) (dummy-variable-p expr)) nil)
		((and (atom (first expr))(dummy-variable-p (first expr))) (cons (first expr) (dummy-var-in-expr (rest expr)))) 
((and (atom (first expr)) (not (dummy-variable-p (first expr)))) (dummy-var-in-expr (rest expr)))	  
(t (adjoin (dummy-var-in-expr (first expr)) (dummy-var-in-expr (rest expr)))))) 

(defun ?()
"create readable variables. eg: ?1"
(new-symbol '? r))

(defun replace-?-vars (expr)
"replace ?x123 type vars with the form ?1"
(setf r 0)
(let ((vars (flatten (var-in-expr expr))))
(loop for var in vars do
(setf expr (deep-replace (?) var expr))
(setf r (+ r 1)))
expr))

(defun replace-?x-vars (val expr)
"replace variables with val"
(let ((vars (flatten (var-in-expr expr))))
(loop for var in vars do
(setf expr (deep-replace val var expr))
)
expr))


(defun replace---vars (expr)
"replace dummy variables with the form ?123"

(let ((vars (remove 'nil (flatten (dummy-var-in-expr expr)))) (p 0))
(loop for var in vars do
(setf expr (deep-replace (new-symbol '? (+ 1 p)) var expr))
(setf p (+ 1 p)))
expr))

(defun last1 (list) 
"Return the last element (not last cons cell) of list" 
(first (last list)))


(defun maybe-add (op exps &optional if-nil) 
"For example,  (maybe-add 'and  exps t) returns 
t if exps is nil, (first exps) if there is only  one, 
and  (and expl exp2 ...I  if there are several exps." 
(cond ((null exps) if-nil) 
((length=l exps) (first exps)) 
(t (cons op exps)))) 

(defun length=l (x) 
"Is x a list of length  1?" 
(and (consp x) (null (cdr x))))

(defun partition-if (pred list) 
"Return 2 values: elements of list that satisfy pred, 
and elements that don't." 
(let ((yes-list nil) 
(no-list nil )) 
(dolist (item list) 
(if (funcall pred item) 
(push item yes-list) 
(push item no-list))) 
(values  (nreverse yes-list) (nreverse no-list))))

(defun mappend (fn the-list) 
"Apply fn to each element of list and  append the results." 
(apply #'append (mapcar  fn the-list)))

(defun first-or-nil (x) 
"The first element of x if it is a list; else nil." 
(if (consp x) (first x) nil))
