;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
;A PROLOG COMPILER BASED ON PETER NORVIG'S PAIP BOOK.
;copyright (c) 2014,2015 elias yilma
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



(defun compile-predicate(name arity)
"COMPILES A PREDICATE WITH A GIVEN ARITY AND GIVES A FUNCTION OF THE FORM:
		INPUT => (LIKES ELIAS CATS) 
		OUTPUT => (DEFUN LIKES/2(?ARG1 ?ARG2 CONT) (setf binds bin-val FUNCTION-BODY)"
(let ((clauses (get-all name arity)) 
(predicate (make-pred name arity)) 
(parameters (generate-parameter arity)))
(eval `(defun ,predicate (,@parameters bin-val cont)
(catch 'eli 
(cond
((eql (get 'cont-val 'c) nil)
(setf binds bin-val).,(undo-bindings
(mapcar #'(lambda (clause) 
(compile-clause clause))
clauses)))
 (t nil)))))))

(defun compile-predicate2(name arity)
"COMPILES A PREDICATE WITH A GIVEN ARITY AND GIVES A FUNCTION OF THE FORM:
		INPUT => (LIKES ELIAS CATS) 
		OUTPUT => (DEFUN LIKES/2(?ARG1 ?ARG2 CONT) (setf binds bin-val FUNCTION-BODY)"
(let ((clauses (get-facts name)) 
(predicate (make-pred name arity)) 
(parameters (generate-parameter arity)))
(eval `(defun ,predicate (,@parameters bin-val cont)
(catch 'eli 
(cond
((eql (get 'cont-val 'c) nil)
(setf binds bin-val).,(undo-bindings
(map 'list #'(lambda (clause) 
(compile-clause clause))
clauses)))
 (t nil)))))))

(defun compile-all()
	(loop for predicate in *predicates* do
	;	(loop for i from 0 to (- (length (get-all predicate)) 1) do
			(compile-predicate2 predicate (arity (car (elt (get-all predicate) 0))))
))

 (defun compile-head(clause i)
"COMPILES A SINGLE CLAUSE/STATEMENT 
INPUT => (LIKES ELIAS CATS) OUTPUT => (IF (UNIFY ARG1 'ELIAS) (IF (UNIFY ARG2 'CATS) CONT))"
(if (null (args clause)) 'cont 
`(if ,(unify-arg (generate-arg i) `(,@(compile-arg (first (args clause))))) 
,(compile-head (args clause) (+ i 1))))) 


(defun compile-rule(rule)
"COMPILES A SINGLE RULE, I.E. WITH MULTIPILE CLAUSES"
(deep-replace  (compile-body (body rule)) 'cont (compile-head (head rule) 0)))

(defun compile-fact(clause i)
"COMPILES A SINGLE FACT 
INPUT => (LIKES ELIAS CATS) OUTPUT => (IF (UNIFY ARG1 'ELIAS) (IF (UNIFY ARG2 'CATS) (FUNCALL CONT)))"
(if (null (args clause)) '(funcall cont binds) 
`(if ,(unify-arg (generate-arg i) (compile-arg (first (args clause)))) 
,(compile-fact (args clause) (+ i 1))))) 


(defun compile-clause(clause/s)
"IF INPUT IS A FACT , USE COMPILE-FACT ;ELSE USE COMPILE RULE"
(let ((params (generate-parameter (arity (first clause/s))))) 
(if (= (length clause/s) 1) (bind-vars params (compile-fact (first clause/s) 0))
(bind-vars params (compile-rule clause/s)))))

(defun bind-vars(param expr)
"create let forms for binding variables contained within variables"
(let ((vars (remove nil (set-difference (vars-in-expr expr) param)))) 
(if (not (null vars))
`(let ,(mapcar #'(lambda (var) `(,var (create-var ',var)))
vars) ,expr) expr)))


(defun compile-body(clauses)
"COMPILES THE BODY OF THE CLAUSE"
(cond ((null clauses) nil)
(t  
` ( ,(generate-predicate (first clauses)) ,@(remove 'list (compile-arg (args (first clauses)))) binds  
,(if (null (cdr clauses)) 'cont `#'(lambda (binds) ,(compile-body (rest clauses)))))))) 

(defun compile-arg(arg)
"INPUT => (A ?X (?X . A) (MEMBER ?X A)) 
 OUTPUT=> (A ?X (CONS ?X A) (LIST 'MEMBER ?X A))"
(cond ((and (symbolp arg) (variable-p arg)) `,arg)
((and (symbolp arg) (not (variable-p arg))) `',arg)
((numberp arg) `',arg)
((proper-listp arg)  `(list ,@(mapcar #'compile-arg  arg))) 
(t `(cons ,(compile-arg (first arg)) ,(compile-arg (rest arg))))))
 						
(defun quotify(args-list)
"adds the quote (') sign to every argument in the top-level function"
`(quote ,args-list))


(defun has-var-p (arg)
(cond ((null arg ) nil)
((variable-p arg) t)
((symbolp arg) nil)
((variable-p (first arg)) t)
((and (symbolp (first arg)) (not (variable-p (first arg)))) (has-var-p (rest arg)))
(t (or (has-var-p (first arg)) (has-var-p (rest arg))))))


(defun undo-bindings(exps)
(if (eql (length exps) 1) exps
`(
,(first exps)
,@(loop for exp in (rest exps)
collect '(setf binds bin-val)
collect  exp))))

(defmacro !- (args)
"CONSTRUCTS AN EXPRESSION AND EVALUATES THE EXPRESSION, WHICH
IS COMPOSED OF AN ALREADY DEFINED PREDICATE FUNCTION AND ITS ARGUMENTS"
(clear-db)
(fetch-em2 args)
(compile-all)
(setf ans nil)
(setf the-args (mapcar #'quotify (cdr args)))
(let ((vari (eval `(catch-vari2 ,@the-args))))
(loop for var in vari do
(setf (get 'list-ans var) nil)
))
(setf binds no-bindings)
(catch 'eli (setf bindings binds)
`(,(new-symbol (car args) '/ (length (cdr args))) ,@(mapcar #'quotify (replace---vars (cdr args))) no-bindings 'show-prolog-vars2)
))

(defmacro !2- (args)
"CONSTRUCTS AN EXPRESSION AND EVALUATES THE EXPRESSION, WHICH
IS COMPOSED OF AN ALREADY DEFINED PREDICATE FUNCTION AND ITS ARGUMENTS(includes indexing)"
(fetch-em args)
(compile-all)
(setf ans nil)
(setf the-args (mapcar #'quotify (cdr args)))
(let ((vari (eval `(catch-vari2 ,@the-args))))
(loop for var in vari do
(setf (get 'list-ans var) nil)
))
(setf binds no-bindings)
(catch 'eli (setf bindings binds)
`(,(new-symbol (car args) '/ (length (cdr args))) ,@(mapcar #'quotify (replace---vars (cdr args))) no-bindings 'show-prolog-vars2)
))

(defmacro ?- (args)
"CONSTRUCTS AN EXPRESSION AND EVALUATES THE EXPRESSION, WHICH
IS COMPOSED OF AN ALREADY DEFINED PREDICATE FUNCTION AND ITS ARGUMENTS"
`(compile-pred ,(predicate args) ,(arity args))
(loop for i from 1 to (length *ans*) do
(vector-pop *ans*))
(loop for i from 1 to (length *q-ans*) do
(vector-pop *q-ans*))
(setf the-args (mapcar #'quotify (cdr args)))
(setf binds no-bindings)
(setf bindings binds)
 `(,(new-symbol (car args) '/ (length (cdr args))) ,@(mapcar #'quotify (replace---vars (cdr args))) no-bindings 'show-prolog-vars)
)

(defmacro ?#- (num args)
"CONSTRUCTS AN EXPRESSION AND EVALUATES THE EXPRESSION, WHICH
IS COMPOSED OF AN ALREADY DEFINED PREDICATE FUNCTION AND ITS ARGUMENTS"
(setf i 0)
(setf ans nil)
(setf num-of-sol num)
(loop for i from 1 to (length *ans*) do
(vector-pop *ans*))
(loop for i from 1 to (length *q-ans*) do
(vector-pop *q-ans*))
(setf the-args (mapcar #'quotify (cdr args)))
(setf binds no-bindings)
(setf bindings binds)
 `(,(new-symbol (car args) '/ (length (cdr args))) ,@(mapcar #'quotify (replace---vars (cdr args))) no-bindings 'show-prolog-vars3)
)

(defvar *ans* (make-array 20 :fill-pointer 0 :adjustable t))
(defvar *q-ans* (make-array 20 :fill-pointer 0 :adjustable t))

(defun show-prolog-vars(bind-type)
"DISPLAYS THE VALUES OF VARIABLES REQUESTED FROM THE USER
BY TAKING RECURSIVELY BOUND VARIABLES"
  (let ((vari (eval `(catch-vari2 ,@the-args))))
(cond ((null vari)
      (format t "~&Yes"))
      (t (loop for var in vari do
  ;      (format t "~&~a = ~a" var
  ;               (subst-bindings bind-type var))
 (if (listp (subst-bindings bind-type var)) 
(vector-push-extend  (subst-bindings bind-type var) *ans*)
(vector-push-extend  (subst-bindings bind-type var) *q-ans*)))))
	(setf bindings binds)
	(if t
		(setf (get 'cont-val 'c) nil)
		(throw 'eli (format t "no.")))))


(defun show-prolog-vars2(bind-type)
"DISPLAYS THE VALUES OF VARIABLES REQUESTED FROM THE USER
BY TAKING RECURSIVELY BOUND VARIABLES"
(setf a nil)
  (let ((vari (eval `(catch-vari2 ,@the-args))))

(cond ((null vari)
(setf ans no-bindings))
      (t  
 (loop for var in vari do
(setf a (cons (cons  var  (subst-bindings bind-type var)) a))
)
(setf ans (cons a ans))
ans))
	(setf bindings binds)
	(if t
		nil
		(throw 'eli "no"))))

(defun show-prolog-vars3(bind-type)
"DISPLAYS THE VALUES OF VARIABLES REQUESTED FROM THE USER
BY TAKING RECURSIVELY BOUND VARIABLES"
(setf a nil)
(setf i (+ i 1))
  (let ((vari (eval `(catch-vari2 ,@the-args))))
(cond ((null vari)
(setf ans no-bindings))
      (t  
 (loop for var in vari do
(setf a (cons (cons  var  (subst-bindings bind-type var)) a))
)
(setf ans (cons a ans))
ans))
	(setf bindings binds)
		(if (< i num-of-sol)
		(setf (get 'cont-val 'c) nil)
		(throw 'eli nil))))
;+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


(defmacro rule (&rest body)
(eval `(rule1 ,@(mapcar #'quotify body))))

(defun rule1 (head &optional (arrow ':-) &rest body)
  "Expand one of several types of logic rules into pure Prolog."
  ;; This is data-driven, dispatching on the arrow
  (eval `(funcall ',(get arrow 'rule-function) ',head 
',body)))
 
(setf (get ':- 'rule-function)
      #'(lambda (head body) `(r ,head .,body)))

(defun dcg-normal-goal-p (x) (or (starts-with x :test) (eq x '!)))

(defun dcg-word-list-p (x) (starts-with x ':word))

(defun new-symbol(&rest args)
"CREATES A NEW SYMBOL BY JOINING THE VALUES OF THE ARGS"
(intern (format nil "~{~a~}" args)))


(setf (get '--> 'rule-function) 'make-dcg)


(defun make-dcg (head body)
  (let ((n (count-if (complement #'dcg-normal-goal-p) body)))
    `(r (,@head ?s0 ,(new-symbol '?s n))
         .,(make-dcg-body body 0))))

(defun =/2 (?arg1 ?arg2 bin-val cont)
(setf binds bin-val)
(if (unify+ ?arg1 ?arg2)
(funcall cont binds)))

(defun make-dcg-body (body n)
  "Make the body of a Definite Clause Grammar (DCG) clause.
  Add ?string-in and -out variables to each constituent.
  Goals like (:test goal) are ordinary Prolog goals,
  and goals like (:word hello) are literal words to be parsed."
  (if (null body)
      nil
      (let ((goal (first body)))
        (cond
          ((eq goal '!) (cons '! (make-dcg-body (rest body) n)))
          ((dcg-normal-goal-p goal)
           (append (rest goal)
                   (make-dcg-body (rest body) n)))
          ((dcg-word-list-p goal)
           (cons
             `(= ,(new-symbol '?s n)
                 (,@(rest goal) .,(new-symbol '?s (+ n 1))))
             (make-dcg-body (rest body) (+ n 1))))
          (t (cons
               (append goal
                       (list (new-symbol '?s n)
                             (new-symbol '?s (+ n 1))))
               (make-dcg-body (rest body) (+ n 1))))))))


