
(defun head (clause) (first clause))
(defun body (clause) (rest clause)) 

(defun get-facts(pred)
(get pred 'facts))

(defun get-rules(pred)
(get pred 'rules))

(defun get-all (pred)
"GET ALL FACTS AND RULES FOR A GIVEN PREDICATE WITHA SPECIFIC ARITY"
(get pred 'facts))

(defun predicate (fact)
(first fact))

(setf *predicates* '())


(defun add-facts(fact)
"ADDS FACTS TO THE 'FACTS' DATABASE"
(let ((fact1 (head fact)))
(if (null (get (predicate fact1) 'facts))
(setf (get (predicate fact1) 'facts) 
(make-array 20 :fill-pointer 0 :adjustable t)))
(pushnew  (predicate fact1) *predicates*)
(prog (x)
(setq x (get-facts (predicate fact1)))
(vector-push-extend fact x)
(setf (get (predicate fact1) 'facts) x))
 (predicate fact1)))

(defun compile-pred(pred arity)
(compile-predicate2 pred arity))

(defun add-rules(rule)
"ADDS RULES TO THE 'RULES' DATABASE"
(let ((rule1 (head rule)))
(pushnew (predicate rule1) *predicates*)
(setf (get (predicate rule1) 'rules) 
(nconc (get-rules (predicate rule1))  (list (remove 'and (remove 'if rule))))) 
(compile-predicate2 (predicate rule1) (arity rule1))
(predicate rule1)))

(defun clear-predicate(pred)
"CLEARS ENTRIES FOR A SINGLE PREDICATE"
(setf (get pred 'facts) (make-array 20 :fill-pointer 0 :adjustable t)))

(defun clear-db ()
  "Remove all clauses (for all predicates) from the data base."
  (mapcar #'clear-predicate *predicates*))

(defmacro f(&rest clause)
`(add-facts ',clause))

(defmacro r(&rest clause)
`(add-rules ',clause))


(defun identify(pred arity)
"IDENTIFY ALL RULES AND FACTS FOR A SINGLE PREDICATE"
(identify-clauses (get-all clauses) arity))

(defun arities(pred)
"GET ALL ARITIES FOR A SINGLE PREDICATE"
(arity (get-all pred))) 


(defun make-pred(pred arity)
"EG: LIKES 2 ==> LIKES/2"
(new-symbol pred '/ arity))

(defun generate-predicate(clause)
"LIKE MAKE-PRED ,BUT TAKES INPUT DIRECTLY FROM THE CLAUSE"
(new-symbol (predicate clause) '/ (arity clause)))

(defun generate-arg(i)
"GENERATE NEW ARGUMENT NAMES EG: ARG1 ,ARG2 ,ARG3..."
(new-symbol '?arg (+ 1 i)))

(defun generate-parameter(arity)
(loop for i from 1 to arity 
collect (new-symbol '?arg i)))

(defun unify-arg(arg1 arg2)
"COMPILE FUNCTION TO UNIFYT"
`(unify+ ,arg1 ,arg2))  

(defun args(clause)
(rest clause))


(defun identify-clauses(clauses arity)
"IDENTIFY CLAUSES WITH THE GIVEN ARITY"
(cond ((null clauses) nil)
((= (arities (head (first clauses))) arity) 
(cons (first clauses) (identify-clauses (rest clauses) arity)))
(t (identify-clauses (rest clauses) arity))))
 
(defun identify-variables(clauses)
"INDENTIFY ALL VARIABLES WITHIN THE CLAUSE/S"
(cond ((null clauses) nil)
((variable-p clauses) clauses) 
((variable-p (first clauses))  (cons (first clauses) (identify-variables (rest clauses))))
((or (numberp (first clauses)) (symbolp (first clauses))) (identify-variables (rest clauses)))
(t (cons (identify-variables (first clauses)) (identify-variables (rest clauses))))
))

(defun vars-in-expr(expr)
"REARRANGE OUTPUT FROM IDENTIFY-VARIABLES"
(remove-duplicates (flatten (identify-variables expr))))

(defun arity (clauses)
"???????????????????"
(cond ((symbolp (first clauses)) (- (length clauses) 1))
(t (remove-duplicates (flatten (mapcar #'(lambda(clause) 
	(- (length (head clause)) 1)) clauses))))))


(defun identify-entities(clause)
"AGAIN... ?????????????????????????"
(cond ((null clause)  nil)
((variable-p (first clause)) (identify-entities (rest clause)))
(t (cons (first clause) (identify-entities (rest clause))))))
