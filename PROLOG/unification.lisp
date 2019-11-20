;************************UNIFICATION********************************
			;directly adopted from peter norvig's 
;"paradigms of artificial intelligence :case studies in common lisp"
;*******************************************************************

(defconstant fail nil)

(defconstant no-bindings '((t . t)))

(defun variable-p(x)
"CHECK IF SYMBOL IS A VARIABLE"
(and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun dummy-variable-p(x)
"CHECK IF SYMBOL IS A DUMMY VARIABLE"
(and (symbolp x) (equal (char (symbol-name x) 0) #\_)))

(defun get-binding(var bindings)
"GET A BINDING ASSOCIATED WITH VAR IN A CONSED DATA LIST"
(assoc var bindings))

(defun binding-val(binding)
"GET THE VALUE OR RIGHT SIDE OF VARIABLE"
(cdr binding))

(defun lookup (var bindings)
"GET THE BINDING VALUE DIRECTLY"
(binding-val (get-binding var bindings)))

(defun extend-bindings(var val bindings)
"APPEND A LIST OF BINDINGS"
(cons (cons var val)
(if (and (eq bindings no-bindings))
nil bindings)))


(defun match-variable (var input bindings) 
(let ((binding (get-binding var bindings)))
(cond ((not binding) (extend-bindings var input bindings))
((equal input (binding-val binding)) bindings)
(t fail))))

(defun unify(x y &optional (bindings no-bindings))
"UNIFIES TWO EXPRESSIONS AND RETURNS TRUE IF THERE 
IS ARE VARIABLES TO BE BOUND (TRY (UNIFY '(ELIAS ?X) '(?Y PROGRAMS)))"  
(cond ((eql x y) bindings)
((eq bindings fail) fail)
((variable-p x) (unify-variable x y bindings))
((variable-p y) (unify-variable y x bindings))
((and (consp x) (consp y))
(unify (rest x) (rest y)
(unify (first x) (first y) bindings)))
(t fail)))

(defun unify-variable (var x bindings)
"UNIFIES VARIABLES TO THEIR RESPECTIVE VALUES AND CREATES A BINDING LIST 
EACH TIME IT ENCOUNTERS A VARIABLE. HOWEVER IF THE VAR IS ALREADY BOUND 
THEN IT WILL RETURN THE ORIGINAL BINDING LIST" 
(cond ((get-binding var bindings)
(unify (lookup var bindings) x bindings))
((and (variable-p x) (get-binding x bindings))
(unify var (lookup x bindings) bindings))
((and *occurs-check* (occurs-check var x bindings)) fail)
(t (extend-bindings var x bindings))))

(defconstant *occurs-check* t)

(defun occurs-check (var x bindings)
"so that as to allow or not allow self-referencing/singleton variables"
(cond ((eq var x) t)
((and (variable-p x) (get-binding x bindings))
(occurs-check var (lookup x bindings) bindings))
((consp x) (or (occurs-check var (first x) bindings)
(occurs-check var (rest x) bindings)))
(t nil)))

(defun subst-bindings (bindings x)
  "Substitute the value of variables in bindings into x,
  taking recursively bound variables into account."
  (cond ((eq bindings fail) fail)
        ((eq bindings no-bindings) x)
        ((and (variable-p x) (get-binding x bindings))
         (subst-bindings bindings (lookup x bindings)))
        ((atom x) x)
        (t (reuse-cons (subst-bindings bindings (car x))
                       (subst-bindings bindings (cdr x))
                       x))))
