
(defun index2(query)
"stores the query in the property-list of its arguments"
(let ((args (first query)))
(loop for arg in args do
(cache-facts (position arg args) arg args)
))
t)

(defun cache-facts(pos atm args)
"cache-facts first looks up the property list of atm for the given pos
and if an array already exists then pushes args into the vector.
if nothing exists, then it will build a new vector and pushes the args
into the newly built vector"
(let ((pos-name (new-symbol "i" pos)))
(let ((db (get pos-name atm)))
(if db
 (vector-push-extend args db)
(and (setf (get pos-name atm) (make-array 20 :fill-pointer 0 :adjustable t)) 
	   (vector-push-extend args (get pos-name atm))))
)))

(defun fetch2(query  &optional  (n 0)(fact-list (make-array 20 :fill-pointer 0 :adjustable t)))
"returns all the facts indexed under each atom of the query 
for the position of the atom
i.e. if the query is (isa a b), it will look for
a vector under isa at 0-position, under a at 1-position
and under b at 2nd-position"
(cond 
((endp query) (remove nil (remove-duplicates fact-list :test #'equal)))
(t 
(let ((indexed-facts (get (new-symbol "i" n) (first query))))
(loop for i from 0 to (1- (length indexed-facts)) do
(vector-push-extend (elt indexed-facts i) fact-list))) 

(fetch2 (rest query)  (+ 1 n) fact-list)
))) 

(defun fetch-em2(query)
"1. loads the data-base file for the predicate only
(i.e. instead of loading the whole database 
(which is to be filled with 100s of thousands of facts 
and detail),it uses only stuff that's relevant)
 2. uses fetch to get all the values under the arguments
 3. defines all the fetched facts"
;clear-db should be clear-index
;for memory conservation and to avoid confusing the compiler..
;(clear-db)
;(setq db-file (concatenate 'string "db/" (string (car query)) ".lisp"))
;(load db-file)
(let ((fact-list (fetch2 (rest query)  1)))
(cond ((null fact-list) nil)
(t (loop for i from 0 to (1- (length fact-list))  do 
		(eval `(f ,(elt fact-list i))))))))

(defmacro i- (&rest clause)
"indexing macro , (used instead of 'assert' or 'f')"
`(index2 ',clause))