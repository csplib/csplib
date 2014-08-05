;;; QGn.m generator for SAT (adapted from code due to Mark Stickel)
;;
;; All the usual disclaimers apply
;;
;; Clauses are returned using the trie data-structure.
;; to return a more list based representation, use trie2clause. 
;; For example, (trie2clause (qg2 5)) returns clauses representing
;; an idempotent version of QG2.5. As a second example, 
;; (trie2clause (qg5 7 :not-necessarily-idempotent T) returns
;; clauses representing QG5.7 in which idempotency is not required. 

(defun trie2clause (tries)
   (cons 'and (trie2clause2 (dp-clauses tries))))

(defun trie2clause2 (tries)
   (cond ((null tries) nil)
         (t (cons (cons 'or (onetrie2oneclause (car tries)))
                  (trie2clause2 (cdr tries))))))

(defun onetrie2oneclause (trie)
   (cond ((null trie) nil)
         ((> (car trie) 0) (cons (car trie) (onetrie2oneclause (cdr trie))))
         (t (cons (list 'not (- (car trie))) (onetrie2oneclause (cdr trie))))))


#|
;; this version can be used to create variables from 1 to n^3 without gaps
(defun encode-qg-atom (n i j k &optional (number 0))
  (+ (* (+ (* (1- i) n) (1- j)) n) k))
|#

(defun encode-qg-atom (n i j k &optional (number 0))
  (+ n (* number 25) (* 100 (+ (* (+ (* (1- i) n) (1- j)) n) (1- k)))))

(defun decode-qg-atom (v)
  (let ((n (mod v 25))
	(sym (cdr (assoc (floor (mod v 100) 25) '((0 . *) (1 . *1) (2 . *2) (3 . *3)))))
	i j k)
    (setq v (floor v 100))
    (setq k (1+ (mod v n)))
    (setq v (floor v n))
    (setq j (1+ (mod v n)))
    (setq v (floor v n))
    (setq i (1+ v))
    (values `(= (,sym ,i ,j) ,k) n)))

(defvar use-row-and-column-surjectivity t)

(defun qg (v &key (number 0) not-necessarily-idempotent incomplete initial-values clause-set)
  (myassert (< v 25))
  (myassert (<= 0 number 3))
  (let ((v-n (if incomplete (- v incomplete) v)))
    (unless not-necessarily-idempotent
      (loop for i from 1 upto v-n
	    do (setq clause-set (dp-insert (list (encode-qg-atom v i i i number)) clause-set))))
    (UNLESS (> NUMBER 0)
      (loop for i from 1 upto v-n
	    do (loop for j from 1 upto v
		     when (< (1+ j) i)
		       do (setq clause-set (dp-insert (list (- (encode-qg-atom v i v j number))) clause-set)))))
    (loop for x in initial-values
	  do (setq clause-set (dp-insert (list (encode-qg-atom v (first x) (second x) (third x))) clause-set)))
    (loop for i from 1 upto v
	  do (loop for j from 1 upto v
		   unless (and (> i v-n) (> j v-n))
		     do (setq clause-set (dp-insert-sorted
					   (loop for k from 1 upto (if (or (> i v-n) (> j v-n)) v-n v)
						 collect (encode-qg-atom v i j k number))
					   clause-set))))
    (when use-row-and-column-surjectivity
      (loop for i from 1 upto v			;new clauses suggested by Fujita 2/24/93
	    do (loop for j from 1 upto v
		     unless (and (> i v-n) (> j v-n))
		       do (setq clause-set (dp-insert-sorted
					     (loop for k from 1 upto (if (or (> i v-n) (> j v-n)) v-n v)
						   collect (encode-qg-atom v i k j number))
					     clause-set))
			  (setq clause-set (dp-insert-sorted
					     (loop for k from 1 upto (if (or (> i v-n) (> j v-n)) v-n v)
						   collect (encode-qg-atom v k i j number))
					     clause-set)))))
    (loop for i from 1 upto v
	  do (loop for j from 1 upto v
		   do (loop for k1 from 1 upto v
			    do (loop for k2 from (1+ k1) upto v
				     do (unless (and (> i v-n) (> j v-n))
					  (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v i j k1 number))
										   (- (encode-qg-atom v i j k2 number)))
									     clause-set)))
					(unless (and (> i v-n) (or (> k1 v-n) (> k2 v-n)))
					  (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v i k1 j number))
										   (- (encode-qg-atom v i k2 j number)))
									     clause-set))
					  (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v k1 i j number))
										   (- (encode-qg-atom v k2 i j number)))
									     clause-set)))))))
    clause-set))

(defun qg1 (v &key not-necessarily-idempotent incomplete initial-values)
  (let ((v-n (if incomplete (- v incomplete) v))
	(clause-set (qg v :not-necessarily-idempotent not-necessarily-idempotent :incomplete incomplete :initial-values initial-values)))
    (loop for a from 1 upto v
	  do (loop for b from 1 upto v
		   unless (and (> a v-n) (> b v-n))
		     do (loop for c from 1 upto v
			      do (loop for d from 1 upto v
				       unless (and (> c v-n) (> d v-n))
					 unless (and (= a c) (= b d))
					   do (loop for ab from 1 upto v
						    do (loop for x from 1 upto v
							     unless (and (> x v-n) (or (> b v-n) (> d v-n)))
							       do (setq clause-set (dp-insert-sorted
										     (list (- (encode-qg-atom v a b ab))
											   (- (encode-qg-atom v c d ab))
											   (- (encode-qg-atom v x b a))
											   (- (encode-qg-atom v x d c)))
										     clause-set))))))))
    clause-set))

(defun qg2 (v &key not-necessarily-idempotent incomplete initial-values)
  (let ((v-n (if incomplete (- v incomplete) v))
	(clause-set (qg v :not-necessarily-idempotent not-necessarily-idempotent :incomplete incomplete :initial-values initial-values)))
    (loop for a from 1 upto v
	  do (loop for b from 1 upto v
		   unless (and (> a v-n) (> b v-n))
		     do (loop for c from 1 upto v
			      do (loop for d from 1 upto v
				       unless (and (> c v-n) (> d v-n))
					 unless (and (= a c) (= b d))
					   do (loop for ab from 1 upto v
						    do (loop for x from 1 upto v
							     unless (and (> x v-n) (or (> b v-n) (> d v-n)))
							       do (setq clause-set (dp-insert-sorted
										     (list (- (encode-qg-atom v a b ab))
											   (- (encode-qg-atom v c d ab))
											   (- (encode-qg-atom v b x a))
											   (- (encode-qg-atom v d x c)))
										     clause-set))))))))
    clause-set))

(defun qg3 (v &key not-necessarily-idempotent incomplete negative-equation-clauses initial-values)
  (let ((v-n (if incomplete (- v incomplete) v))
	(clause-set (qg v :not-necessarily-idempotent not-necessarily-idempotent :incomplete incomplete :initial-values initial-values)))
    (loop for a from 1 upto v
	  do (loop for b from 1 upto v
		   unless (and (> a v-n) (> b v-n))
		     do (loop for ab from 1 upto v
			      do (loop for ba from 1 upto v
				       unless (and (> ab v-n) (> ba v-n))
					 do (if negative-equation-clauses
						(loop for u from 1 upto v
						      unless (= a u)
							do (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v a b ab))
												    (- (encode-qg-atom v b a ba))
												    (- (encode-qg-atom v ab ba u)))
											      clause-set)))
						(setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v a b ab))
											 (- (encode-qg-atom v b a ba))
											 (encode-qg-atom v ab ba a))
										   clause-set)))))))
    ;; Slaney: if a.ax=x or xa.a=x then a=x
    (loop for a from 1 upto v
	  do (loop for x from 1 upto v
		   unless (and (> a v-n) (> x v-n))
		     unless (= a x)
		       do (loop for u from 1 upto v
				unless (and (> a v-n) (> u v-n))
				  do (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v a x u))
									      (- (encode-qg-atom v a u x)))
									clause-set))
				     (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v x a u))
									      (- (encode-qg-atom v u a x)))
									clause-set)))))
    clause-set))

(defun qg4 (v &key not-necessarily-idempotent incomplete negative-equation-clauses initial-values)
  (let ((v-n (if incomplete (- v incomplete) v))
	(clause-set (qg v :not-necessarily-idempotent not-necessarily-idempotent :incomplete incomplete :initial-values initial-values)))
    (loop for a from 1 upto v
	  do (loop for b from 1 upto v
		   unless (and (> a v-n) (> b v-n))
		     do (loop for ab from 1 upto v
			      do (loop for ba from 1 upto v
				       unless (and (> ab v-n) (> ba v-n))
					 do (if negative-equation-clauses
						(loop for u from 1 upto v
						      unless (= a u)
							do (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v a b ab))
												    (- (encode-qg-atom v b a ba))
												    (- (encode-qg-atom v ba ab u)))
											      clause-set)))
						(setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v a b ab))
											 (- (encode-qg-atom v b a ba))
											 (encode-qg-atom v ba ab a))
										   clause-set)))))))
    clause-set))

(defun qg5 (v &key not-necessarily-idempotent incomplete negative-equation-clauses no-extra-equation-clauses initial-values)
  (let ((v-n (if incomplete (- v incomplete) v))
	(clause-set (qg v :not-necessarily-idempotent not-necessarily-idempotent :incomplete incomplete :initial-values initial-values)))
    (loop for a from 1 upto v
	  do (loop for b from 1 upto v
		   unless (and (> b v-n) (> a v-n))
		     do (loop for ba from 1 upto v
			      unless (and (> ba v-n) (> b v-n))
				do (loop for ba.b from 1 upto v
					 unless (and (> ba.b v-n) (> b v-n))
					   do (if negative-equation-clauses
						  (loop for u from 1 upto v
							unless (= a u)
							  do (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v b a ba))
												      (- (encode-qg-atom v ba b ba.b))
												      (- (encode-qg-atom v ba.b b u)))
												clause-set)))
						  (progn
						    (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v b a ba))
											     (- (encode-qg-atom v ba b ba.b))
											     (encode-qg-atom v ba.b b a))
										       clause-set))
						    ;; suggested by Fujita
						    ;; also Slaney's y(xy.y)=x and (y.xy)y=x (6/28/93)
						    (UNLESS NO-EXTRA-EQUATION-CLAUSES	;these are valid
						      (SETQ CLAUSE-SET (DP-INSERT-SORTED (LIST (ENCODE-QG-ATOM V B A BA)
											       (- (ENCODE-QG-ATOM V BA B BA.B))
											       (- (ENCODE-QG-ATOM V BA.B B A)))
											 CLAUSE-SET))
						      (SETQ CLAUSE-SET (DP-INSERT-SORTED (LIST (- (ENCODE-QG-ATOM V B A BA))
											       (ENCODE-QG-ATOM V BA B BA.B)
											       (- (ENCODE-QG-ATOM V BA.B B A)))
											 CLAUSE-SET)))))))))
    clause-set))

(defun qg6 (v &key incomplete negative-equation-clauses initial-values)
  (let ((v-n (if incomplete (- v incomplete) v))
	(clause-set (qg v :incomplete incomplete :initial-values initial-values)))
    (if negative-equation-clauses
	(loop for ab.b from 1 upto v
	      do (loop for a.ab from 1 upto v
		       unless (= ab.b a.ab)
			 do (loop for a from 1 upto v
				  do (loop for b from 1 upto v
					   unless (and (> a v-n) (> b v-n))
					     do (loop for ab from 1 upto v
						      unless (and (> ab v-n) (or (> a v-n) (> b v-n)))
							do (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v a b ab))
												    (- (encode-qg-atom v ab b ab.b))
												    (- (encode-qg-atom v a ab a.ab)))
											      clause-set)))))))
	(loop for x from 1 upto v
	      do (loop for a from 1 upto v
		       do (loop for b from 1 upto v
				unless (and (> a v-n) (> b v-n))
				  do (loop for ab from 1 upto v
					   unless (and (> ab v-n) (or (> a v-n) (> b v-n)))
					     do (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v a b ab))
											 (- (encode-qg-atom v ab b x))
											 (encode-qg-atom v a ab x))
										   clause-set))
						(setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v a b ab))
											 (- (encode-qg-atom v a ab x))
											 (encode-qg-atom v ab b x))
										   clause-set)))))))
    clause-set))

(defun qg7 (v &key incomplete negative-equation-clauses MORE-EQUATION-CLAUSES initial-values)
  (let ((v-n (if incomplete (- v incomplete) v))
	(clause-set (qg v :incomplete incomplete :initial-values initial-values)))
    (if negative-equation-clauses
	(loop for ba.b from 1 upto v
	      do (loop for a.ba from 1 upto v
		       unless (= ba.b a.ba)
			 do (loop for a from 1 upto v
				  do (loop for b from 1 upto v
					   unless (and (> a v-n) (> b v-n))
					     do (loop for ba from 1 upto v
						      unless (and (> ba v-n) (or (> a v-n) (> b v-n)))
							do (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v b a ba))
												    (- (encode-qg-atom v ba b ba.b))
												    (- (encode-qg-atom v a ba a.ba)))
											      clause-set)))))))
	(loop for x from 1 upto v
	      do (loop for a from 1 upto v
		       do (loop for b from 1 upto v
				unless (and (> a v-n) (> b v-n))
				  do (loop for ba from 1 upto v
					   unless (and (> ba v-n) (or (> a v-n) (> b v-n)))
					     do (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v b a ba))
											 (- (encode-qg-atom v ba b x))
											 (encode-qg-atom v a ba x))
										   clause-set))
						(setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v b a ba))
											 (- (encode-qg-atom v a ba x))
											 (encode-qg-atom v ba b x))
										   clause-set))
						(WHEN MORE-EQUATION-CLAUSES	;valid if zb=az has unique solution for z
						  (SETQ CLAUSE-SET (DP-INSERT-SORTED (LIST (ENCODE-QG-ATOM V B A BA)
											   (- (ENCODE-QG-ATOM V BA B X))
											   (- (ENCODE-QG-ATOM V A BA X)))
										     CLAUSE-SET))))))))
    clause-set))

(defun qg7a (v &key incomplete initial-values)
  (let ((v-n (if incomplete (- v incomplete) v))
	(clause-set (qg v :incomplete incomplete :initial-values initial-values)))
    ;;finds (132) conjugate equivalents, as suggested by Slaney
    ;;note: constraint on rightmost column not translated to conjugate equivalent, so number of models differs from qg7
    ;; (xy.x)y=x
    (loop for x from 1 upto v
	  do (loop for y from 1 upto v
		   unless (and (> x v-n) (> y v-n))
		     do (loop for xy from 1 upto v
			      unless (and (> xy v-n) (> x v-n))
				do (loop for xy.x from 1 upto v
					 unless (and (> xy.x v-n) (> y v-n))
					   do (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v x y xy))
										       (- (encode-qg-atom v xy x xy.x))
										       (encode-qg-atom v xy.x y x))
										 clause-set))))))
    ;; (xy.y).xy=x
    (loop for x from 1 upto v
	  do (loop for y from 1 upto v
		   unless (and (> x v-n) (> y v-n))
		     do (loop for xy from 1 upto v
			      unless (and (> xy v-n) (> y v-n))
				do (loop for xy.y from 1 upto v
					 unless (and (> xy.y v-n) (> xy v-n))
					   do (setq clause-set (dp-insert-sorted (list (- (encode-qg-atom v x y xy))
										       (- (encode-qg-atom v xy y xy.y))
										       (encode-qg-atom v xy.y xy x))
										 clause-set))))))
    clause-set))

(defun quasigroup-problem (problem order &rest other-args &key find-one-model INITIAL-CLAUSES &allow-other-keys)
  (let (clause-set x1)
    (setq problem (or (cdr (assoc problem '((1 . qg1) (2 . qg2) (3 . qg3) (4 . qg4) (5 . qg5) (6 . qg6) (7 . qg7)
					    (7a . qg7a))))
		      problem))
    (print (list* 'begin 'problem problem order other-args))
    (setq other-args (loop for x = other-args then (cddr x)
			   until (null x)
			   unless (member (first x) '(:find-one-model :INITIAL-CLAUSES))
			     nconc (list (first x) (second x))))
    (time (setq clause-set (apply problem order other-args)))
    (LOOP FOR CLAUSE IN INITIAL-CLAUSES
	  DO (SETQ CLAUSE-SET (DP-INSERT-SORTED (MAPCAR #'(LAMBDA (X) (ENCODE-QG-ATOM ORDER (FIRST X) (SECOND X) (THIRD X))) CLAUSE)
						CLAUSE-SET)))
;;  (dp-count clause-set t)
    (time (setq x1 (dp-satisfiable-p clause-set (not find-one-model))))
    (when x1
      (print x1)
      (print-quasigroup-multiplication-table (make-quasigroup-multiplication-table (if find-one-model x1 (car x1))))
      (loop for model in (if find-one-model (list x1) x1)
	    do (myassert2 (verify-quasigroup-multiplication-table (make-quasigroup-multiplication-table model) problem))))
    (print (list* 'end 'problem problem order other-args))
    nil))

(defun quasigroup-problem-file (file problem order &rest other-args)
  (let (clause-set)
    (setq problem (or (cdr (assoc problem '((1 . qg1) (2 . qg2) (3 . qg3) (4 . qg4) (5 . qg5) (6 . qg6) (7 . qg7)))) problem))
    (setq clause-set (apply problem order other-args))
    (dp-count clause-set t)
    (with-open-file (s file :direction :output)
      (mapc #'(lambda (x) (prin1 x s) (terpri s)) (dp-clauses clause-set)))
    nil))

(defun make-quasigroup-multiplication-table (model)
  (multiple-value-bind (x n)
      (decode-qg-atom (car model))
    (declare (ignore x))
    (loop with table = (make-array (list (1+ n) (1+ n)) :initial-element nil)
	  for v in model
	  as atom = (decode-qg-atom v)
	  as i = (cadadr atom)
	  as j = (car (cddadr atom))
	  as k = (caddr atom)
	  IF (NOT (EQ (CAADR ATOM) '*))		;ONLY CONSTRUCT FIRST TABLE
	    DO NIL
	  else
	    if (null (aref table i j))
	      do (setf (aref table i j) k)
	  else
	    do (error "~D*~D=~D and ~D*~D=~D" i j (aref table i j) i j k)
	  finally (return table))))

(defun print-quasigroup-multiplication-table (table)
  (let ((n (1- (car (array-dimensions table)))))
    (format t "~%x*y  y")
    (loop for j from 1 upto n do (format t "~4D" j))
    (format t "~%   x +")
    (loop repeat n do (format t "----"))
    (loop for i from 1 upto n
	  do (format t "~%~4D |" i)
	     (loop for j from 1 upto n
		   as v = (aref table i j)
		   do (if v (format t "~4D" v) (format t "   -"))))
;;  (format t "~%")
    table))

(defun verify-quasigroup-multiplication-table (table &optional type not-necessarily-idempotent)
  (let ((n (1- (car (array-dimensions table)))))
    (and
      (loop for i from 1 upto n
	    always (loop for j from 1 upto n
			 always (and (integerp (aref table i j)) (<= 1 (aref table i j) n))))
      (or
	not-necessarily-idempotent
	(loop for i from 1 upto n
	      always (= (aref table i i) i)))
      (loop for i from 1 upto n
	    always (loop for k from 1 upto n
			 always (= (loop for j from 1 upto n count (= (aref table i j) k)) 1)))
      (loop for j from 1 upto n
	    always (loop for k from 1 upto n
			 always (= (loop for i from 1 upto n count (= (aref table i j) k)) 1)))
      (or
	(null type)
	(case type
	  ((132 213 231)
	   (qg-orthogonal-p table (qg-conjugate table type)))
	  ((qg1 qg1a 1 321)
	   (qg-orthogonal-p table (qg-conjugate table 321)))
	  ((qg2 qg2a 2 312)
	   (qg-orthogonal-p table (qg-conjugate table 312)))
	  ((qg3 3)
	   (loop for x from 1 upto n
		  always (loop for y from 1 upto n
			       always (= (aref table (aref table x y) (aref table y x)) x))))
	  ((qg4 4)
	    (loop for x from 1 upto n
		  always (loop for y from 1 upto n
			       always (= (aref table (aref table y x) (aref table x y)) x))))
	  ((qg5 5)
	    (loop for x from 1 upto n
		  always (loop for y from 1 upto n
			       always (= (aref table (aref table (aref table y x) y) y) x))))
	  ((qg6 6)
	    (loop for x from 1 upto n
		  always (loop for y from 1 upto n
			       always (= (aref table (aref table x y) y) (aref table x (aref table x y))))))
	  ((qg7 7)
	    (loop for x from 1 upto n
		  always (loop for y from 1 upto n
			       always (= (aref table (aref table y x) y) (aref table x (aref table y x))))))
	  ((qg7a 7a)
	    (let ((table (qg-conjugate table 132)))
	      (loop for x from 1 upto n
		    always (loop for y from 1 upto n
				 always (= (aref table (aref table y x) y) (aref table x (aref table y x)))))))
	  (otherwise
	    (format t "~%Don't know how to verify models of problem ~A." type)
	    t)))
      table)))

(defun qg-conjugate (table conjugate)
  (cond
    ((= conjugate 123)
     table)
    (t
     (loop with n = (1- (car (array-dimensions table)))
	   with table2 = (make-array (list (1+ n) (1+ n)))
	   for a from 1 upto n
	   do (loop for b from 1 upto n
		    as c = (aref table a b)
		    do (ecase conjugate
			 (132 (setf (aref table2 a c) b))
			 (213 (setf (aref table2 b a) c))
			 (231 (setf (aref table2 b c) a))
			 (312 (setf (aref table2 c a) b))
			 (321 (setf (aref table2 c b) a))))
	   finally (return table2)))))

(defun qg-orthogonal-p (table1 table2)
  (loop with n = (1- (car (array-dimensions table1)))
	for a from 1 upto n
	always (loop for b from 1 upto n
		     always (loop for c from 1 upto n
				  always (loop for d from 1 upto n
					       always (or (not (= (aref table1 a b) (aref table1 c d)))
							  (not (= (aref table2 a b) (aref table2 c d)))
							  (and (= a c) (= b d))))))))


(defun myassert2 (arg)
  (assert arg)
)


(defmacro EMPTY-TRIE-P (trie)
  ;; trie constains satisfiable empty set of clauses
  `(null ,trie))

(defmacro EMPTY-CLAUSE-TRIE-P (trie)
  ;; trie contains just the unsatisfiable empty clause
  `(eq ,trie t))


(defun DP-INSERT (clause clause-set)
  (trie-insert clause clause-set))

(defun DP-INSERT-SORTED (clause clause-set)
  (trie-insert-sorted clause clause-set))

(defun DP-INSERT-FILE (filename &optional clause-set)
  (when dp-print-summary
    (format t "~2%Problem from file ~A:" filename))
  (with-open-file (s filename :direction :input)
    (loop for clause = (read s nil 'eof)
	  until (eq clause 'eof)
	  if (consp clause)
	    do (setq clause-set (dp-insert-sorted clause clause-set))
	  else
	    do (warn "Skipping nonclause ~A in file." clause)
	  finally (return clause-set))))

(defun DP-COUNT (clause-set &optional print-p)
  ;; (dp-count clause-set) returns and optionally prints the clause and literal count
  ;; of clauses stored in clause-set
  (trie-count clause-set print-p))

(defun DP-CLAUSES (clause-set &optional decode-fun map-fun)
  ;; either return or apply map-fun to all clauses in clause-set
  (trie-clauses clause-set decode-fun map-fun))

(defun DP-CLAUSES-FILE (filename clause-set &optional decode-fun)
  ;; write clauses in clause-set to a file
  (with-open-file (s filename :direction :output :if-exists :new-version)
    (dp-clauses clause-set decode-fun #'(lambda (clause) (princ clause s) (terpri s))))
  nil)

;;; Propositional clause trie is of the form
;;;   t  for a clause with no more literals at this position in the trie
;;;   alist  contains pairs (called "tp" for "trie-pair")
;;;          of the form (atom . (trie1 . trie2))
;;;     atom   proposition symbol encoded as a positive number
;;;     trie1  subtrie for clauses here in trie containing atom positively
;;;     trie2  subtrie for clauses here in trie containing atom negatively
;;;   alist pairs are ordered by <
;;; This is a very efficient representation for sets of clauses
;;; that was suggested by Johan deKleer (AAAI92).
;;;
;;; Propositional clause tries are constructed by inserting clauses one-by-one
;;; by the trie-insert function
;;;   input for trie-insert: C v A v -B  =>  (1 -2 3)
;;;     where A is encoded as 1, B as 2, and C as 3
;;;   literals must be ordered by < on absolute value
;;;   trie-insert-sorted sorts the clause before inserting it
;;;
;;; Proposition symbols are assumed to be encoded as fixnums for greater efficiency.
;;; Nonfixnum (e.g., larger or noninteger) numbers can be used by eliminating the
;;; (typep atom 'fixnum) assertion in trie-insert* and replacing (the fixnum expr)
;;; by expr in trie-merge* and trie-assign-atoms*.

(defmacro MAKE-TP (atom subtries)
  `(cons ,atom ,subtries))

(defmacro TP-ATOM (tp)
  `(car ,tp))

(defmacro TP-SUBTRIES (tp)
  `(cdr ,tp))

(defmacro TRUE-SUBTRIE (subtries)
  `(car ,subtries))

(defmacro FALSE-SUBTRIE (subtries)
  `(cdr ,subtries))

(defun TRIE-INSERT-SORTED (clause trie)
  (trie-insert (sort-clause clause) trie))

(defun TRIE-INSERT (clause trie)
  ;; destructive insertion, but value must be used
  (myassert (not (null clause)))		;only nonempty clauses can be inserted
  (myassert (not (zerop (first clause))))	;don't allow 0, since -0 = 0 => (not atom) = atom
  (trie-insert* clause trie 0))

(defun TRIE-INSERT* (clause trie previous-literal)
  (myassert (<= (abs previous-literal) (abs (first clause))))	;literals in ascending order
  (cond
    ((empty-clause-trie-p trie)
     t)
    ((= previous-literal (first clause))	;ignore duplicate literal in clause
     (let ((l1 (rest clause)))
       (if (null l1) t (trie-insert* l1 trie previous-literal))))
    ((= previous-literal (- (first clause)))	;ignore clause with complementary literal
     trie)
    (t
     (let ((atom (abs (first clause))) v)
       (myassert (typep atom 'fixnum))
       (cond
	 ((or (null trie) (< atom (tp-atom (first trie))))
	  (setq trie (cons (make-tp atom (setq v (cons nil nil))) trie)))
	 ((= atom (tp-atom (first trie)))
	  (setq v (tp-subtries (first trie))))
	 (t
	  (loop for trie on trie
		when (or (null (rest trie)) (< atom (tp-atom (second trie))))
		  do (setf (rest trie) (cons (make-tp atom (setq v (cons nil nil))) (rest trie)))
		     (return nil)
		when (= atom (tp-atom (second trie)))
		  do (setq v (tp-subtries (second trie)))
		     (return nil))))
       (let ((l1 (rest clause)))
	 (cond
	   ((> (first clause) 0)
	    (setf (true-subtrie v) (if (null l1) t (trie-insert* l1 (true-subtrie v) (first clause)))))
	   (t
	    (setf (false-subtrie v) (if (null l1) t (trie-insert* l1 (false-subtrie v) (first clause)))))))
       trie))))

(defun SORT-CLAUSE (clause)
  ;; utility function returns < sorted variant of clause
  ;; to use for insertion into a trie by trie-insert
  (cond
    ((null clause)
     nil)
    ((null (rest clause))
     clause)
    ((null (rest (rest clause)))
     (if (< (abs (first clause)) (abs (second clause)))
	 clause
	 (list (second clause) (first clause))))
    (t
     (sort (copy-list clause) #'< :key #'abs))))

(defun TRIE-COUNT (trie &optional print-p)
  ;; (trie-count trie) returns and optionally prints the clause and literal count
  ;; of clauses stored in trie
  (let ((nclauses 0) (nliterals 0) (nnodes 0))
    (labels
      ((trie-count* (trie length)
	 (cond
	   ((empty-clause-trie-p trie)
	    (incf nclauses)
	    (incf nliterals length))
	   (t
	    (loop for tp in trie
		  as subtries = (tp-subtries tp)
		  do (incf nnodes)
		     (let ((true-subtrie (true-subtrie subtries)))
		       (unless (empty-trie-p true-subtrie)
			 (trie-count* true-subtrie (1+ length))))
		     (let ((false-subtrie (false-subtrie subtries)))
		       (unless (empty-trie-p false-subtrie)
			 (trie-count* false-subtrie (1+ length)))))))))
      (trie-count* trie 0)
      (when print-p
	(format t "~&Trie contains ~D clauses with ~D literals using ~D nodes." nclauses nliterals nnodes))
      (values nclauses nliterals nnodes))))

(defun TRIE-CLAUSES (trie &optional decode-fun map-fun)
  ;; either return or apply map-fun to all clauses in trie
  (let (clauses)
    (labels
      ((trie-clauses* (trie decode-fun literals)
	 (cond
	   ((empty-clause-trie-p trie)
	    (if map-fun
		(funcall map-fun (reverse literals))
		(push (reverse literals) clauses)))
	   (t
	    (loop for tp in trie
		  as subtries = (tp-subtries tp)
		  do (let ((true-subtrie (true-subtrie subtries)))
		       (unless (empty-trie-p true-subtrie)
			 (trie-clauses* true-subtrie
					decode-fun
					(cons (if decode-fun
						  (funcall decode-fun (tp-atom tp))
						  (tp-atom tp))
					      literals))))
		     (let ((false-subtrie (false-subtrie subtries)))
		       (unless (empty-trie-p false-subtrie)
			 (trie-clauses* false-subtrie
					decode-fun
					(cons (if decode-fun
						  (list 'not (funcall decode-fun (tp-atom tp)))
						  (- (tp-atom tp)))
					      literals)))))))))
      (trie-clauses* trie decode-fun nil))
    (nreverse clauses)))

;;; trie-assign-atoms and trie-merge do the work of creating the new trie
;;; that results from assignming truth values to some atoms in a trie

(defmacro TRIE-MERGE (trie1 trie2)
  ;; check for empty and empty-clause tries without calling a function
  (cond
    ((not (symbolp trie1))
     (let ((x (gensym)))
       `(let ((,x ,trie1))
	  (trie-merge ,x ,trie2))))
    ((not (symbolp trie2))
     (let ((y (gensym)))
       `(let ((,y ,trie2))
	  (trie-merge ,trie1 ,y))))
    (t
     `(cond
	((empty-trie-p ,trie1)
	 ,trie2)
	((empty-trie-p ,trie2)
	 ,trie1)
	((or (empty-clause-trie-p ,trie1) (empty-clause-trie-p ,trie2))
	 t)
	(t
	 (trie-merge* ,trie1 ,trie2))))))

(defun TRIE-MERGE* (trie1 trie2)
  (loop with result = nil
	with result-tail
	as atom1 = (tp-atom (first trie1))
	as atom2 = (tp-atom (first trie2))
	as tp = (cond
		  ((<= (the fixnum atom1) (the fixnum atom2))
		   (cond
		     ((= (the fixnum atom1) (the fixnum atom2))
		      (let* ((v1 (tp-subtries (pop trie1)))
			     (v2 (tp-subtries (pop trie2)))
			     (w1 (trie-merge (true-subtrie v1) (true-subtrie v2)))
			     (w2 (trie-merge (false-subtrie v1) (false-subtrie v2))))
			(cond
			  ((and (empty-clause-trie-p w1) (empty-clause-trie-p w2))
			   (return-from trie-merge* t))
			  (t
			   (make-tp atom1 (cons w1 w2))))))
		     (t
		      (pop trie1))))
		  (t
		   (pop trie2)))
	do (if result
	       (setq result-tail (setf (cdr result-tail) (cons tp nil)))
	       (setq result (setq result-tail (cons tp nil))))
	when (null trie1)
	  do (setf (cdr result-tail) trie2)
	     (return result)
	when (null trie2)
	  do (setf (cdr result-tail) trie1)
	     (return result)))

(defmacro TRIE-ASSIGN-ATOMS (assignment trie)
  ;; check for empty and empty-clause tries without calling a function
  (cond
    ((not (symbolp trie))
     (let ((y (gensym)))
       `(let ((,y ,trie))
	  (trie-assign-atoms ,assignment ,y))))
    (t
     `(cond
	((or (empty-trie-p ,trie) (empty-clause-trie-p ,trie))
	 ,trie)
	(t
	 (trie-assign-atoms* ,assignment ,trie))))))

(defmacro TRIE-ASSIGN-ATOMS-VALUE (true-subtrie* false-subtrie* v)
  (let ((w (gensym)))
    `(let ((,w ,v))
       (cond
	 ((empty-clause-trie-p ,w)
	  t)
	 ((and (eq ,true-subtrie* true-subtrie)
	       (eq ,false-subtrie* false-subtrie))
	  (if (eq ,w (rest trie))
	      trie
	      (cons tp ,w)))
	 (t
	  (cons (make-tp atom (cons ,true-subtrie* ,false-subtrie*)) ,w))))))

(defun TRIE-ASSIGN-ATOMS* (assignment trie)
  ;; applies an assignment of truth values to atoms to a trie
  ;; assignment is list of dotted pairs of form (atom . truth-value)
  ;; where truth-value is :true or :false
  ;; and assignment is ordered by < on atoms
  (loop with tp = (first trie)
	with atom = (tp-atom tp)
	for l on assignment
	as assign = (first l)
	as x = (car assign)
	when (<= (the fixnum atom) (the fixnum x))
	  return (if (= (the fixnum atom) (the fixnum x))
		     (if (null (setq l (rest l)))
			 (trie-merge (let ((v (if (eq (cdr assign) :true)
						  (false-subtrie (tp-subtries tp))
						  (true-subtrie (tp-subtries tp)))))
				       (when v
					 (setq *pure* nil))
				       v)
				     (rest trie))
			 (trie-merge (trie-assign-atoms
				       l (if (eq (cdr assign) :true)
					     (false-subtrie (tp-subtries tp))
					     (true-subtrie (tp-subtries tp))))
				     (trie-assign-atoms
				       l (rest trie))))
		     (let ((w (trie-assign-atoms l (rest trie))))
		       (cond
			 ((empty-clause-trie-p w)
			  t)
			 (t
			  (let* ((subtries (tp-subtries tp))
				 (true-subtrie (true-subtrie subtries))
				 (false-subtrie (false-subtrie subtries))
				 (true-subtrie* (trie-assign-atoms l true-subtrie))
				 (false-subtrie* (trie-assign-atoms l false-subtrie)))
			    (cond
			      ((and (empty-trie-p true-subtrie*)
				    (empty-trie-p false-subtrie*))
			       w)
			      ((empty-clause-trie-p true-subtrie*)
			       (cond
				 ((empty-clause-trie-p false-subtrie*)
				  t)
				 (t
				  (trie-assign-atoms-value true-subtrie* nil (trie-merge false-subtrie* w)))))
			      ((empty-clause-trie-p false-subtrie*)
			       (trie-assign-atoms-value nil false-subtrie* (trie-merge true-subtrie* w)))
			      (t
			       (trie-assign-atoms-value true-subtrie* false-subtrie* w))))))))
	finally (return trie)))

(defun FIND-UNIT-CLAUSES-IN-TRIE (trie)
  (loop for tp in trie
	as subtries = (tp-subtries tp)
	when (empty-clause-trie-p (true-subtrie subtries))
	  if (empty-clause-trie-p (false-subtrie subtries)) do (return :unsatisfiable) else collect (cons (tp-atom tp) :true)
	when (empty-clause-trie-p (false-subtrie subtries))
	  collect (cons (tp-atom tp) :false)))

(defun LENGTH-OF-SHORTEST-POSITIVE-CLAUSE-IN-TRIE (trie &optional bound)
  ;; assume trie isn't t (just the empty clause) and bound if given is >= 2
  ;; only look for clauses strictly shorter than bound
  (cond
    ((loop for tp in trie			;look for positive unit clause
	   thereis (empty-clause-trie-p (true-subtrie (tp-subtries tp))))
     1)
    ((eql bound 2)
     nil)
    (t
     (loop with bound1 = (and bound (1- bound))
	   with best = nil
	   with m = nil
	   for tp in trie
	   as true-subtrie = (true-subtrie (tp-subtries tp))
	   unless (empty-trie-p true-subtrie)
	     do (cond
		  ((null (setq m (length-of-shortest-positive-clause-in-trie true-subtrie bound1)))
		   )
		  ((= m 1)
		   (return 2))
		  (t
		   (setq bound1 (setq best m))))
	   finally (return (and best (1+ best)))))))

(defun CHOOSE-AN-ATOM-OF-A-SHORTEST-POSITIVE-CLAUSE (trie)
  ;; assume trie isn't t (just the empty clause) and has no unit clauses
  ;; return the first literal of the first positive clause of minimum length
  (loop with atom = nil
	with best = nil
	with m = nil
	for tp in trie
	as true-subtrie = (true-subtrie (tp-subtries tp))
	unless (empty-trie-p true-subtrie)
	  do (cond
	       ((null (setq m (length-of-shortest-positive-clause-in-trie true-subtrie best)))
		)
	       ((= m 1)				;quit at first 2-clause
		(return (values (tp-atom tp) :true :false)))
	       (t
		(setq best m)
		(setq atom (tp-atom tp))))
	finally (return (if atom (values atom :true :false) :satisfiable))))

;;; Examples.
;;; Clauses are represented by lists of literals.
;;; Atomic formulas are represented by numbers > 0.
;;; For example, 3 is a positive literal and -3 is its negation.
;;; Clauses are added to a set of clauses by dp-insert.
;;; Literals in clauses are required to be ordered by < on absolute value
;;; dp-insert-sorted can be used instead of dp-insert to sort and add clauses.
;;; Tautologies and duplicate literals are automatically eliminated.

(defun ALLWAYS-3-PROBLEM ()
  ;; all signed combinations of three propositions
  ;; this is not satisfiable
  ;; you can omit some of the clauses to make the set
  ;; satisfiable and observe dp-satisfiable-p's behavior
  (let ((clause-set nil))
    (setq clause-set (dp-insert '(1 2 3) clause-set))
    (setq clause-set (dp-insert '(1 2 -3) clause-set))
    (setq clause-set (dp-insert '(1 -2 3) clause-set))
    (setq clause-set (dp-insert '(1 -2 -3) clause-set))
    (setq clause-set (dp-insert '(-1 2 3) clause-set))
    (setq clause-set (dp-insert '(-1 2 -3) clause-set))
    (setq clause-set (dp-insert '(-1 -2 3) clause-set))
    (setq clause-set (dp-insert '(-1 -2 -3) clause-set))
;;  (dp-count clause-set t)
;;  (mapc #'print (dp-clauses clause-set))
    (dp-satisfiable-p clause-set)))

(defun PIGEONHOLE-PROBLEM (nobjects)
  (dp-satisfiable-p (pigeonhole-problem-clauses nobjects)))

(defun QUEENS-PROBLEM (n &optional find-all-models)
  (dp-satisfiable-p (queens-problem-clauses n) find-all-models))

(defun PIGEONHOLE-PROBLEM-CLAUSES (nobjects)
  (let ((nholes (1- nobjects)) (clause-set nil))
    (loop for i from 1 to nobjects
	  do (setq clause-set (dp-insert (loop for j from 1 to nholes collect (encode-example-atom nobjects i j))
					 clause-set)))
    (loop for j from 1 to nholes
	  do (loop for i1 from 1 to (1- nobjects)
		   do (loop for i2 from (1+ i1) to nobjects
			    do (setq clause-set (dp-insert (list (- (encode-example-atom nobjects i1 j))
								 (- (encode-example-atom nobjects i2 j)))
							   clause-set)))))
    clause-set))

(defun QUEENS-PROBLEM-CLAUSES (n)
  (let ((clause-set nil))
    (loop for i from 1 upto n
	  do (setq clause-set (dp-insert (loop for j from 1 upto n collect (encode-example-atom n i j))
					 clause-set)))
    (loop for i from 1 upto n
	  do (loop for j from 1 upto (1- n)
		   do (loop for k from (1+ j) upto n
			    do (setq clause-set (dp-insert (list (- (encode-example-atom n i j))
								 (- (encode-example-atom n i k)))
							   clause-set))
			       (setq clause-set (dp-insert (list (- (encode-example-atom n j i))
								 (- (encode-example-atom n k i)))
							   clause-set)))))
    (loop for i1 from 1 upto (1- n)
	  do (loop for i2 from (1+ i1) upto n
		   as d = (- i2 i1)
		   do (loop for j1 from 1 upto n
			    when (>= (- j1 d) 1)
			      do (setq clause-set (dp-insert-sorted (list (- (encode-example-atom n i1 j1))
									  (- (encode-example-atom n i2 (- j1 d))))
								    clause-set))
			    when (<= (+ j1 d) n)
			      do (setq clause-set (dp-insert-sorted (list (- (encode-example-atom n i1 j1))
									  (- (encode-example-atom n i2 (+ j1 d))))
								    clause-set)))))
    clause-set))

(defun ENCODE-EXAMPLE-ATOM (n i j)
  (+ n (* 1000 (+ (* (1- i) n) (1- j)))))

(defun DECODE-EXAMPLE-ATOM (v)
  (let ((n (mod v 1000)) i j)
    (setq v (floor v 1000))
    (setq j (1+ (mod v n)))
    (setq v (floor v n))
    (setq i (1+ v))
    (values `(p ,i ,j) n)))


(defun myassert (arg)
  (assert arg)
)
