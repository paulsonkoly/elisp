;;; infix.el --- Arithmetic expression infix to lisp (prefix) rewriter

;;; Commentary:
;;
;; Use at your own risk.  This will probably set your computer on fire.
;; 

(require 'cl)

;;; Code:

(defconst *infix/operator-precedence*
  '((+ . 0) (- . 0) (* . 1) (/ . 1))

  "Association list of the operators and their precedence.")


(defun infix/find-lowest-precedence(exp)
  "Finds the first lowest precedence operator in EXP.

Goes from right to left, equal precedence returns the position of the
rightmost operator. This results in left associative rewrite. '(1 + 2 - 3) is
rewritten as '(- (+ 1 2) 3) rather then '(+ 1 (- 2 3))"

  (let ((result-index 0)
	(result-prec 1000)
	(current-index 0)
	(rexp (reverse exp)))

    (dolist (v rexp)
      (let ((vprec-pair (assoc v *infix/operator-precedence*)))
	(if vprec-pair
	    (let ((vprec (cdr vprec-pair)))
	      (if (< vprec result-prec)
		  (progn
		    (setq result-index current-index)
		    (setq result-prec vprec)))))
	(setq current-index (1+ current-index))))
    (- (length exp) result-index 1)))


(defun infix/split(exp idx)
  "Splits EXP around IDX"

  (list (subseq exp 0 idx) (elt exp idx) (subseq exp (1+ idx))))


(defun infix/wrapper(exp op) (if (and (listp exp) (eq op (car exp))) (cdr exp) (list exp)))


(defun infix/to-lisp(exp)
  "Rewrites EXP from infix form of a list of SYMBOLs to lisp form.

For example '(1 * 2 + 3) is rewritten as '(+ (* 1 2) 3)"

  (if (listp exp)
      
      (case (length exp)
	(0 nil)
	(1 (infix/to-lisp (car exp)))
	(2 (list (cadr exp) (car exp)))
	(otherwise
	 (let* ((spl (infix/split exp (infix/find-lowest-precedence exp)))
		(left (infix/to-lisp (car spl)))
		(op (cadr spl))
		(right (infix/to-lisp (cddr spl))))
	   (append (list op) (infix/wrapper left op) (infix/wrapper right op)))))
    exp))


(defun infix/rewrite-to-lisp(string &optional start end)
  "Rewrites a string from infix form to lisp ie : (1 * 2 + 3) => (+ (* 1 2) 3)

When called interactively, work on current list or text selection.

When called in lisp code, if STRING is non-nil, returns a changed string.
If STRING nil, change the text in the region between positions START and END"

  (interactive
   (if (use-region-p)
       (list nil (region-beginning) (region-end))
     (let ((bds (bounds-of-thing-at-point 'list)))
       (list nil (car bds) (cdr bds)))))

  (if string
      (infix/to-lisp (read string))
    (let* ((exp (read (buffer-substring-no-properties start end)))
	   (rexp (infix/to-lisp exp)))
	(delete-region start end)
	(print rexp (current-buffer)))))

(provide 'infix)

;;; infix.el ends here
