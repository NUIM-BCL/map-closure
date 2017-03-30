;;; LaHaShem HaAretz U'Mloah

;;; Stalin 0.10 - A global optimizing compiler for Scheme
;;; Copyright 1993, 1994, and 1995 University of Toronto. All rights reserved.
;;; Copyright 1996 Technion. All rights reserved.
;;; Copyright 1996 and 1997 University of Vermont. All rights reserved.
;;; Copyright 1997, 1998, 1999, 2000, and 2001 NEC Research Institute, Inc. All
;;; rights reserved.
;;; Copyright 2002 and 2003 Purdue University. All rights reserved.

;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; written by:
;;;    Jeffrey Mark Siskind
;;;    NEC Research Institute, Inc.
;;;    4 Independence Way
;;;    Princeton NJ 08540-6620 USA
;;;    voice: 609/951-2705
;;;    FAX:   609/951-2483
;;;    Qobi@research.nj.nec.com
;;;    ftp://ftp.nj.nec.com/pub/qobi
;;;    http://www.neci.nj.nec.com/homepages/qobi

;;; Derived from the t4aug98 archive of QobiScheme, updated to the m7dec98,
;;; m24jan00, f10mar00, h22apr00, f5may00, m12jun00, and m25jun01 archives.

;;; removed: MODULE

;;; TTMTTD
;;;  1. learn how to use profiler
;;;  2. self-documentation
;;;  3. ability to abort out of button presses
;;;  4. What if debugger called inside WITH-INPUT-FROM-FILE or
;;;     WITH-OUTPUT-TO-FILE? I.e. should temporarily rebind CURRENT-INPUT-PORT
;;;     and CURRENT-OUTPUT-PORT inside debugger.
;;;  5. Should catch stack overflow error and out of memory error.
;;;  6. $, $$, and $$$ only work in debugger.
;;;  7. What about errors inside debugger?
;;;  8. Breakpoints, tracing, and timeouts.
;;;  9. Should save error string and only call format once on format-string
;;;     and args.
;;; 10. Can't nest interrupts more than two deep.
;;; 11. Need to make c-z c, c-z e, c-d, c-z a, and m-TAB work.
;;; 12. Need way to set DISPLAY, SCGCINFO, SCHEAP, SCLIMIT, SCMAXHEAP,
;;;     stack (and other) limits, and cd.
;;; 13. Maybe put back checks for "SCEVAL_INTERPRETED-PROC" and
;;;     "LOOP [inside EXEC]".

(include "../dependencies/define-macro/define-macro.scm")

(define-macro (define-structure . form)
 ;; needs work: To check that SLOTS is disjoint.
 (let ((form (cons 'define-structure form)))
  (unless (and (>= (length form) 3)
	       (let loop ((x (cdr form)))
		(or (null? x) (and (symbol? (car x)) (loop (cdr x))))))
   (error 'define-structure "Improper DEFINE-STRUCTURE: ~s" form))
  (define rest cdr)
  (define (map-indexed f l)
   ;; needs work: To eliminate REVERSE.
   (let loop ((i 0) (l l) (c '()))
    (if (null? l)
	(reverse c)
	(loop (+ i 1) (rest l) (cons (f (first l) i) c)))))
  (let ((type (second form))
	(slots (rest (rest form))))
   `(begin
     (define (,(string->symbol (string-append "make-" (symbol->string type)))
	      ,@slots)
      (vector ',type ,@slots))
     (define (,(string->symbol (string-append (symbol->string type) "?"))
	      obj)
      (and (vector? obj)
	   (= (vector-length obj) ,(- (length form) 1))
	   (eq? (vector-ref obj 0) ',type)))
     ,@(map-indexed
	(lambda (slot i)
	 `(begin
	   (define (,(string->symbol
		      (string-append "local-set-"
				     (symbol->string type)
				     "-"
				     (symbol->string slot)
				     "!"))
		    type
		    obj)
	    (unless (,(string->symbol
		       (string-append (symbol->string type) "?"))
		     type)
	     (panic ,(string-append "local-set-"
				    (symbol->string type)
				    "-"
				    (symbol->string slot)
				    "! error")))
	    (local-vector-set! type ,(+ i 1) obj))
	   (define (,(string->symbol
		      (string-append (symbol->string type)
				     "-"
				     (symbol->string slot)))
		    type)
	    (unless (,(string->symbol
		       (string-append (symbol->string type) "?"))
		     type)
	     (panic ,(string-append (symbol->string type)
				    "-"
				    (symbol->string slot)
				    " error")))
	    (vector-ref type ,(+ i 1)))
	   (define (,(string->symbol
		      (string-append "set-"
				     (symbol->string type)
				     "-"
				     (symbol->string slot)
				     "!"))
		    type
		    obj)
	    (unless (,(string->symbol
		       (string-append (symbol->string type) "?"))
		     type)
	     (panic ,(string-append "set-"
				    (symbol->string type)
				    "-"
				    (symbol->string slot)
				    "! error")))
	    (vector-set! type ,(+ i 1) obj))))
	slots)))))

(define-macro (define-command . form)
 (let ((form (cons 'define-structure form)))
  (define rest cdr)
  (define (reduce f l i)
   (cond ((null? l) i)
	 ((null? (rest l)) (first l))
	 (else (let loop ((l (rest l)) (c (first l)))
		(if (null? l) c (loop (rest l) (f c (first l))))))))
  (define (valid-command-arguments? l)
   (define (valid-optional-parameter? l)
    (and (list? l)
	 (= (length l) 4)
	 (symbol? (first l))
	 (string? (second l))))
   (define (valid-required-parameter? l)
    (and (list? l)
	 (= (length l) 3)
	 (symbol? (first l))
	 (string? (second l))))
   (define (order-ok-optional? l)
    (or (null? l)
	(and (eq? (first (first l)) 'optional)
	     (order-ok-optional? (rest l)))
	(and (eq? (first (first l)) 'rest)
	     (null? (rest l)))))
   (define (order-ok-required? l)
    (or (null? l)
	(and (eq? (first (first l)) 'required)
	     (order-ok-required? (rest l)))
	(and (eq? (first (first l)) 'optional)
	     (order-ok-optional? (rest l)))
	(and (eq? (first (first l)) 'rest)
	     (null? (rest l)))))
   (define (order-ok? l)
    (or (null? l)
	(and (or (eq? (first (first l)) 'any-number)
		 (eq? (first (first l)) 'at-least-one)
		 (eq? (first (first l)) 'at-most-one)
		 (eq? (first (first l)) 'exactly-one))
	     (order-ok? (rest l)))
	(and (eq? (first (first l)) 'required)
	     (order-ok-required? (rest l)))
	(and (eq? (first (first l)) 'optional)
	     (order-ok-optional? (rest l)))
	(and (eq? (first (first l)) 'rest)
	     (null? (rest l)))))
   (and
    (list? l)
    (>= (length l) 1)
    (symbol? (first l))
    (every
     (lambda (l)
      (and
       (list? l)
       (>= (length l) 1)
       (or (and (or (eq? (first l) 'exactly-one) (eq? (first l) 'at-most-one))
		(>= (length l) 2)
		(every
		 (lambda (l)
		  (and (list? l)
		       (>= (length l) 2)
		       (string? (first l))
		       (symbol? (second l))
		       (every valid-optional-parameter? (rest (rest l)))))
		 (rest l)))
	   (and (or (eq? (first l) 'at-least-one) (eq? (first l) 'any-number))
		(>= (length l) 2)
		(every
		 (lambda (l)
		  (and (list? l)
		       (>= (length l) 2)
		       (string? (first l))
		       (symbol? (second l))
		       (every valid-required-parameter? (rest (rest l)))))
		 (rest l)))
	   (and (or (eq? (first l) 'required) (eq? (first l) 'rest))
		(= (length l) 2)
		(valid-required-parameter? (second l)))
	   (and (eq? (first l) 'optional)
		(= (length l) 2)
		(valid-optional-parameter? (second l))))))
     (rest l))
    (order-ok? (rest l))))
  (define (command-usage l)
   (define (command-usage1 l)
    (let ((s (let loop ((l l))
	      (define (command-usage l)
	       (string-append
		"-"
		(first l)
		(let loop ((l (rest (rest l))))
		 (cond
		  ((null? l) "")
		  ((null? (rest l)) (string-append " " (second (first l))))
		  (else
		   (string-append " " (second (first l)) (loop (rest l))))))))
	      (if (null? (rest l))
		  (command-usage (first l))
		  (string-append
		   (command-usage (first l)) "|" (loop (rest l)))))))
     (if (= (length l) 1) s (string-append "[" s "]"))))
   (if (null? l)
       ""
       (case (first (first l))
	((any-number)
	 (string-append " ["
			(command-usage1 (rest (first l)))
			"]*"
			(command-usage (rest l))))
	((at-least-one)
	 (string-append " ["
			(command-usage1 (rest (first l)))
			"]+"
			(command-usage (rest l))))
	((at-most-one)
	 (string-append
	  " [" (command-usage1 (rest (first l))) "]" (command-usage (rest l))))
	((exactly-one)
	 (string-append
	  " " (command-usage1 (rest (first l))) (command-usage (rest l))))
	((required)
	 (string-append " "
			(second (second (first l)))
			(command-usage (rest l))))
	((optional)
	 (string-append " ["
			(second (second (first l)))
			(command-usage (rest l)) "]"))
	((rest) (string-append " [" (second (second (first l))) "]*"))
	(else (fuck-up)))))
  (define (command-bindings l)
   (if (null? l)
       '()
       (case (first (first l))
	((any-number at-least-one)
	 (append (reduce
		  append
		  (map (lambda (l)
			(cons (list (second l) #f)
			      (map (lambda (l) (list (first l) ''()))
				   (rest (rest l)))))
		       (rest (first l)))
		  '())
		 (command-bindings (rest l))))
	((at-most-one exactly-one)
	 (append (reduce
		  append
		  (map (lambda (l)
			(cons (list (second l) #f)
			      (map (lambda (l) (list (first l) (fourth l)))
				   (rest (rest l)))))
		       (rest (first l)))
		  '())
		 (command-bindings (rest l))))
	((required) (cons (list (first (second (first l))) #f)
			  (command-bindings (rest l))))
	((optional) (cons (list (first (second (first l)))
				(fourth (second (first l))))
			  (command-bindings (rest l))))
	((rest) (cons (list (first (second (first l))) ''())
		      (command-bindings (rest l))))
	(else (fuck-up)))))
  (define (command-keyword-argument-parser l)
   (cons
    `(let loop ()
      (unless (null? arguments)
       (cond
	,@(let loop ((l l))
	   (if (null? l)
	       '(((string=? (first arguments) "-usage") (usage)))
	       (case (first (first l))
		((any-number at-least-one)
		 (append
		  (map
		   (lambda (l)
		    `((string=? (first arguments)
				,(string-append "-" (first l)))
		      (set! arguments (rest arguments))
		      (set! ,(second l) #t)
		      ,@(reduce
			 append
			 (map
			  (lambda (l)
			   `((when (null? arguments) (usage))
			     (set! ,(first l)
				   (cons (,(third l) (first arguments) usage)
					 ,(first l)))
			     (set! arguments (rest arguments))))
			  (rest (rest l)))
			 '())
		      (loop)))
		   (rest (first l)))
		  (loop (rest l))))
		((at-most-one exactly-one)
		 (append
		  (map
		   (lambda (l1)
		    `((string=? (first arguments)
				,(string-append "-" (first l1)))
		      (set! arguments (rest arguments))
		      (when (or ,@(map second (rest (first l)))) (usage))
		      (set! ,(second l1) #t)
		      ,@(reduce
			 append
			 (map (lambda (l)
			       `((when (null? arguments) (usage))
				 (set! ,(first l)
				       (,(third l) (first arguments) usage))
				 (set! arguments (rest arguments))))
			      (rest (rest l1)))
			 '())
		      (loop)))
		   (rest (first l)))
		  (loop (rest l))))
		((required optional rest) (loop (rest l)))
		(else (fuck-up))))))))
    (let loop ((l l))
     (if (null? l)
	 '()
	 (case (first (first l))
	  ((at-least-one exactly-one)
	   (cons `(unless (or ,@(map second (rest (first l)))) (usage))
		 (loop (rest l))))
	  ((at-most-one any-number required optional rest) (loop (rest l)))
	  (else (fuck-up)))))))
  (define (command-positional-argument-parser l)
   (let loop ((l l))
    (if (null? l)
	'((unless (null? arguments) (usage)))
	(case (first (first l))
	 ((any-number at-least-one at-most-one exactly-one) (loop (rest l)))
	 ((required)
	  (append
	   `((when (null? arguments) (usage))
	     (set! ,(first (second (first l)))
		   (,(third (second (first l))) (first arguments) usage))
	     (set! arguments (rest arguments)))
	   (loop (rest l))))
	 ((optional)
	  (cons `(unless (null? arguments)
		  (set! ,(first (second (first l)))
			(,(third (second (first l))) (first arguments) usage))
		  (set! arguments (rest arguments)))
		(loop (rest l))))
	 ((rest)
	  `((let loop ()
	     (unless (null? arguments)
	      (set! ,(first (second (first l)))
		    (cons (,(third (second (first l))) (first arguments) usage)
			  ,(first (second (first l)))))
	      (set! arguments (rest arguments))
	      (loop)))))
	 (else (fuck-up))))))
  (unless (and (list? form)
	       (>= (length form) 2)
	       (valid-command-arguments? (second form)))
   (error 'define-command "Improper DEFINE-COMMAND: ~s" form))
  `(define (,(first (second form)) arguments)
    (define (string-argument string usage) string)
    (define (integer-argument string usage)
     (let ((integer (string->number string)))
      (unless (integer? integer) (usage))
      integer))
    (define (real-argument string usage)
     (let ((real (string->number string)))
      (unless (real? real) (usage))
      real))
    (let ((program (first arguments)))
     (define (usage)
      (format
       #t
       ,(string-append "usage: ~a" (command-usage (rest (second form))) "~%")
       program)
      (exit -1))
     (set! arguments (rest arguments))
     (let ,(command-bindings (rest (second form)))
      ,@(command-keyword-argument-parser (rest (second form)))
      ,@(command-positional-argument-parser (rest (second form)))
      ,@(rest (rest form)))))))

(define pp write)			;added
(define (remq! x l) (panic "REMQ! is not (yet) implemented")) ;added

;;; System Conditionalization

;;; note: The following can't use TMP since the variable *TMP* might not be
;;;       initialized yet.

(define *cpu-type* #f)

(define (cpu-type)
 (unless *cpu-type*
  (system "uname -m >/tmp/QobiScheme.tmp")
  (set! *cpu-type* (first (read-file "/tmp/QobiScheme.tmp")))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *cpu-type*)

(define *os-type* #f)

(define (os-type)
 (unless *os-type*
  (system "uname -s >/tmp/QobiScheme.tmp")
  (set! *os-type* (first (read-file "/tmp/QobiScheme.tmp")))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *os-type*)

(define *os-version* #f)

(define (os-version)
 (unless *os-version*
  (system "uname -r >/tmp/QobiScheme.tmp")
  (set! *os-version* (first (read-file "/tmp/QobiScheme.tmp")))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *os-version*)

(define *os-major-version* #f)

(define (os-major-version)
 (unless *os-major-version*
  (system "uname -r|cut -f 1 -d. >/tmp/QobiScheme.tmp")
  (set! *os-major-version*
	(string->number (first (read-file "/tmp/QobiScheme.tmp"))))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *os-major-version*)

(define *os-minor-version* #f)

(define (os-minor-version)
 (unless *os-minor-version*
  (system "uname -r|cut -f 2 -d. >/tmp/QobiScheme.tmp")
  (set! *os-minor-version*
	(string->number (first (read-file "/tmp/QobiScheme.tmp"))))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *os-minor-version*)

(define *os-sub-version* #f)

(define (os-sub-version)
 (unless *os-sub-version*
  (system "uname -r|cut -f 3 -d. >/tmp/QobiScheme.tmp")
  (set! *os-sub-version*
	(string->number (first (read-file "/tmp/QobiScheme.tmp"))))
  (system "rm -f /tmp/QobiScheme.tmp"))
 *os-sub-version*)

;;; Sugar

;;; removed: EVAL-WHEN

(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define (fifth x) (car (cddddr x)))
(define (sixth x) (cadr (cddddr x)))
(define (seventh x) (caddr (cddddr x)))
(define (eighth x) (cadddr (cddddr x)))
(define (ninth x) (car (cddddr (cddddr x))))
(define (tenth x) (cadr (cddddr (cddddr x))))
(define (eleventh x) (caddr (cddddr (cddddr x))))
(define (twelfth x) (cadddr (cddddr (cddddr x))))
(define rest cdr)			;changed

(define (last x) (if (null? (rest x)) (first x) (last (rest x))))

(define (sqr x) (* x x))

(define (xor a b) (if a (not b) b))

(define (nan? x) (not (= x x)))

;;; removed: WHILE

;;; changed: This is just a stub.
(define *panic?* #t)

(define *program* "")			;changed

;;; removed: PANIC

(define (fuck-up) (panic "This shouldn't happen"))

;;; removed: USAGE

(define (compose . fs)
 (if (null? fs)
     identity
     (lambda (x) ((apply compose (rest fs)) ((first fs) x)))))

(define (rounded-number->string x . digits-of-precision)
 (if (null? digits-of-precision)
     (number->string (inexact->exact (round x)))
     (let* ((digits (first digits-of-precision))
	    (factor (expt 10.0 digits))
	    (n (abs (inexact->exact (round (* x factor)))))
	    (s (number->string n))
	    (l (string-length s))
	    (rs (if (< n factor)
		    (string-append "0."
				   (make-string (- digits l) #\0)
				   s)
		    (string-append (substring s 0 (- l digits))
				   "."
				   (substring s (- l digits) l)))))
      (if (< x 0) (string-append "-" rs) rs))))

(define (number->string-of-length number length)
 (let ((string (number->string number)))
  (string-append (make-string (- length (string-length string)) #\space)
		 string)))

(define (number->padded-string-of-length number length)
 (when (negative? number) (fuck-up))
 (let ((string (number->string number)))
  (string-append (make-string (- length (string-length string)) #\0) string)))

(define (number->string-of-length-and-precision number length precision)
 (let* ((negative? (negative? number))
	(integer-part (inexact->exact (floor (abs number))))
	(fraction-part
	 (inexact->exact
	  (floor (* (expt 10 precision) (- (abs number) integer-part)))))
	(integer-part-string (number->string integer-part))
	(fraction-part-string (number->string fraction-part)))
  (if negative?
      (string-append
       (make-string
	(- length (string-length integer-part-string) 2 precision) #\space)
       "-"
       integer-part-string
       "."
       (make-string (- precision (string-length fraction-part-string)) #\0)
       fraction-part-string)
      (string-append
       (make-string
	(- length (string-length integer-part-string) 1 precision) #\space)
       integer-part-string
       "."
       (make-string (- precision (string-length fraction-part-string)) #\0)
       fraction-part-string))))

(define (getenv string)
 ;; changed
 (if (zero? (c-getenv string)) #f (c-getenv string)))

(define (archive-date)
 (rm (tmp "archive-date"))
 (system (format #f "archive-date >~a" (tmp "archive-date")))
 (let ((archive-date (read-file (tmp "archive-date"))))
  (rm (tmp "archive-date"))
  (first archive-date)))

;;; Structures

;;; removed: DEFINE-STRUCTURE

;;; removed: DEFINE-STRUCTURE-INTERNAL

;;; Sequences

(define (list-set! l i x)
 (if (zero? i) (set-car! l x) (list-set! (cdr l) (- i 1) x)))

(define (list-insert l i x)
 (if (zero? i)
     (cons x l)
     (cons (first l) (list-insert (rest l) (- i 1) x))))

(define (list-remove l i)
 (if (zero? i) (rest l) (cons (first l) (list-remove (rest l) (- i 1)))))

(define (list-replace l i x)
 (if (zero? i)
     (cons x (rest l))
     (cons (first l) (list-replace (rest l) (- i 1) x))))

(define (but-last x) (reverse (rest (reverse x))))

;;; removed: SUBLIST

;;; removed: SUBVECTOR

;;; removed: EVAL-WHEN

(define (reduce f l i)
 (cond ((null? l) i)
       ((null? (rest l)) (first l))
       (else (let loop ((l (rest l)) (c (first l)))
	      (if (null? l) c (loop (rest l) (f c (first l))))))))

(define (reduce-n f n i)
 (let loop ((i 0) (c i)) (if (>= i n) c (loop (+ i 1) (f c i)))))

(define (reduce-vector f v i)
 (let ((n (vector-length v)))
  (cond ((zero? n) i)
	((= n 1) (vector-ref v 0))
	(else (let loop ((i 1) (c (vector-ref v 0)))
	       (if (= i n) c (loop (+ i 1) (f c (vector-ref v i)))))))))

(define (sum f n)
 (let loop ((n (- n 1)) (c 0))
  (if (negative? n) c (loop (- n 1) (+ c (f n))))))

(define (product f n)
 (let loop ((n (- n 1)) (c 1))
  (if (negative? n) c (loop (- n 1) (* c (f n))))))

(define (factorial n) (product (lambda (i) (+ i 1)) n))

(define (choose n m) (product (lambda (i) (/ (+ i n (- m) 1) (+ i 1))) m))

(define (some p l . &rest)
 (let loop ((l l) (&rest &rest))
  (and (not (null? l))
       (or (apply p (first l) (map first &rest))
	   (loop (rest l) (map rest &rest))))))

(define (some-n p n)
 (let loop ((i 0)) (and (< i n) (or (p i) (loop (+ i 1))))))

(define (some-vector p v . &rest)
 (let loop ((i 0))
  (and (< i (vector-length v))
       (or (apply p
		  (vector-ref v i)
		  (map (lambda (v) (vector-ref v i)) &rest))
	   (loop (+ i 1))))))

;;; removed: EVAL-WHEN

(define (every p l . &rest)
 (let loop ((l l) (&rest &rest))
  (or (null? l)
      (and (apply p (first l) (map first &rest))
	   (loop (rest l) (map rest &rest))))))

(define (every-n p n)
 (let loop ((i 0)) (or (>= i n) (and (p i) (loop (+ i 1))))))

(define (every-vector p v . &rest)
 (let loop ((i 0))
  (or (>= i (vector-length v))
      (and (apply p
		  (vector-ref v i)
		  (map (lambda (v) (vector-ref v i)) &rest))
	   (loop (+ i 1))))))

(define (one p l . &rest)
 (let loop ((l l) (&rest &rest))
  (and (not (null? l))
       (if (apply p (first l) (map first &rest))
	   (let loop ((l (rest l)) (&rest (map rest &rest)))
	    (or (null? l)
		(and (not (apply p (first l) (map first &rest)))
		     (loop (rest l) (map rest &rest)))))
	   (loop (rest l) (map rest &rest))))))

(define (one-n p n)
 (let loop ((i 0))
  (and (< i n)
       (if (p i)
	   (let loop ((i (+ i 1)))
	    (or (>= i n) (and (not (p i)) (loop (+ i 1)))))
	   (loop (+ i 1))))))

(define (one-vector p v . &rest)
 (let loop ((i 0))
  (and (< i (vector-length v))
       (if (apply p
		  (vector-ref v i)
		  (map (lambda (v) (vector-ref v i)) &rest))
	   (let loop ((i (+ i 1)))
	    (or (>= i (vector-length v))
		(and (not (apply p
				 (vector-ref v i)
				 (map (lambda (v) (vector-ref v i)) &rest)))
		     (loop (+ i 1)))))
	   (loop (+ i 1))))))

(define (for-each-indexed f l)
 (let loop ((i 0) (l l))
  (unless (null? l) (f (first l) i) (loop (+ i 1) (rest l)))))

(define (for-each-n f n)
 (let loop ((i 0)) (when (< i n) (f i) (loop (+ i 1)))))

(define (for-each-from-a-up-to-b f a b)
 (let loop ((i a)) (when (< i b) (f i) (loop (+ i 1)))))

(define (for-each-n-decreasing f n)
 (when (> n 0) (let ((i (- n 1))) (f i) (for-each-n-decreasing f i))))

(define (for-each-vector f v . &rest)
 (for-each-n
  (lambda (i)
   (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) &rest)))
  (vector-length v)))

;;; removed: EVAL-WHEN

(define (map-indexed f l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((i 0) (l l) (c '()))
  (if (null? l)
      (reverse c)
      (loop (+ i 1) (rest l) (cons (f (first l) i) c)))))

(define (map-n f n)
 ;; needs work: To eliminate REVERSE.
 (let loop ((i 0) (c '()))
  (if (< i n) (loop (+ i 1) (cons (f i) c)) (reverse c))))

(define (map-vector f v . &rest)
 ;; needs work: Won't work correctly when F is nondeterministic.
 (let ((u (make-vector (vector-length v))))
  (for-each-n
   (lambda (i)
    (vector-set!
     u i
     (apply f (vector-ref v i) (map (lambda (v) (vector-ref v i)) &rest))))
   (vector-length v))
  u))

(define (map-n-vector f n)
 (let ((v (make-vector n)))
  (let loop ((i 0))
   (when (< i n)
    (vector-set! v i (f i))
    (loop (+ i 1))))
  v))

(define (enumerate n)
 (let loop ((i (- n 1)) (c '()))
  (if (>= i 0) (loop (- i 1) (cons i c)) c)))

(define (enumerate-vector n)
 (let ((v (make-vector n)))
  (for-each-n (lambda (i) (vector-set! v i i)) n)
  v))

(define (memp p x l)
 (cond ((null? l) #f) ((p x (first l)) l) (else (memp p x (rest l)))))

(define (assp p x alist)
 (and (not (null? alist))
      (if (p x (car (first alist))) (first alist) (assp p x (rest alist)))))

(define (pairwise? p l)
 (or (null? l)
     (let loop ((l1 l) (l2 (rest l)))
      ;; needs work: To make tail recursive.
      (or (null? l2)
	  (and (p (first l1) (first l2)) (loop (rest l1) (rest l2)))))))

(define (adjoinq x l) (if (memq x l) l (cons x l)))

(define (adjoinv x l) (if (memv x l) l (cons x l)))

(define (adjoin x l) (if (member x l) l (cons x l)))

(define (adjoinp p x l) (if (memp p x l) l (cons x l)))

(define (removeq x l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((eq? x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (removev x l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((eqv? x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (removep p x l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p x (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (remove-if p l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p (first l)) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (remove-if-not p l)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l l) (c '()))
  (cond ((null? l) (reverse c))
	((p (first l)) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (positionq x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((eq? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (positionv x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((eqv? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (position x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((equal? x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (positionp p x l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p x (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (position-if p l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p (first l)) i)
	(else (loop (rest l) (+ i 1))))))

(define (position-if-not p l)
 (let loop ((l l) (i 0))
  (cond ((null? l) #f)
	((p (first l)) (loop (rest l) (+ i 1)))
	(else i))))

(define (findq x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((eq? x (first l)) (first l))
	(else (loop (rest l))))))

(define (findv x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((eqv? x (first l)) (first l))
	(else (loop (rest l))))))

(define (find x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((equal? x (first l)) (first l))
	(else (loop (rest l))))))

(define (findp p x l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((p x (first l)) (first l))
	(else (loop (rest l))))))

(define (find-if p l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((p (first l)) (first l))
	(else (loop (rest l))))))

(define (find-if-not p l)
 (let loop ((l l))
  (cond ((null? l) #f)
	((p (first l)) (loop (rest l)))
	(else (first l)))))

(define (countq x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((eq? x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (countv x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((eqv? x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (count x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((equal? x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (countp p x l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p x (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (count-if p l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p (first l)) (loop (rest l) (+ c 1)))
	(else (loop (rest l) c)))))

(define (count-if-not p l)
 (let loop ((l l) (c 0))
  (cond ((null? l) c)
	((p (first l)) (loop (rest l) c))
	(else (loop (rest l) (+ c 1))))))

(define (subsetq? x y) (every (lambda (xe) (memq xe y)) x))

(define (subsetv? x y) (every (lambda (xe) (memv xe y)) x))

(define (subset? x y) (every (lambda (xe) (member xe y)) x))

(define (subsetp? p x y) (every (lambda (xe) (memp p xe y)) x))

(define (set-equalq? x y) (and (subsetq? x y) (subsetq? y x)))

(define (set-equalv? x y) (and (subsetv? x y) (subsetv? y x)))

(define (set-equal? x y) (and (subset? x y) (subset? y x)))

(define (set-equalp? p x y) (and (subsetp? p x y) (subsetp? p y x)))

(define (unionq x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((memq (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (unionv x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((memv (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (union x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((member (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (unionp p x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (append (reverse c) y))
	((memp p (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (intersectionq x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memq (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (intersectionv x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memv (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (intersection x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((member (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (intersectionp p x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memp p (first l) y) (loop (rest l) (cons (first l) c)))
	(else (loop (rest l) c)))))

(define (set-differenceq x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memq (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (set-differencev x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memv (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (set-difference x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((member (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (set-differencep p x y)
 ;; needs work: To eliminate REVERSE.
 (let loop ((l x) (c '()))
  (cond ((null? l) (reverse c))
	((memp p (first l) y) (loop (rest l) c))
	(else (loop (rest l) (cons (first l) c))))))

(define (remove-duplicatesq x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((memq (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (remove-duplicatesv x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((memv (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (remove-duplicates x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((member (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (remove-duplicatesp p x)
 ;; needs work: To eliminate REVERSE.
 (let loop ((x x) (c '()))
  (cond ((null? x) (reverse c))
	((memp p (first x) c) (loop (rest x) c))
	(else (loop (rest x) (cons (first x) c))))))

(define (equivalence-classesq x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (equivalence-classesq (rest x)))
	    (z (find-if (lambda (w) (eq? y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (equivalence-classesv x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (equivalence-classesv (rest x)))
	    (z (find-if (lambda (w) (eqv? y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (equivalence-classes x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (equivalence-classes (rest x)))
	    (z (find-if (lambda (w) (equal? y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (transitive-equivalence-classesp p x)
 ;; needs work: To make tail recursive.
 (if (null? x)
     '()
     (let* ((y (first x))
	    (x (transitive-equivalence-classesp p (rest x)))
	    (z (find-if (lambda (w) (p y (first w))) x)))
      (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))

(define (equivalence-classesp p x)
 ;; This wrapper is necessary since P may not be transitive.
 (define (equivalence-classesp p x)
  ;; needs work: To make tail recursive.
  (if (null? x)
      '()
      (let* ((y (first x))
	     (x (equivalence-classesp p (rest x)))
	     (z (find-if (lambda (w) (some (lambda (v) (p y v)) w)) x)))
       (if z (cons (cons y z) (removeq z x)) (cons (list y) x)))))
 (let loop ((c (map list x)))
  (let ((d (map (lambda (z) (reduce append z '()))
		(equivalence-classesp
		 (lambda (x y) (some (lambda (xe) (memp p xe y)) x)) c))))
   (if (= (length d) (length c)) d (loop d)))))

(define (topological-sort p l)
 (let loop ((l l) (c '()))
  (if (null? l)
      (reverse c)
      (let ((x (find-if
		(lambda (x1)
		 (not (some (lambda (x2) (and (not (eq? x2 x1)) (p x2 x1)))
			    l)))
		l)))
       (unless x (fuck-up))
       (loop (removeq x l) (cons x c))))))

(define (every-other list)
 (cond ((null? list) '())
       ((null? (rest list)) list)
       (else (cons (first list) (every-other (rest (rest list)))))))

(define (merge list1 list2 predicate key)
 (cond ((null? list1) list2)
       ((null? list2) list1)
       ((predicate (key (first list1)) (key (first list2)))
	(cons (first list1) (merge (rest list1) list2 predicate key)))
       (else (cons (first list2) (merge list1 (rest list2) predicate key)))))

(define (sort list predicate key)
 (if (or (null? list) (null? (rest list)))
     list
     (merge (sort (every-other list) predicate key)
	    (sort (every-other (rest list)) predicate key)
	    predicate
	    key)))

(define (minp p l)
 (when (null? l) (fuck-up))
 (let loop ((x (first l)) (l (rest l)))
  (if (null? l) x (loop (if (p x (first l)) x (first l)) (rest l)))))

(define (unionvt x y) (if (or (eq? x #t) (eq? y #t)) #t (unionv x y)))

(define (intersectionvt x y)
 (cond ((eq? x #t) y) ((eq? y #t) x) (else (intersectionv x y))))

(define (set-differencevt x y)
 (cond ((eq? y #t) '()) ((eq? x #t) x) (else (set-differencev x y))))

(define (subsetvt? x y)
 (cond ((eq? y #t) #t)
       ((eq? x #t) #f)
       (else (every (lambda (xe) (memq xe y)) x))))

(define (lexicographically<? <? =?)
 ;; note: There is a bug in Scheme->C which doesn't allow < and = to shadow
 ;;       the global bindings which is why these are named <? and =?.
 ;; note: This fixes a bug in the Stalin version of QobiScheme.
 (lambda (x y)
  (let loop ((x x) (y y))
   (and (not (null? y))
	(or (null? x)
	    (<? (first x) (first y))
	    (and (=? (first x) (first y)) (loop (rest x) (rest y))))))))

(define (minimal-membersp <? =? l)
 ;; note: There is a bug in Scheme->C which doesn't allow < and = to shadow
 ;;       the global bindings which is why these are named <? and =?.
 (when (null? l) (fuck-up))
 (let loop ((xs (list (first l))) (l (rest l)))
  (if (null? l)
      xs
      (loop (cond ((<? (first l) (first xs)) (list (first l)))
		  ((=? (first l) (first xs)) (cons (first l) xs))
		  (else	xs))
	    (rest l)))))

;;; Memoization

(define-structure entry arguments continuations results)

(define (memoize f)
 (let ((cache '()))
  (lambda arguments
   ;; removed: comment
   (call-with-current-continuation
    (lambda (continuation)
     (let ((entry (find-if (lambda (e) (equal? arguments (entry-arguments e)))
			   cache)))
      (cond
       (entry (set-entry-continuations!
	       entry (cons continuation (entry-continuations entry)))
	      (a-member-of (entry-results entry)))
       (else (set! entry (make-entry arguments (list continuation) '()))
	     (set! cache (cons entry cache))
	     (let ((result (apply f arguments)))
	      (set-entry-results! entry (cons result (entry-results entry)))
	      ((a-member-of (entry-continuations entry)) result))))))))))

;;; Strings

(define (prefix? prefix string)
 (and (<= (string-length prefix) (string-length string))
      (string=? prefix (substring string 0 (string-length prefix)))))

;;; removed: STRING-REVERSE

(define (suffix? suffix string)
 (prefix? (string-reverse suffix) (string-reverse string)))

(define (directory-prefix? prefix string)
 (or (string=? prefix string) (prefix? (string-append prefix "/") string)))

(define (string-downcase string)
 (list->string (map char-downcase (string->list string))))

(define (string-upcase string)
 (list->string (map char-upcase (string->list string))))

(define (symbol-downcase symbol)
 (string->symbol (string-downcase (symbol->string symbol))))

(define (pad-left string n)
 (string-append (make-string (- n (string-length string)) #\space) string))

(define (pad-right string n)
 (string-append string (make-string (- n (string-length string)) #\space)))

(define (substring? s1 s2)
 (let ((n (string-length s1)))
  (some-n (lambda (i)
	   (every-n (lambda (j)
		     (char=? (string-ref s1 j) (string-ref s2 (+ j i))))
		    n))
	  (+ (- (string-length s2) n) 1))))

(define (substring-ci? s1 s2)
 (let ((n (string-length s1)))
  (some-n (lambda (i)
	   (every-n (lambda (j)
		     (char-ci=? (string-ref s1 j) (string-ref s2 (+ j i))))
		    n))
	  (+ (- (string-length s2) n) 1))))

(define (slashify string)
 (let loop ((characters (string->list string)) (result '()))
  (cond
   ((null? characters) (list->string (reverse result)))
   ((char=? (first characters) #\\)
    (loop (rest characters) (cons #\\ (cons #\\ result))))
   ((char=? (first characters) #\")
    (loop (rest characters) (cons #\" (cons #\\ result))))
   ;; note: This is not really legitimate.
   ((or (char<? (first characters) #\space)
	(char>=? (first characters) (integer->char 127)))
    (loop (rest characters)
	  (cons (integer->char
		 (+ (bit-and (char->integer (first characters)) 7)
		    (char->integer #\0)))
		(cons (integer->char
		       (+ (bit-and
			   (quotient (char->integer (first characters)) 8) 7)
			  (char->integer #\0)))
		      (cons
		       (integer->char
			(+ (bit-and
			    (quotient (char->integer (first characters)) 64) 7)
			   (char->integer #\0)))
		       (cons #\\ result))))))
   (else (loop (rest characters) (cons (first characters) result))))))

(define (string-insert-character character)
 (lambda (string position)
  (list (string-append (substring string 0 position)
		       (list->string (list character))
		       (substring string position (string-length string)))
	(+ position 1))))

(define (string-beginning-of-line string position)
 (list string 0))

(define (string-backward-char string position)
 (when (zero? position) (abort))
 (list string (- position 1)))

(define (string-delete-char string position)
 (when (= position (string-length string)) (abort))
 (list (string-append
	(substring string 0 position)
	(substring string (+ position 1) (string-length string)))
       position))

(define (string-end-of-line string position)
 (list string (string-length string)))

(define (string-forward-char string position)
 (when (= position (string-length string)) (abort))
 (list string (+ position 1)))

(define (string-kill-line string position)
 (list (substring string 0 position) position))

(define (string-backward-delete-char string position)
 (when (zero? position) (abort))
 (list (string-append (substring string 0 (- position 1))
		      (substring string position (string-length string)))
       (- position 1)))

(define (char-alphanumeric? char)
 (or (char-alphabetic? char) (char-numeric? char)))

(define (beginning-of-word? string position)
 (or (zero? position)
     (and (not (= position (string-length string)))
	  (not (char-alphanumeric? (string-ref string (- position 1))))
	  (char-alphanumeric? (string-ref string position)))))

(define (end-of-word? string position)
 (or (= position (string-length string))
     (and (not (zero? position))
	  (char-alphanumeric? (string-ref string (- position 1)))
	  (not (char-alphanumeric? (string-ref string position))))))

(define (string-backward-word string position)
 (when (zero? position) (abort))
 (let loop ((position (- position 1)))
  (if (beginning-of-word? string position)
      (list string position)
      (loop (- position 1)))))

(define (string-kill-word string position)
 (when (= position (string-length string)) (abort))
 (list (string-append (substring string 0 position)
		      (substring string
				 (second (string-forward-word string position))
				 (string-length string)))
       position))

(define (string-forward-word string position)
 (when (= position (string-length string)) (abort))
 (let loop ((position (+ position 1)))
  (if (end-of-word? string position)
      (list string position)
      (loop (+ position 1)))))

(define (string-backward-kill-word string position)
 (when (zero? position) (abort))
 (let ((new-position (second (string-backward-word string position))))
  (list (string-append (substring string 0 new-position)
		       (substring string position (string-length string)))
	new-position)))

;;; Fields

(define (number-of-fields string)
 (let loop ((n 0) (chars (string->list string)))
  (if (null? chars)
      n
      (if (char-whitespace? (first chars))
	  (loop n (rest chars))
	  (loop (+ n 1)
		(let loop ((chars chars))
		 (if (or (null? chars) (char-whitespace? (first chars)))
		     chars
		     (loop (rest chars)))))))))

(define (field-ref string n)
 (let loop ((n n) (chars (string->list string)))
  (if (char-whitespace? (first chars))
      (loop n (rest chars))
      (if (zero? n)
	  (let loop ((chars chars) (field '()))
	   (if (or (null? chars) (char-whitespace? (first chars)))
	       (list->string (reverse field))
	       (loop (rest chars) (cons (first chars) field))))
	  (loop (- n 1)
		(let loop ((chars chars))
		 (if (char-whitespace? (first chars))
		     chars
		     (loop (rest chars)))))))))

(define (fields string)
 (map-n (lambda (i) (field-ref string i)) (number-of-fields string)))

;;; Line and Whole-File I/O

(define (read-line . port)
 (if (null? port) (set! port (current-input-port)) (set! port (first port)))
 (let loop ((chars '()))
  (let ((char (read-char port)))
   (if (eof-object? char)
       (if (null? chars) char (list->string (reverse chars)))
       (if (char=? char #\newline)
	   (list->string (reverse chars))
	   (loop (cons char chars)))))))

(define (read-file pathname)
 (if (string=? pathname "-")
     (let loop ((lines '()) (line (read-line)))
      (if (eof-object? line)
	  (reverse lines)
	  (loop (cons line lines) (read-line))))
     (call-with-input-file pathname
      (lambda (port)
       (let loop ((lines '()) (line (read-line port)))
	(if (eof-object? line)
	    (reverse lines)
	    (loop (cons line lines) (read-line port))))))))

(define (write-file lines pathname)
 (if (string=? pathname "-")
     (for-each (lambda (line) (display line) (newline)) lines)
     (call-with-output-file pathname
      (lambda (port)
       (for-each (lambda (line) (display line port) (newline port)) lines)))))

(define (read-object-from-file pathname)
 (if (string=? pathname "-") (read) (call-with-input-file pathname read)))

(define (write-object-to-file object pathname)
 (cond ((string=? pathname "-") (pp object) (newline))
       (else (call-with-output-file pathname
	      (lambda (port) (pp object port) (newline port))))))

(define (read-from-string string)
 (rm (tmp "cdslib.tmp"))
 (write-file (list string) (tmp "cdslib.tmp"))
 (let ((input (call-with-input-file (tmp "cdslib.tmp") read)))
  (rm (tmp "cdslib.tmp"))
  input))

;;; Pathnames

;;; needs work: missing notions: ., .., foo~, foo.~n~, .foo, #foo#, /foo/, and
;;;             foo/

(define (has-directory? pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (let loop ((l (reverse (string->list pathname))))
  (and (not (null? l)) (or (char=? (first l) #\/) (loop (rest l))))))

(define (directory pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (unless (has-directory? pathname) (panic "No directory"))
 (let ((l (string->list pathname)))
  (substring pathname 0 (- (length l) (positionv #\/ (reverse l)) 1))))

(define (strip-directory pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (if (has-directory? pathname)
     (let ((l (string->list pathname)))
      (substring pathname
		 (- (length l) (positionv #\/ (reverse l)))
		 (length l)))
     pathname))

(define (has-extension? pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (let loop ((l (reverse (string->list pathname))))
  (and (not (null? l))
       (not (char=? (first l) #\/))
       (or (char=? (first l) #\.) (loop (rest l))))))

(define (extension pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (unless (has-extension? pathname) (panic "No extension"))
 (substring pathname
	    (+ (positionv #\. (string->list pathname)) 1)
	    (string-length pathname)))

(define (strip-extension pathname)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (let loop ((l (reverse (string->list pathname))))
  (if (or (null? l) (char=? (first l) #\/))
      pathname
      (if (char=? (first l) #\.)
	  (list->string (reverse (rest l)))
	  (loop (rest l))))))

(define (default-extension pathname extension)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (if (has-extension? pathname)
     pathname
     (string-append pathname "." extension)))

(define (replace-extension pathname extension)
 (when (string=? pathname "-") (panic "Invalid pathname"))
 (string-append (strip-extension pathname) "." extension))

(define (format destination format-string . arguments)
 (cond
  ((output-port? destination)
   (let ((twiddle? #f)
	 (n (- (string-length format-string) 1)))
    (unless (negative? n)
     (let loop ((i 0))
      (let ((char (string-ref format-string i)))
       (cond (twiddle?
	      (case char
	       ((#\a #\A)
		(when (null? arguments) (panic "Too few FORMAT arguments"))
		(display (car arguments) destination)
		(set! arguments (cdr arguments)))
	       ((#\s #\S)
		(when (null? arguments) (panic "Too few FORMAT arguments"))
		(write (car arguments) destination)
		(set! arguments (cdr arguments)))
	       ((#\~) (write-char #\~ destination))
	       ((#\%) (newline destination))
	       (else (panic "Improper FORMAT directive")))
	      (set! twiddle? #f))
	     ((char=? char #\~) (set! twiddle? #t))
	     (else (write-char char destination)))
       (cond ((< i n) (loop (+ i 1)))
	     ((not (null? arguments)) (panic "Too many FORMAT arguments"))
	     (twiddle? (panic "Twiddle at end of FORMAT string"))))))))
  ((eq? destination #t)
   (let ((twiddle? #f)
	 (n (- (string-length format-string) 1)))
    (unless (negative? n)
     (let loop ((i 0))
      (let ((char (string-ref format-string i)))
       (cond (twiddle?
	      (case char
	       ((#\a #\A)
		(when (null? arguments) (panic "Too few FORMAT arguments"))
		(display (car arguments))
		(set! arguments (cdr arguments)))
	       ((#\s #\S)
		(when (null? arguments) (panic "Too few FORMAT arguments"))
		(write (car arguments))
		(set! arguments (cdr arguments)))
	       ((#\~) (write-char #\~))
	       ((#\%) (newline))
	       (else (panic "Improper FORMAT directive")))
	      (set! twiddle? #f))
	     ((char=? char #\~) (set! twiddle? #t))
	     (else (write-char char)))
       (cond ((< i n) (loop (+ i 1)))
	     ((not (null? arguments)) (panic "Too many FORMAT arguments"))
	     (twiddle? (panic "Twiddle at end of FORMAT string"))))))))
  ((eq? destination #f)
   (let ((twiddle? #f)
	 (result '())
	 (n (- (string-length format-string) 1)))
    (define (write-to-result x)
     (cond ((null? x)
	    (set! result (cons #\( result))
	    (set! result (cons #\) result)))
	   ((eq? x #t)
	    (set! result (cons #\# result))
	    (set! result (cons #\T result)))
	   ((not x)
	    (set! result (cons #\# result))
	    (set! result (cons #\F result)))
	   ((char? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\\ result))
	    ;; needs work: To handle other non printing characters.
	    (case x
	     ((#\newline)
	      (set! result (cons #\N result))
	      (set! result (cons #\e result))
	      (set! result (cons #\w result))
	      (set! result (cons #\l result))
	      (set! result (cons #\i result))
	      (set! result (cons #\n result))
	      (set! result (cons #\e result)))
	     ((#\space)
	      (set! result (cons #\S result))
	      (set! result (cons #\p result))
	      (set! result (cons #\a result))
	      (set! result (cons #\c result))
	      (set! result (cons #\e result)))
	     (else (set! result (cons x result)))))
	   ((number? x)
	    (for-each (lambda (c) (set! result (cons c result)))
		      (string->list (number->string x))))
	   ((input-port? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\* result))
	    (set! result (cons #\I result))
	    (set! result (cons #\N result))
	    (set! result (cons #\P result))
	    (set! result (cons #\U result))
	    (set! result (cons #\T result))
	    (set! result (cons #\- result))
	    (set! result (cons #\P result))
	    (set! result (cons #\O result))
	    (set! result (cons #\R result))
	    (set! result (cons #\T result))
	    (set! result (cons #\* result)))
	   ((output-port? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\* result))
	    (set! result (cons #\O result))
	    (set! result (cons #\U result))
	    (set! result (cons #\T result))
	    (set! result (cons #\P result))
	    (set! result (cons #\U result))
	    (set! result (cons #\T result))
	    (set! result (cons #\- result))
	    (set! result (cons #\P result))
	    (set! result (cons #\O result))
	    (set! result (cons #\R result))
	    (set! result (cons #\T result))
	    (set! result (cons #\* result)))
	   ((eof-object? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\* result))
	    (set! result (cons #\E result))
	    (set! result (cons #\O result))
	    (set! result (cons #\F result))
	    (set! result (cons #\- result))
	    (set! result (cons #\O result))
	    (set! result (cons #\B result))
	    (set! result (cons #\J result))
	    (set! result (cons #\E result))
	    (set! result (cons #\C result))
	    (set! result (cons #\T result))
	    (set! result (cons #\* result)))
	   ((symbol? x)
	    ;; needs work: Should slashify.
	    (let* ((x (symbol->string x))
		   (n (string-length x)))
	     (let loop ((i 0))
	      (when (< i n)
	       (set! result (cons (string-ref x i) result))
	       (loop (+ i 1))))))
	   ((procedure? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\* result))
	    (set! result (cons #\P result))
	    (set! result (cons #\R result))
	    (set! result (cons #\O result))
	    (set! result (cons #\C result))
	    (set! result (cons #\E result))
	    (set! result (cons #\D result))
	    (set! result (cons #\U result))
	    (set! result (cons #\R result))
	    (set! result (cons #\E result))
	    (set! result (cons #\* result)))
	   ((string? x)
	    (set! result (cons #\" result))
	    (let ((n (string-length x)))
	     (let loop ((i 0))
	      (when (< i n)
	       (when (or (char=? (string-ref x i) #\\)
			 (char=? (string-ref x i) #\")
			 (set! result (cons #\\ result)))
		(set! result (cons (string-ref x i) result))
		(loop (+ i 1))))))
	    (set! result (cons #\" result)))
	   ((pair? x)
	    (set! result (cons #\( result))
	    (let loop ((x x))
	     (cond ((null? (cdr x))
		    (write-to-result (car x)))
		   ((pair? (cdr x))
		    (write-to-result (car x))
		    (set! result (cons #\space result))
		    (loop (cdr x)))
		   (else (write-to-result (car x))
			 (set! result (cons #\space result))
			 (write-to-result (cdr x)))))
	    (set! result (cons #\) result)))
	   ((vector? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\( result))
	    (let ((n (vector-length x)))
	     (unless (zero? n)
	      (write-to-result (vector-ref x 0))
	      (let loop ((i 1))
	       (unless (= i n)
		(set! result (cons #\space result))
		(write-to-result (vector-ref x i))
		(loop (+ i 1))))))
	    (set! result (cons #\) result)))
	   (else (panic
		  "FORMAT with WRITE-methods is not (yet) implemented"))))
    (define (display-to-result x)
     (cond ((null? x)
	    (set! result (cons #\( result))
	    (set! result (cons #\) result)))
	   ((eq? x #t)
	    (set! result (cons #\# result))
	    (set! result (cons #\T result)))
	   ((not x)
	    (set! result (cons #\# result))
	    (set! result (cons #\F result)))
	   ((char? x)
	    (set! result (cons x result)))
	   ((number? x)
	    (for-each (lambda (c) (set! result (cons c result)))
		      (string->list (number->string x))))
	   ((input-port? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\* result))
	    (set! result (cons #\I result))
	    (set! result (cons #\N result))
	    (set! result (cons #\P result))
	    (set! result (cons #\U result))
	    (set! result (cons #\T result))
	    (set! result (cons #\- result))
	    (set! result (cons #\P result))
	    (set! result (cons #\O result))
	    (set! result (cons #\R result))
	    (set! result (cons #\T result))
	    (set! result (cons #\* result)))
	   ((output-port? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\* result))
	    (set! result (cons #\O result))
	    (set! result (cons #\U result))
	    (set! result (cons #\T result))
	    (set! result (cons #\P result))
	    (set! result (cons #\U result))
	    (set! result (cons #\T result))
	    (set! result (cons #\- result))
	    (set! result (cons #\P result))
	    (set! result (cons #\O result))
	    (set! result (cons #\R result))
	    (set! result (cons #\T result))
	    (set! result (cons #\* result)))
	   ((eof-object? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\* result))
	    (set! result (cons #\E result))
	    (set! result (cons #\O result))
	    (set! result (cons #\F result))
	    (set! result (cons #\- result))
	    (set! result (cons #\O result))
	    (set! result (cons #\B result))
	    (set! result (cons #\J result))
	    (set! result (cons #\E result))
	    (set! result (cons #\C result))
	    (set! result (cons #\T result))
	    (set! result (cons #\* result)))
	   ((symbol? x)
	    (let* ((x (symbol->string x))
		   (n (string-length x)))
	     (let loop ((i 0))
	      (when (< i n)
	       (set! result (cons (string-ref x i) result))
	       (loop (+ i 1))))))
	   ((procedure? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\* result))
	    (set! result (cons #\P result))
	    (set! result (cons #\R result))
	    (set! result (cons #\O result))
	    (set! result (cons #\C result))
	    (set! result (cons #\E result))
	    (set! result (cons #\D result))
	    (set! result (cons #\U result))
	    (set! result (cons #\R result))
	    (set! result (cons #\E result))
	    (set! result (cons #\* result)))
	   ((string? x)
	    (let ((n (string-length x)))
	     (let loop ((i 0))
	      (when (< i n)
	       (set! result (cons (string-ref x i) result))
	       (loop (+ i 1))))))
	   ((pair? x)
	    (set! result (cons #\( result))
	    (let loop ((x x))
	     (cond ((null? (cdr x))
		    (display-to-result (car x)))
		   ((pair? (cdr x))
		    (display-to-result (car x))
		    (set! result (cons #\space result))
		    (loop (cdr x)))
		   (else (display-to-result (car x))
			 (set! result (cons #\space result))
			 (display-to-result (cdr x)))))
	    (set! result (cons #\) result)))
	   ((vector? x)
	    (set! result (cons #\# result))
	    (set! result (cons #\( result))
	    (let ((n (vector-length x)))
	     (unless (zero? n)
	      (display-to-result (vector-ref x 0))
	      (let loop ((i 1))
	       (unless (= i n)
		(set! result (cons #\space result))
		(display-to-result (vector-ref x i))
		(loop (+ i 1))))))
	    (set! result (cons #\) result)))
	   (else (panic
		  "FORMAT with DISPLAY-methods is not (yet) implemented"))))
    (unless (negative? n)
     (let loop ((i 0))
      (let ((char (string-ref format-string i)))
       (cond (twiddle?
	      (case char
	       ((#\a #\A)
		(when (null? arguments) (panic "Too few FORMAT arguments"))
		(display-to-result (car arguments))
		(set! arguments (cdr arguments)))
	       ((#\s #\S)
		(when (null? arguments) (panic "Too few FORMAT arguments"))
		(write-to-result (car arguments))
		(set! arguments (cdr arguments)))
	       ((#\~) (set! result (cons #\~ result)))
	       ((#\%) (set! result (cons #\newline result)))
	       (else (panic "Improper FORMAT directive")))
	      (set! twiddle? #f))
	     ((char=? char #\~) (set! twiddle? #t))
	     (else (set! result (cons char result))))
       (cond ((< i n) (loop (+ i 1)))
	     ((not (null? arguments)) (panic "Too many FORMAT arguments"))
	     (twiddle? (panic "Twiddle at end of FORMAT string"))
	     (else (list->string (reverse result)))))))))
  (else (panic "Improper FORMAT destination"))))

(define (can-open-file-for-input? pathname) (file-read-access? pathname))

(define (panic . arguments) (apply format #t arguments) (newline) (exit -1))

;;; Tam V'Nishlam Shevah L'El Borei Olam
