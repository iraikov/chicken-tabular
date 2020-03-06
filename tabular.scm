;;
;; 
;;  Routines for parsing and printing tabular text data such as comma-
;;  and delimiter-separated values, as well as fixed-width column
;;  data.
;;
;;  Based in part on RFC 4180, "Common Format and MIME Type for
;;  Comma-Separated Values (CSV) Files", and on the Haskell Text.CSV
;;  module by Jaap Weel.
;;
;;  Differences with the RFC:
;;
;;   1) the RFC prescribes CRLF standard network line breaks, but many
;;   CSV files have platform-dependent line endings, so this library
;;   accepts a user-configurable sequence of CRs and LFs as a line
;;   break.
;;
;;   2) The format of header lines is exactly like a regular record
;;   and the presence of a header can only be determined from the mime
;;   type.  This library treats all lines as regular records, but
;;   allows the column names to be read from the first row via the
;;   column-names-header option.
;;
;;   3) The formal grammar specifies that fields can contain only
;;   certain US ASCII characters, but the specification of the MIME
;;   type allows for other character sets. This library allows all
;;   characters in fields, except for the field delimiter character,
;;   CRs and LFs in unquoted fields. This should make it possible to
;;   parse CSV files in any encoding, but it allows for characters
;;   such as tabs that the RFC may be interpreted to forbid even in
;;   non-US-ASCII character sets.
;;
;;   4) The delimiter character is specified by the user and can be
;;   a character other than comma, or an SRFI-14 character set.
;;
;;   5) The optional argument comment-char specifies that all lines
;;   that start with the given character should be dropped.
;;
;;   Copyright 2019 Ivan Raikov
;;
;;   This program is free software: you can redistribute it and/or
;;   modify it under the terms of the GNU General Public License as
;;   published by the Free Software Foundation, either version 3 of
;;   the License, or (at your option) any later version.
;;
;;   This program is distributed in the hope that it will be useful,
;;   but WITHOUT ANY WARRANTY; without even the implied warranty of
;;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;   General Public License for more details.
;;
;;   A full copy of the GPL license can be found at
;;   <http://www.gnu.org/licenses/>.


(module tabular

	(reader reader* writer record fwrecord)
	
	(import scheme (chicken base) (chicken format) (chicken string) (chicken io)
                matchable srfi-1 srfi-127
                (only utf8-srfi-13 string-concatenate string-take string-null?)
                (only utf8-srfi-14 char-set-contains?  ucs-range->char-set string->char-set 
                      char-set? char-set char-set-union char-set-complement char-set-difference
                      char-set:full char-set:ascii)
                (only regex regexp regexp-escape string-search string-substitute* string-split-fields)
                )



(define (string-generator str)
  (let ( (max+1 (string-length str))
         (sindex (make-parameter 0)) )
    (lambda ()
      (let ((i (sindex)))
        (cond ((< i max+1)
               (sindex (+ 1 i))
               (string-ref str i)
               )
              (else (eof-object))
              ))
      ))
  )
        
(define (string->lseq str)
  (let* ((gen (string-generator str))
         (value (gen)))
      (if (eof-object? value)
          '()
          (cons value gen))))
        
(define (port->lseq port)
  (let* ((gen (lambda () (read-char port)))
         (value (gen)))
      (if (eof-object? value)
          '()
          (cons value gen))))

(define (lseq-prefix lseq lst)
  (let loop ((lst lst) (ls lseq) (ax '()))
    (cond ((null? lst) (list ax ls))
          ((null? ls) #f)
          (else
           (if (char=? (car lst) (lseq-first ls))
               (loop (cdr lst) (lseq-rest ls) (cons (lseq-first ls) ax))))
          )))


(define (lseq-prefix-or-empty lseq lst)
  (or (and (null? lseq) `(() ,lseq))
      (let loop ((lst lst) (ls lseq) (ax '()))
        (cond ((null? lst) (list ax ls))
              ((null? ls) #f)
              (else
               (if (char=? (car lst) (lseq-first ls))
                   (loop (cdr lst) (lseq-rest ls) (cons (lseq-first ls) ax))))
              ))
      ))
  
(define (fold1 proc lst)
  (if (null? lst) #f
      (let ((seed (car lst)))
        (fold proc seed (cdr lst)))))

  
(define (many pred?)
  (lambda (s)
    (let loop ((lst (list)) (rst s))
      (cond ((null? rst)
             `(,(reverse lst) ,rst))
	    ((pred? (lseq-first rst))
             (loop (cons (lseq-first rst) lst) (lseq-rest rst)))
	    (else
             `(,(reverse lst) ,rst))))))
  
(define (until-prefix prefix)
  (let ((prefix-n (length prefix)))
    (lambda (s)
      (let loop ((lst (list)) (rst s))
        (cond ((null? rst)
               `(,(reverse lst) ,rst))
              ((lseq-prefix rst prefix)
               `(,(reverse lst) ,(lseq-take rst prefix-n)))
              (else
               (loop (cons (lseq-first rst) lst) (lseq-rest rst))))
        ))
    ))


(define (one pred?)
    (lambda (s)
      (cond ((null? s) #f)
             ((pred? (lseq-first s))
              `(,(lseq-first s) ,(lseq-rest s)))
             (else #f)
            )))

(define (one-cs cs)
  (one (lambda (c) (char-set-contains? cs c))))


(define (many-cs cs)
  (many (lambda (c) (char-set-contains? cs c))))


(define (alternatives f g) (lambda (s) (or (f s) (g s))))

(define (repetition f) 
  (lambda (s)
    (let loop ((res '()) (rst s))
      (match (f rst)
             ((a rst)
              (loop (cons a res) rst))
             (else `(,(reverse res) ,rst)))
      ))
  )
             
(define (fixed-width width s)
  (assert (>= width 0))
  (let loop ((n width) (ax '()) (ss s))
    (if (or (null? ss) (= n 0))
        `(,(list->string (reverse ax)) ,ss)
        (loop (- n 1) (cons (lseq-first ss) ax) (lseq-rest ss))
        ))
  )
          
(define (non-escaped delim)
  (let ((cs (char-set-complement
             (char-set-union
              (if (char? delim) (char-set delim) delim)
              (string->char-set "\n\r\""))
             )))
    (lambda (s)
      (match ((many-cs cs) s)
             ((a rst)
              `(,(list->string a) ,rst))
             (else #f)))
    ))


(define (escaped-dquote s)
  (match s
         ((#\" . _)
          (let ((rst (lseq-rest s)))
            (and (char=? (lseq-first rst) #\") `(#\" ,(lseq-rest rst)))))
         (else #f)))


(define textdata-cs
  (char-set-union
   (char-set-difference char-set:full char-set:ascii)
   (char-set-union (ucs-range->char-set #x20 #x22)
		   (ucs-range->char-set #x23 #x2D)
		   (ucs-range->char-set #x2D #x7F))))


(define (escaped delim)
  (let* ((cs (char-set-union (if (char? delim) (char-set delim) delim)
                             (char-set #\newline #\return)
                             textdata-cs))
         (es (repetition (alternatives escaped-dquote (one-cs cs)))))
    (lambda (s)
      (match s
             ((#\" . _)
              (match (es (lseq-rest s))
                     ((a (and rst (#\" . _)))
                      `(,(list->string a) 
                        ,(lseq-rest rst)))
                     (else #f)))
             (else #f))
      ))
  )
  

(define (field delim)
  (alternatives 
   (escaped delim)
   (non-escaped delim) 
   ))

  
(define (record delim na)
  
  (let ((delim-set (if (char? delim) (char-set delim) delim))
        (pfield (field delim)))

    (lambda (ls)
      (let loop ((ls ls) (fields '()))
        (match (pfield ls)
               ((a rst)
                (let ((va (or (and (string-null? a) na) a)))
                  (match rst
                         ((c . _)
                          (if (char-set-contains? delim-set c)
                              (loop (lseq-rest rst) (cons va fields))
                              `(,(reverse (cons va fields)) ,rst)))
                         (else `(,(reverse (cons va fields)) ,rst))))
                )
               (else `(,(reverse fields) ,ls))
               ))
      ))
  )

  
(define (fwrecord column-widths na)
    (lambda (ls)
      (let loop ((ls ls) (widths column-widths) (fields '()))
        (if (null? widths)
            `(,(reverse fields) ,ls)
            (match (fixed-width (car widths) ls)
                   ((a rst)
                    (let ((va (or (and (string-null? a) na) a)))
                      (match rst
                             ((c . _)
                              (loop rst (cdr widths) (cons va fields)))
                             (else `(,(reverse (cons va fields)) ,rst)))
                      ))
                   (else `(,(reverse fields) ,ls))
                   ))
        ))
    )



(define (check-delimiter d comment-char)
  (if (not (or (char? d) (char-set? d)))
      (error 'check-delimiter "delimiter is not a character or a character set"))
  (cond ((char? d)
	 (case d
	   ((#\newline #\return #\")
	    (error 
	     'parser
	     "delimiter character is one of newline, carriage return or quotation mark"))))
	((char-set? d)
	 (if (or (char-set-contains? d #\newline)
		 (char-set-contains? d #\return)
		 (char-set-contains? d #\"))
	    (error 
	     'check-delimiter
	     "delimiter character set includes newline, carriage return or quotation mark"))))
  (if (and (char? comment-char) (equal? d comment-char))
      (error 
       'check-delimiter
       "delimiter character cannot be the same as comment character"))
  )
      


 
(define *eof-object* (read (open-input-string "")))
(define (eof-object) *eof-object*)

;; TODO:
;; 5) fixed width columns

(define (reader input #!key (delimiter #\,) (eol "\n") (comment-char #f) (na #f)
                (column-names #f) (column-widths #f) (return-names #f) )
  (if (and delimiter column-widths)
      (error 'reader "both delimiter and column-widths are provided; only one or the other must be specified"))
  (if (not (or delimiter column-widths))
      (error 'reader "neither delimiter and column-widths are provided; one or the other must be specified"))
  (if (and column-widths column-names)
      (assert (= (length column-widths) (length column-names))))
  (if delimiter (check-delimiter delimiter comment-char))
  (let* ((precord (if delimiter (record delimiter na) (fwrecord column-widths na)))
         (eoll (if (string? eol) (string->list eol) eol))
         (next-line (until-prefix eoll))
         (ls   (cond ((port? input) (port->lseq input))
                     ((string? input) (string->lseq input))
                     (else (error 'reader "unknown input type")))))
    (match-let (((headers ls) (match column-names
                                     ('header (precord ls))
                                     ((and (? list?) lst) `(,column-names ,ls))
                                     (else `(#f ,ls)))))
               (let* (
                      (pf (lambda (ls)
                            (if (null? ls)
                                (eof-object)
                                (match-let (((rec rst) (precord ls)))
                                           (let ((rst (match (lseq-prefix-or-empty rst eoll)
                                                             ((prefix rst1) rst1)
                                                             (else (error 'reader "unable to parse record" rst)))))
                                             `(,rec ,rst))
                                           ))
                            ))
                      
                      (pfc (if (char? comment-char)
                               (lambda (ls)
                                 (if (null? ls)
                                     (eof-object)
                                     (let loop ((ls ls))
                                       (if (char=? (lseq-first ls) comment-char)
                                           (loop (cadr (next-line ls)))
                                           (pf ls))
                                       ))
                                 )
                               pf))
                      
                      (pfch (if (and return-names headers)
                                (compose (lambda (rec.rst) `(,(zip headers (car rec.rst)) ,(cadr rec.rst))) pfc)
                                pfc))
                      )
                 (values pfch ls)
                 ))
    ))


(define (reader* input #!key (delimiter #\,) (eol "\n") (comment-char #f) (na #f)
                (column-names #f) (column-widths #f) (return-names #f))
  (if (and delimiter column-widths)
      (error 'reader "both delimiter and column-widths are provided; only one or the other must be specified"))
  (if (not (or delimiter column-widths))
      (error 'reader "neither delimiter and column-widths are provided; one or the other must be specified"))
  (if (and column-widths column-names)
      (assert (= (length column-widths) (length column-names))))
  (if delimiter (check-delimiter delimiter comment-char))
  (let* ((precord (if delimiter (record delimiter na) (fwrecord column-widths na)))
         (eoll (if (string? eol) (string->list eol) eol))
         (next-line (until-prefix eoll))
         (ls  (make-parameter (cond ((port? input) (port->lseq input))
                                    ((string? input) (string->lseq input))
                                    (else (error 'reader* "unknown input type"))))))
    (let ((headers (match column-names
                          ('header (match-let (((rec rst) (precord (ls))))
                                              (ls rst)
                                              rec))
                          ((and (? list?) lst) `(,column-names ,ls))
                          (else `(#f ,ls)))))
      (let* (
             (pf (lambda ()
                   (if (null? (ls))
                       (eof-object)
                       (match-let (((rec rst) (precord (ls))))
                                  (let ((rst (match (lseq-prefix-or-empty rst eoll)
                                                    ((prefix rst1) rst1)
                                                    (else (error 'reader* "unable to parse record" (ls) rst)))))
                                    (ls rst)
                                    rec)))))
             
             (pfc (if (char? comment-char)
                      (lambda ()
                        (if (null? (ls))
                            (eof-object)
                            (let loop ((ls1 (ls)))
                              (if (char=? (lseq-first ls1) comment-char)
                                  (loop (cadr (next-line ls1)))
                                  (begin (ls ls1)
                                         (pf)))
                              )))
                      pf))
             
             (pfch (if (and return-names headers)
                       (compose (lambda (rec.rst) `(,(zip headers (car rec.rst)) ,(cadr rec.rst))) pfc)
                       pfc))
             )
        pfch
        ))
    ))


    
(define rx-newline (regexp "[^\r\n]+"))

(define (normalise-newlines s endline) 
  (string-concatenate (intersperse (string-split-fields rx-newline s) endline)))

(define rx-quote (regexp "\""))

(define (normalise-quotes s)  (string-substitute* s `((,rx-quote . "\"\""))))

(define (writer output #!key (delimiter #\,) (column-widths #f) (endline "\n") )

  (if (and delimiter column-widths)
      (error 'writer "both delimiter and column-widths are provided; only one or the other must be specified"))
  (if (not (or delimiter column-widths))
      (error 'writer "neither delimiter and column-widths are provided; one or the other must be specified"))
  (if delimiter (check-delimiter delimiter #f))

   (let* (
          (special-strs
           (map (compose regexp-escape ->string)
                (if delimiter
                    (list delimiter #\" #\newline #\return)
                    (list #\" #\newline #\return))))
          (rx-special
           (regexp (string-concatenate (intersperse special-strs "|"))))
          (format-field
           (lambda (str)
             (if (string-search rx-special str)
                 (string-append "\"" (normalise-newlines (normalise-quotes str) endline) "\"")
                 str)))
          (format-fields
           (if column-widths
               (lambda (ls) (map (lambda (fld wd) (string-take (format-field fld) wd)) ls column-widths ))
               (lambda (ls) (map format-field ls))))
          (format-record
           (if delimiter 
               (lambda (ls)
                 (and (pair? ls)
                      (string-concatenate
                       (append (intersperse (format-fields ls) (list->string (list delimiter)))
                               (list endline)))
                      ))
               (lambda (ls)
                 (and (pair? ls)
                      (string-concatenate
                       (append (format-fields ls) (list endline)))
                      ))
               ))
          (port (cond ((port? output) output)
                      ((string? output) (open-output-file output))
                      (else (error 'writer "unknown output type"))))
          )

    (lambda records
      (for-each
       (lambda (record) (display (format-record record) port))
       records))
    ))
            

)
