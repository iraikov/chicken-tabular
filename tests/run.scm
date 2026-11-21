
(import scheme (chicken base) (chicken pathname) (chicken string)
        (chicken process) (chicken port) (chicken process-context)
        (only utf8-srfi-13 string-trim-right)
        test tabular)

(define prefix (pathname-directory (program-name)))

(define (generator->list gen)
  (let recur ((ax '()))
    (let ((value (gen)))
      (if (eof-object? value)
          (reverse ax)
          (recur (cons value ax))
          ))
    ))

(define (stream->list proc strm)
  (let recur ((ax '()) (strm strm))
    (let ((value.strm (proc strm)))
      (if (eof-object? value.strm)
          (reverse ax)
          (recur (cons (car value.strm) ax) (cadr value.strm))
          ))
    ))

#; (test-group "csv read from invalid string"
 	    (test
 	     `(("Test \n1" "Test 2" "Test 3")
 	       ("Test 4" "Test 5" ))
             (let-values (((proc strm)
                           (call-with-input-string
                               "\"Test \n1\n"
                             (lambda (port) (reader port delimiter: #\|)))))
               (stream->list proc strm))
              ))

 (test-group "csv read from string without newline"
 	    (test
 	     `(("Test 1"))
             (let-values (((proc strm)
                           (call-with-input-string
                               "Test 1"
                             (lambda (port) (reader port delimiter: #\|)))))
               (stream->list proc strm))
              ))

 (test-group "csv read from string"
 	    (test
 	     `(("Test \n1" "Test 2" "Test 3")
 	       ("Test 4" "Test 5" ))
             (let-values (((proc strm)
                           (call-with-input-string
                               "\"Test \n1\"|Test 2|Test 3\nTest 4|Test 5\n"
                             (lambda (port) (reader port delimiter: #\|)))))
               (stream->list proc strm))
              ))
(let-values (((proc strm)
              (call-with-input-string
                  "\"Test \n1\",\"Test \"\"2\"\"\",Test 3\nTest 4,Test 5\n"
                (lambda (port) (reader port delimiter: #\,)))))
  (stream->list proc strm))

 (test-group "csv read from string with escaped quotes"
 	    (test
 	     `(("Test \n1" "Test \"2\"" "Test 3")
 	       ("Test 4" "Test 5" ))
             (let-values (((proc strm)
                           (call-with-input-string
                               "\"Test \n1\",\"Test \"\"2\"\"\",Test 3\nTest 4,Test 5\n"
                             (lambda (port) (reader port delimiter: #\,)))))
               (stream->list proc strm))
              ))

 (test-group "csv read from file"
 	    (test
              `("Bank Name" "City" "ST" "CERT" "Acquiring Institution" "Closing Date" "Updated Date")
               (let ((input (open-input-file (make-pathname prefix "data/banklist.csv"))))
                 (let-values (((proc strm) (reader input delimiter: #\,)))
                   (let ((lst (stream->list proc strm)))
                     (close-input-port input)
                     (car lst)))
                 ))
            )
                     

 (test-group "csv read from file with column names inferred from header"
 	    (test
              `(("Bank Name" "Banks of Wisconsin d/b/a Bank of Kenosha")
                ("City" "Kenosha")
                ("ST" "WI")
                ("CERT" "35386")
                ("Acquiring Institution" "North Shore Bank, FSB")
                ("Closing Date" "31-May-13")
                ("Updated Date" "31-May-13"))
               (let ((input (open-input-file (make-pathname prefix "data/banklist.csv"))))
                 (let-values (((proc strm) (reader input delimiter: #\, column-names: 'header return-names: #t)))
                   (let ((lst (stream->list proc strm)))
                     (close-input-port input)
                     (car lst)))
                 ))
            )
                     

 (test-group "csv read from file with specified column names"
 	    (test
             `(("C1" "Column 1")
               ("C2" "Column 2")
               ("C3" "Column 3"))
               (let ((input (open-input-file (make-pathname prefix "data/test1.csv"))))
                 (let-values (((proc strm) (reader input delimiter: #\,
                                                   column-names: '("C1" "C2" "C3" )
                                                   return-names: #t)))
                   (let ((lst (stream->list proc strm)))
                     (close-input-port input)
                     (car lst)))
                 ))
            )
                     

 (test-group "csv read generator from file with column names inferred from header"
 	    (test
              `(("Bank Name" "Banks of Wisconsin d/b/a Bank of Kenosha")
                ("City" "Kenosha")
                ("ST" "WI")
                ("CERT" "35386")
                ("Acquiring Institution" "North Shore Bank, FSB")
                ("Closing Date" "31-May-13")
                ("Updated Date" "31-May-13"))
               (let ((input (open-input-file (make-pathname prefix "data/banklist.csv"))))
                 (let ((reader (reader* input delimiter: #\, column-names: 'header return-names: #t)))
                   (let ((rec (reader)))
                     (close-input-port input)
                     rec))
                 ))
            )
                     

 (test-group "csv read generator from file with specified column names"
 	    (test
             `(("C1" "Column 1")
               ("C2" "Column 2")
               ("C3" "Column 3"))
               (let ((input (open-input-file (make-pathname prefix "data/test1.csv"))))
                 (let ((reader (reader* input delimiter: #\,
                                        column-names: '("C1" "C2" "C3" )
                                        return-names: #t)))
                   (let ((rec (reader)))
                     (close-input-port input)
                     rec))
                 ))
            )
                     

 (test-group "csv read from string generator"
 	    (test
 	     `(("Test \n1" "Test 2" "Test 3")
 	       ("Test 4" "Test 5" ))
              (let ((gen (call-with-input-string
                             "\"Test \n1\"|Test 2|Test 3\nTest 4|Test 5\n"
                           (lambda (port) (reader* port delimiter: #\|)))))
                (generator->list gen))
              ))

 (test-group "csv read from string generator with missing values"
 	    (test
 	     `(("Test \n1" "MISSING" "Test 3")
 	       ("Test 4" "Test 5" ))
              (let ((gen (call-with-input-string
                             "\"Test \n1\"||Test 3\nTest 4|Test 5\n"
                           (lambda (port) (reader* port delimiter: #\| na: "MISSING")))))
                (generator->list gen))
              ))

 (test-group "csv read from string generator with missing values"
 	    (test
 	     `(("Test \n1" "Test 2" "Test 3")
 	       ("Test 4" "MISSING" ))
              (let ((gen (call-with-input-string
                             "\"Test \n1\"|Test 2|Test 3\nTest 4|\n"
                           (lambda (port) (reader* port delimiter: #\| na: "MISSING")))))
                (generator->list gen))
              ))


 (test-group "csv read from file generator"
             (test
              `("Bank Name" "City" "ST" "CERT" "Acquiring Institution" "Closing Date" "Updated Date")
              (car
               (let* ((input (open-input-file (make-pathname prefix "data/banklist.csv")))
                      (gen (reader* input delimiter: #\,))
                      (lst (generator->list gen)))
                 (close-input-port input)
                 lst))
              ))

 (test-group "fixed-width read from string generator"
 	    (test
 	     `(("Test 1" "Test 2" "Test 3")
 	       ("Test 4" "Test 5" "Test 6"))
              (let ((gen (call-with-input-string
                             "Test 1Test 2Test 3\nTest 4Test 5Test 6\n"
                           (lambda (port) (reader* port delimiter: #f column-widths: '(6 6 6))))))
                (generator->list gen))
              ))

(test-group "writer"
 	    (test
 	     "Test 1,Test 2,Test 3\nTest 4,Test 5\n"
             (call-with-output-string
              (lambda (port)
                (let ((out (writer port)))
                  (out (list "Test 1" "Test 2" "Test 3")
                       (list "Test 4" "Test 5" )))
                ))
              ))

(test-group "writer"
 	    (test
 	     "Test 1,Test 2,Test 3\nTest 4,Test 5\n"
             (call-with-output-string
              (lambda (port)
                (let ((out (writer port)))
                  (out (list "Test 1" "Test 2" "Test 3")
                       (list "Test 4" "Test 5" )))
                ))
              ))

(test-group "fixed-width writer"
 	    (test
 	     "Test 1Test 2Test 3\nTest 4Test 5\n"
             (call-with-output-string
              (lambda (port)
                (let ((out (writer port delimiter: #f column-widths: `(6 6 6))))
                  (out (list "Test 1" "Test 2" "Test 3")
                       (list "Test 4" "Test 5" )))
                ))
              ))

(define (write-fixed-width widths records #!optional (align #f))
  (call-with-output-string
   (lambda (port)
     (let ((out (writer port delimiter: #f column-widths: widths align: align)))
       (apply out records)))))

(test-group "fixed-width writer field sizes"
            (test "short field is padded on the right"
                  "ab   cdefg\n"
                  (write-fixed-width '(5 5) (list (list "ab" "cdefgh"))))
            (test "long field is cut to the column width"
                  "abc\n"
                  (write-fixed-width '(3) (list (list "abcdefgh"))))
            (test "field of exactly the column width is unchanged"
                  "abcde\n"
                  (write-fixed-width '(5) (list (list "abcde"))))
            (test "empty field becomes all spaces"
                  "   x\n"
                  (write-fixed-width '(3 1) (list (list "" "x"))))
            (test "record mixing short and long fields"
                  "a  longxyzq \n"
                  (write-fixed-width '(3 4 3 2) (list (list "a" "longfield" "xyz" "q"))))
            (test "widths count characters, not bytes"
                  "\x00e4;b  |\n"
                  (write-fixed-width '(4 1) (list (list "\x00e4;b" "|"))))
            )

(test-group "fixed-width writer alignment"
            (test "right-aligned columns are padded on the left"
                  "ab   cdefg  7\n"
                  (write-fixed-width '(5 5 3)
                                     (list (list "ab" "cdefgh" "7"))
                                     '(left right right)))
            (test-error "align without column-widths is an error"
                        (writer (current-output-port) align: '(left)))
            )

(define (read-fixed-width widths text)
  (generator->list
   (call-with-input-string
    text
    (lambda (port) (reader* port delimiter: #f column-widths: widths)))))

;; The fixed-width reader returns each field with the exact number of
;; characters in its column, so padding spaces written by the writer
;; come back as part of the field and must be trimmed by the caller.
(test-group "fixed-width writer output read back"
            (let* ((widths '(6 4 5))
                   (records '(("alpha" "1" "x")
                              ("beta" "22" "yy")
                              ("gamma" "333" "zzzzz")))
                   (text (write-fixed-width widths records)))
              (test "reader keeps the padding spaces"
                    `(("alpha " "1   " "x    ")
                      ("beta  " "22  " "yy   ")
                      ("gamma " "333 " "zzzzz"))
                    (read-fixed-width widths text))
              (test "trimmed fields equal the original fields"
                    records
                    (map (lambda (rec) (map string-trim-right rec))
                         (read-fixed-width widths text)))
              ))


(test-exit)
