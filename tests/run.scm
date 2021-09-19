
(import scheme (chicken base) (chicken pathname) (chicken string)
        (chicken process) (chicken port) (chicken process-context)
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


(test-exit)
