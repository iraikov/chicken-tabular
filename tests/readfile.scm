(import scheme (chicken base) (chicken pathname) (chicken string)
        (chicken pretty-print) tabular)


(define (stream->list proc strm)
  (let recur ((ax '()) (strm strm))
    (let ((value.strm (proc strm)))
      (if (eof-object? value.strm)
          (reverse ax)
          (recur (cons (car value.strm) ax) (cadr value.strm))
          ))
    ))


(define prefix "tests/data")

#;(let ((input (open-input-file (make-pathname prefix "large.csv"))))
  (let-values (((proc strm) (reader input delimiter: #\, column-names: 'header  return-names: #t eol: "\r\n")))
    (let ((lst (stream->list proc strm)))
      (close-input-port input)
      (pretty-print lst)
      ))
  )
  
(let ((input (open-input-file (make-pathname prefix "large.csv"))))
  (let ((reader (reader* input delimiter: #\, column-names: 'header return-names: #t eol: "\r\n")))
    (let loop ((rec (reader)))
      (if (not (eof-object? rec))
          (begin
            (pretty-print rec)
            (loop (reader)))))
      (close-input-port input)
    ))

