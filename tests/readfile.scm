
(import scheme (chicken port) (chicken pretty-print) test sdsv)

(define (generator->list gen)
  (let recur ((ax '()))
    (let ((value (gen)))
      (if (eof-object? value)
          (reverse ax)
          (recur (cons value ax))
          ))
    ))

(define lst
  (let* ((input (open-input-file "data/banklist.csv"))
         (gen (reader* input delimiter: #\,))
         (lst (generator->list gen)))
    (close-input-port input)
    (pretty-print lst)))

