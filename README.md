# tabular

Parsing and formatting of tabular text data such as comma- and delimiter-separated values (CSV and DSV).

## Documentation

The goal of the tabular library is to provide means of reading and
writing tabular text data, such as comma- and delimiter-separated
values (CSV and DSV), as well as fixed-width columns.


## Procedures

### Readers
   
`reader:: INPUT [DELIMITER: #\,] [EOL: "\n"] [COMMENT-CHAR: #f]
[NA: #f] [COLUMN-NAMES: #f] [COLUMN-WIDTHS: #f] [RETURN-NAMES: #f] -> READER STREAM`

Constructs a reader procedure for the given input format specification and returns two values: the reader procedure and the input stream. The reader procedure is of the form `LAMBDA STRM -> RECORD STRM`. It reads one record from the given input stream and by default returns the record as a list of values and the remainder of the input stream. If optional argument `RETURN-NAMES` is true, the record will be returned as an alist where the keys are column names (if `COLUMN-NAMES` is provided) or indices, and the values are the column values. 

- Argument `INPUT` is a string or a port.
- Argument `DELIMITER` specifies the delimiter character (default is `#\,`). If its value is #f, then `COLUMNN-WIDTHS` must be specified.
- Argument `EOL` specifies the end-of-line string (default is `"\n"`).
- Argument `COMMENT-CHAR` specifies a comment character: the reader will skip all lines that start with this character (default: none).
- Argument `NA` specifies a default value to be supplied when a given field is empty (default: none).
- Argument `COLUMN-NAMES` specifies column names which will be used if `RETURN-NAMES` is true (default: none).
- Argument `COLUMN-WIDTHS` specifies column widths  which will be used if `DELIMITER` is not specified (default: none).
- Argument `RETURN-NAMES` specifies that each returned record will have the form of an alist with column name and value pairs. If `COLUMN-NAMES` is the symbol `'headers` then the first line of the input will be used to infer the column names. If `COLUMN-NAMES` is not provided, the column index will be used as key instead.

`reader*:: INPUT [DELIMITER: #\,] [EOL: "\n"] [COMMENT-CHAR: #f]
[NA: #f] [COLUMN-NAMES: #f] [COLUMN-WIDTHS: #f] [RETURN-NAMES: #f] -> READER`

Constructs a reader generator procedure for the given input format specification and returns the reader procedure. The reader procedure is of the form `LAMBDA () -> RECORD`. It reads one record from the given input stream and by default returns the record as a list of values. If optional argument `RETURN-NAMES` is true, the record will be returned as an alist where the keys are column names (if `COLUMN-NAMES` is provided) or indices, and the values are the column values. 

- Argument `INPUT` is a string or a port.
- Argument `DELIMITER` specifies the delimiter character (default is `#\,`). If its value is #f, then `COLUMNN-WIDTHS` must be specified.
- Argument `EOL` specifies the end-of-line string (default is `"\n"`).
- Argument `COMMENT-CHAR` specifies a comment character: the reader will skip all lines that start with this character (default: none).
- Argument `NA` specifies a default value to be supplied when a given field is empty (default: none).
- Argument `COLUMN-NAMES` specifies column names which will be used if `RETURN-NAMES` is true (default: none).
- Argument `COLUMN-WIDTHS` specifies column widths  which will be used if `DELIMITER` is not specified (default: none).
- Argument `RETURN-NAMES` specifies that each returned record will have the form of an alist with column name and value pairs. If `COLUMN-NAMES` is the symbol `'headers` then the first line of the input will be used to infer the column names. If `COLUMN-NAMES` is not provided, the column index will be used as key instead.


### Writers

` writer:: OUTPUT [DELIMITER: #\,] [COLUMN-WIDTHS: #f] [ENDLINE: "\n"] -> WRITER `

Constructs a writer procedure that writes out records according to the
given output specification. The writer procedure is of the form
`LAMBDA RECORDS -> UNIT`. Each record is represented as a list.

- Argument `OUTPUT` is a string file name or a port.
- Argument `DELIMITER` specifies the delimiter character (default is `#\,`). If its value is #f, then `COLUMNN-WIDTHS` must be specified.
- Argument `ENDLINE` specifies the end-of-line string (default is `"\n"`).
- Argument `COLUMN-WIDTHS` specifies column widths  which will be used if `DELIMITER` is not specified (default: none).


## Examples


```scheme

(import tabular)

(define (stream->list proc strm)
  (let recur ((ax '()) (strm strm))
    (let ((value.strm (proc strm)))
      (if (eof-object? value.strm)
          (reverse ax)
          (recur (cons (car value.strm) ax) (cadr value.strm))
          ))
    ))

;; Uses a reader generator to print all records in the given input string
(let-values (((proc strm)
             (call-with-input-string
                "\"Test \n1\"|Test 2|Test 3\nTest 4|Test 5\n"
               (lambda (port) (reader port delimiter: #\|)))))
               (print (stream->list proc strm)))


;; Prints all the records from the given file, where column names are inferred from the first line
;; and each field in a record is prefixed by its column name
(let ((input (open-input-file "file.csv")))
  (let-values (((proc strm) (reader input delimiter: #\, column-names: 'header return-names: #t)))
    (let ((lst (stream->list proc strm)))
      (close-input-port input)
      (pretty-print lst)
      ))
  )

;; Prints the first record of the given file
(let ((input (open-input-file "file.csv"))
      (gen (reader* input delimiter: #\,)))
   (print (gen)) 
   )



;; Writes the given records to a string port
(call-with-output-string
 (lambda (port)
  (let ((out (writer port)))
    (out (list "Test 1" "Test 2" "Test 3")
         (list "Test 4" "Test 5" )))
))


```

## Version history

- 1.3 : Fix for return-names [thanks to Vasilij Schneidermann for reporting]
- 1.2 : Fix for escaped double quotes [thanks to Matt Welland for reporting]
- 1.0 : Initial release

## License

>
> Copyright 2019-2021 Ivan Raikov
> 
>  This program is free software: you can redistribute it and/or modify
>  it under the terms of the GNU General Public License as published by
>  the Free Software Foundation, either version 3 of the License, or (at
>  your option) any later version.
>  
>  This program is distributed in the hope that it will be useful, but
>  WITHOUT ANY WARRANTY; without even the implied warranty of
>  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
>  General Public License for more details.
> 
>  A full copy of the GPL license can be found at
>  <http://www.gnu.org/licenses/>.

