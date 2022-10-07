#lang racket

;import libraries for lexer
(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))


;takes the text from the user-entered file and returns a list of tokens based on grammar 
(define scan 
  (lexer
   [whitespace
    (scan input-port)]
   ["read" (cons '(READ)
                 (scan input-port))]
   ["write" (cons '(WRITE)
                  (scan input-port))]
   [(:+ (:or (char-range #\a #\z) (char-range #\A #\Z)))
    (cons `(ID ,(string->symbol lexeme))
          (scan input-port))]
   [(:: (:+ (char-range #\0 #\9)))
    (cons `(INT ,(string->number lexeme))
          (scan input-port))]
   [(:or #\+ #\-)
    (cons `(ADD_OP ,(string->symbol lexeme))
          (scan input-port))]
   [(:or #\* #\/)
    (cons `(MULT_OP ,(string->symbol lexeme))
          (scan input-port))]
   [":=" (cons '(ASSIGN)
              (scan input-port))]
   [#\( (cons '(LPAREN)
              (scan input-port))]
   [#\) (cons '(RPAREN)
              (scan input-port))]
   [#\$ (cons '(END)
              (scan input-port))]
   [(eof)
    '()]))


;takes the token at the beginning of the token list and 'consumes' it
;returns the rest of the token list without the original first token
(define (match token expected)
  (display "match ")
  (display (car token))
  (display "\n")

  ;checks current token is what is desired for grammar 
  (cond 
    [(equal? (car token) expected)
     (rest token)]
    [(equal? (car (car token)) expected)
     (rest token)]
    [(equal? (cdr (car token)) expected)
     (rest token)]
    [else (parse_error token)]))


;Begins the recursive parsing
;takes the token at the beginning of the token list and evaluates it against grammar 
(define (program token)
  (display "program ")
  (display (car token))
  (display "\n")

  ;checks current token is what is desired for grammar
  ;executes next function based on grammar 
  (cond
    [(equal? (car (car token)) 'ID)  
     (match (stmt_list token) '(END))
     (display "\n")
     (display "Accept")]
    [(equal? (car (car token)) 'READ)
     (match (stmt_list token) '(END))
     (display "\n")
     (display "Accept")]
    [(equal? (car (car token)) 'WRITE)
     (match (stmt_list token) '(END))
     (display "\n")
     (display "Accept")]
    [else (parse_error token)]))


;continues recursive parsing based on grammar 
;takes the token at the beginning of the token list and evaluates it against grammar 
(define (stmt_list token)
  (display "stmt_list ")
  (display (car token))
  (display "\n")

  ;checks current token is what is desired for grammar
  ;executes next function based on grammar 
  (cond
    [(equal? (car (car token)) 'ID) 
     (stmt_list (stmt token))]
    [(equal? (car token) '(READ)) 
     (stmt_list (stmt token))]
    [(equal? (car token) '(WRITE))
     (stmt_list (stmt token))]
    [(equal? (car token) '(END)) 
     (rest token)]
    [else (parse_error token)]))


;continues recursive parsing based on grammar 
;takes the token at the beginning of the token list and evaluates it against grammar
(define (stmt token)
  (display "stmt ")
  (display (car token))
  (display "\n")

  ;checks current token is what is desired for grammar
  ;executes next function based on grammar 
  (cond
    [(equal? (car (car token)) 'ID)  
     (expr (match (match token 'ID) '(ASSIGN)))]
    [(equal? (car token) '(READ))  
     (match (match token '(READ)) 'ID)]
    [(equal? (car token) '(WRITE))
     (expr (match token '(WRITE)))]
    [else (parse_error token)]))


;continues recursive parsing based on grammar 
;takes the token at the beginning of the token list and evaluates it against grammar 
(define (expr token)
  (display "expr ")
  (display (car token))
  (display "\n")

  ;chekcs current token is what is desired for grammar
  ;executes next function based on grammar 
  (cond
    [(equal? (car(car token)) 'ID)
     (term_tail (term token))]
    [(equal? (car (car token)) 'INT)
     (term_tail (term token))]
    [(equal? (car token) '(LPAREN))
     (term_tail(term token))]
    [else (parse_error token)]))


;continues recursive parsing based on grammar 
;takes the token at the beginning of the token list and evaluates it against grammar 
(define (term_tail token)
  (display "term_tail ")
  (display (car token))
  (display "\n")

  ;checks current token is what is desired for grammar
  ;executes next function based on grammar 
  (cond
    [(equal? (car (car token)) 'ADD_OP)
     (term_tail (term (add_op token)))]
    [(equal? (car token) '(RPAREN))
     token]
    [(equal? (car (car token)) 'ID)
     token]
    [(equal? (car token) '(READ))
     token]
    [(equal? (car token) '(WRITE))
     token]
    [(equal? (car token) '(END))
     token]
    [else (parse_error token)]))


;continues recursive parsing based on grammar
;takes the token at the beginning of the token list and evaluates it against grammar 
(define (term token)
  (display "term ")
  (display (car token))
  (display "\n")

  ;checks current token is what is desired for grammar
  ;executes next function based on grammar 
  (cond
    [(equal? (car (car token)) 'ID)
     (factor_tail (factor token))]
    [(equal? (car (car token)) 'INT)
     (factor_tail (factor token))]
    [(equal? (car token) '(LPAREN))
     (factor_tail (factor token))]
    [else (parse_error token)]))


;continues recursive parsing based on grammar 
;takees the token at the beginning of the token list and evaluates it against grammar
(define (factor_tail token)
  (display "factor_tail ")
  (display (car token))
  (display "\n")
  
  ;checks current token is what is desired for grammar
  ;executes next function based on grammar 
  (cond
    [(equal? (car (car token)) 'MULT_OP)
     (factor_tail (factor (mult_op token)))]
    [(equal? (car (car token)) 'ADD_OP)
     token]
    [(equal? (car token) '(RPAREN))
     token]
    [(equal? (car (car token)) 'ID)
     token]
    [(equal? (car token) '(READ))
     token]
    [(equal? (car token) '(WRITE))
     token]
    [(equal? (car token) '(END))
     token]
    [else (parse_error token)]))


;continues recursive parsing based on grammar 
;takes the token at the beginning of the token list and evaluates it against grammar 
(define (factor token)
  (display "factor ")
  (display (car token))
  (display "\n")

  ;checks current token is what is desired for grammar
  ;executes next function based on grammar 
  (cond
   [(equal? (car (car token)) 'ID)
    (match token 'ID)]
   [(equal? (car(car token)) 'INT)
    (match token 'INT)]
   [(equal? (car token) '(LPAREN))
    (match (expr (match token '(LPAREN))) '(RPAREN))]
   [else (parse_error token)]))


;contineus recursive parsing based on grammar 
;takes the token at the beginning of the token list and evaluates it against grammar 
(define (add_op token)
  (display "add_op ")
  (display (car token))
  (display "/n")

  ;checks current token is what is desired for grammar
  ;executes next function based on grammar 
  (cond
    [(equal? (cdr (car token)) '(+))
     (match token '(+))]
    [(equal? (cdr (car token)) '(-))
     (match token '(-))]
    [else (parse_error token)]))


;continues recursive parsing based on grammar
;takes the token at the begining of the token list and evaluates it against grammar 
(define (mult_op token)
  (display "mult_op ")
  (display (car token))
  (display "\n")

  ;checks current token is what is desired for grammar
  ;executes next function based on grammar 
  (cond
    [(equal? (cdr (car token)) '(*))
     (match token '(*))]
    [(equal? (cdr token) '(/))
     (match token '(/))]
    [else (parse_error token)]))


;displays error warning
;ends program 
(define (parse_error token)
  (display "\n")
  (display "Syntax Error")
  (exit #t))
 

;begins the parsing process 
(define (parser file_name)

  (define input (file->string file_name)) ;scans the information from the file, returns string
  
  (define hold (scan (open-input-string input))) ;scans the information from the string with file info, returns list of symbols
  
  (program hold)) ;calls the beginning function for parsing 

