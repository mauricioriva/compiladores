#lang nanopass

#|
Compiladores 2022-1
Profesora: Dra. Lourdes del Carmen Gonzalez Huesca
Ayudante: Naomi Itzel Reyes Granados
Laboratorio: Nora Hilda Hernández Luna

Lexer y parser
|#

(require "Lexer.rkt"
         (prefix-in : parser-tools/lex-sre)
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc
         parser-tools/lex)

;; An abstraction of the grammar minHS.
(define-struct var-exp (i) #:transparent) ; For the variables.
(define-struct num-exp (n) #:transparent) ; For the numbers.
(define-struct bool-exp (b) #:transparent) ; For the booleans.

(define-struct fun-exp (lv e) #:transparent)       ; For fun with vars list and expr
(define-struct fun-f-exp (lv t e) #:transparent)   ; For funf with vars list and expr
(define-struct let-exp (vars body) #:transparent)  ; For the let statement
(define-struct typeof-exp (v e) #:transparent)     ; For the type of operator ":".
(define-struct typeof-f-exp (f t e) #:transparent) ; For the name and the parameters of the function (f), the returning type (t) and the body (e).
(define-struct int-exp () #:transparent)           ; For the Int type.
(define-struct boole-exp () #:transparent)         ; For the Bool type.
(define-struct func-exp (e1 e2) #:transparent)     ; For the Func type.
(define-struct and-exp (p q) #:transparent) ; For the AND expr.
(define-struct or-exp (p q) #:transparent)  ; For the OR expr.

; Note: There is a difference: bool is for values and boole is for type

(define-struct prim-exp (op e1 e2) #:transparent)   ; For the arithmetic operations.
(define-struct assign-exp (e1 e2) #:transparent)    ; For the assign expression.
(define-struct if-then-else-exp (g e1 e2) #:transparent) ; For the if then else conditionals.

(define-struct par-exp (exp) #:transparent)       ; For the parenthesis.
(define-struct brack-exp (exp) #:transparent)     ; For the square brackets
(define-struct key-exp (exp) #:transparent)       ; For the keys.
(define-struct app-exp (exp1 exp2) #:transparent) ; For app expression.
(define-struct app-t-exp (e1 e2) #:transparent)   ; For continuous expressions

;; Práctica 03.
(define-struct begin-exp (exp) #:transparent) ; For the begin expression.
(define-struct if-then-exp (g e1) #:transparent) ; For the if then expression.

(define minHS-parser
  (parser
   (start exp) ; start clause. The exp is the initial symbol where the parser begins the analysis. 
   (end EOF) ; end clause. The parser ends when it reads the given symbol. In our case, EOF.
   (error (λ(ok? name value) (printf "Couldn't parse: ~a\n" name))) ; error clause. Here can be some errors presented in the anlysis.
   (tokens a b) ; tokens clause. Here goes our tokens. In our case, we defined the tokens in the lexer script.
   (precs (nonassoc LP RP LC RC INT BOOLE : APP)
          (left - + =) ; precs clause. Here we can give some precedence of our language operators.
          (left * /))
   (suppress)
   (grammar ; grammar clause. Here goes the grammar of minHS.
    (exp ((NUM) (num-exp $1)) ;; ((Token) (constructor $1 $2 ... $n)) [1,2,3,...,n]
         ((BOOLEAN) (bool-exp $1)) 
         ((VAR) (var-exp $1))

         ; Binary expressions.
         ((exp APP exp) (app-exp $1 $3))
         ((exp + exp) (make-prim-exp + $1 $3)) ; ((e1 e2 e3 .... en) (constructor $1 $2 $3 ... $n))
         ((exp - exp) (make-prim-exp - $1 $3))
         ((exp * exp) (make-prim-exp * $1 $3))
         ((exp / exp) (make-prim-exp / $1 $3))
         ((exp = exp) (assign-exp $1 $3))
         ((exp AND exp) (make-prim-exp 'and $1 $3))
         ((exp OR exp) (make-prim-exp 'or $1 $3))
         ((LP exp RP) (make-par-exp $2))
         ((LC exp RC) (make-brack-exp $2))

         ; BEGIN expression.
         ((BEGIN LL exp RL) (begin-exp $3))
         
         ; IF-THEN-ELSE expression.
         ((IF LP exp RP THEN LL exp RL ELSE LL exp RL) (if-then-else-exp $3 $7 $11))
         ; IF-THEN expression.
         ((IF LP exp RP THEN LL exp RL) (if-then-exp $3 $7))

         ; FUN expression with no variables.
         ((FUN LP RP => exp) (fun-exp 'empty $5))
         ; FUN expression w variables.
         ((FUN LP listVarF : type RP => exp) (fun-exp (typeof-exp (make-brack-exp $3) $5) $8))

         ; FUNF expression with no variables.
         ((FUNF LP LP RP RP => exp) (fun-f-exp 'empty 'empty $7))
         ; FUNF expression w variables.
         ((FUNF LP exp LP listVarF RP : type RP => exp) (fun-f-exp (typeof-exp $3 (make-brack-exp $5)) $8 $11))

         ; LET expression with no variables.
         ((LET LP RP IN exp END) (let-exp 'empty $5))
         ; LET expression w variables.
         ((LET LP listVarL RP IN exp END) (let-exp (make-brack-exp $3) $6))
         )
    ; For the different types of minHS.
    (type
     ((INT) (int-exp))
     ((BOOLE) (boole-exp))
     ;;((FUNC type type) (func-exp $2 $3))
     ((LP type RP) (par-exp $2))
     )
    
    ; For variable list in Fun and FunF expressions.
    (listVarF
     ((LC exp : type RC) (typeof-exp $2 $4))
     ((LC exp : type RC listVarF) (app-t-exp (typeof-exp $2 $4) $6)))

    ; For variable list in Let expression.
    (listVarL
     ((LC exp : type = exp RC) (assign-exp (typeof-exp $2 $4) $6))
     ((LC exp : type = exp RC listVarL) (app-t-exp (assign-exp (typeof-exp $2 $4) $6) $8)))
    )))

;; Práctica 03.
; Función auxiliar para los procedimientos.
(define (procedure->string op)
  (cond
    [(equal? op +) "+ "]
    [(equal? op -) "- "]
    [(equal? op /) "/ "]
    [(equal? op *) "* "]
    [(equal? op 'and) "and "]
    [(equal? op 'or) "or "]
    )
  )
  
; Representación en cadena de una expresión
(define (expr->string e)
  (match e
    [(var-exp e) (symbol->string e)]
    [(num-exp e) (number->string e)]
    [(bool-exp e) (format "~a" e)]
    [(int-exp) "Int"]
    [(boole-exp) "Bool"]
    [(par-exp e) (string-append "(" (expr->string e) ")")]
    [(key-exp e) (string-append "{" (expr->string e) "}")]
    [(key-exp e) (string-append "[" (expr->string e) "]")]
    [(begin-exp e) (string-append "(begin " (expr->string e) ")")]
    [(brack-exp t) (expr->string t)]
    [(prim-exp op e1 e2) (string-append "(" (procedure->string op) (expr->string e1) " " (expr->string e2) ")")]
    [(prim-exp 'or p q) (string-append "or " (expr->string p) " " (expr->string q))]
    [(typeof-exp v e) (string-append "(" (expr->string v) " " (expr->string e) ")")]
    [(let-exp e b) (string-append "(let " (expr->string e) " " (expr->string b) ")")]
    [(app-exp e1 e2) (string-append "(" (expr->string e1) " " (expr->string e2) ")")]
    [(app-t-exp e1 e2) (string-append "(" (expr->string e1) " " (expr->string e2) ")")]
    [(assign-exp e1 e2) (string-append "(" (expr->string e1) " " (expr->string e2) ")")]
    [(if-then-exp g e1) (string-append "(if " (expr->string g) " " (expr->string e1) ")")]
    [(if-then-else-exp g t e) (string-append "(if " (expr->string g) " " (expr->string t) " " (expr->string e) ")")]
    [(fun-f-exp lv a b) (string-append "(funF " (expr->string lv) " " (expr->string a) " " (expr->string b) ")" )]
    [(fun-exp (typeof-exp args t) b) (string-append "(fun " (expr->string args) " " (expr->string t) " " (expr->string b) ")")]))



; A function that stores our lexer into a lambda function without arguments.
(define (lex-this lexer input) (lambda () (lexer input)))

;; Ejemplos función expr->string.
(let ((input (open-input-string "33 + 2")))
  (expr->string (minHS-parser (lex-this minHS-lexer input))))

(let ((input (open-input-string "3 - 3 / 6")))
  (expr->string (minHS-parser (lex-this minHS-lexer input))))

(let ((input (open-input-string "if(#t and #f)then{2}else{3}")))
  (expr->string (minHS-parser (lex-this minHS-lexer input))))

(let ((input (open-input-string "fun ([x:Int]:Int) => x")))
  (expr->string (minHS-parser (lex-this minHS-lexer input))))

(let ((input (open-input-string "fun ([x:Int][y:Int]:Int) => x*y")))
  (expr->string (minHS-parser (lex-this minHS-lexer input))))

(let ((input (open-input-string "funF (sumita ([x:Int][y:Int]):Int) => x+y")))
  (expr->string (minHS-parser (lex-this minHS-lexer input))))
