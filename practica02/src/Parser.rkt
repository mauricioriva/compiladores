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

; Note: There is a difference: bool is for values and boole is for type

(define-struct prim-exp (op e1 e2) #:transparent)   ; For the arithmetic operations.
(define-struct assign-exp (e1 e2) #:transparent)    ; For the assign expression.
(define-struct if-then-exp (g e1 e2) #:transparent) ; For the if conditionals.

(define-struct par-exp (exp) #:transparent)       ; For the parenthesis.
(define-struct brack-exp (exp) #:transparent)     ; For the square brackets
(define-struct key-exp (exp) #:transparent)       ; For the keys.
(define-struct app-exp (exp1 exp2) #:transparent) ; For app expression.
(define-struct app-t-exp (e1 e2) #:transparent)   ; For continuous expressions

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
         ((LP exp RP) (make-par-exp $2))
         ((LC exp RC) (make-brack-exp $2))
         
         ; IF expression.
         ((IF LP exp RP THEN LL exp RL ELSE LL exp RL) (if-then-exp $3 $7 $11))

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
     ((FUNC type type) (func-exp $2 $3))
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

; A function that stores our lexer into a lambda function without arguments.
(define (lex-this lexer input) (lambda () (lexer input)))

; A lot of examples.
(display "Example 1: 3 - (3 / 6)\n")
(let ((input (open-input-string "3 - (3 / 6)")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(prim-exp #<procedure:-> (num-exp 3) (par-exp (prim-exp #<procedure:/> (num-exp 3) (num-exp 6))))
|#

(display "\nExample 2: if(#t)then{2}else{3}\n")
(let ((input (open-input-string "if(#t)then{2}else{3}")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(if-exp (bool-exp #t) (num-exp 2) (num-exp 3))
|#

(display "\nExample 3: fun ([x:Int]:Int) => x\n")
(let ((input (open-input-string "fun ([x:Int]:Int) => x")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'x) (int-exp))) (int-exp)) (var-exp 'x))
|#

(display "\nExample 4: fun ([f:Func Int Int]:Int) => f app 1\n")
(let ((input (open-input-string "fun ([f:Func Int Int]:Int) => f app 1")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'f) (func-exp (int-exp) (int-exp)))) (int-exp)) (app-exp (var-exp 'f) (num-exp 1)))
|#

(display "\nExample 5: fun ([f:Func (Func Int Bool) Int]:Bool) => #t\n")
(let ((input (open-input-string "fun ([f:Func (Func Int Bool) Int]:Bool) => #t")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-exp (typeof-exp (brack-exp (typeof-exp (var-exp 'f) (func-exp (par-exp (func-exp (int-exp) (boole-exp))) (int-exp)))) (boole-exp)) (bool-exp #t))
|#


(display "\nExample 6: funF (sumita ([x:Int][y:Int]):Int) => x+y\n")
(let ((input (open-input-string "funF (sumita ([x:Int][y:Int]):Int) => x+y")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(fun-f-exp
 (typeof-exp (var-exp 'sumita) (brack-exp (app-t-exp (typeof-exp (var-exp 'x) (int-exp)) (typeof-exp (var-exp 'y) (int-exp)))))
 (int-exp)
 (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y)))
|#

(display "\nExample 7: let ([x:Int = 1][y:Int = 2]) in x+y end\n")
(let ((input (open-input-string "let ([x:Int = 1][y:Int = 2]) in x+y end")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(let-exp
 (brack-exp (app-t-exp (assign-exp (typeof-exp (var-exp 'x) (int-exp)) (num-exp 1)) (assign-exp (typeof-exp (var-exp 'y) (int-exp)) (num-exp 2))))
 (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y)))
|#

(display "\nExample 8: ((funF (sumita ([x:Int][y:Int]):Int) => x+y) app 2) app 4\n")
(let ((input (open-input-string "((funF (sumita ([x:Int][y:Int]):Int) => x+y) app 2) app 4")))
  (minHS-parser (lex-this minHS-lexer input)))
#|
Desired response:
(app-exp
 (par-exp
  (app-exp
   (par-exp
    (fun-f-exp
     (typeof-exp (var-exp 'sumita) (brack-exp (app-t-exp (typeof-exp (var-exp 'x) (int-exp)) (typeof-exp (var-exp 'y) (int-exp)))))
     (int-exp)
     (prim-exp #<procedure:+> (var-exp 'x) (var-exp 'y))))
   (num-exp 2)))
 (num-exp 4))
|#