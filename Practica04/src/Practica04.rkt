#lang nanopass

;; Cargamos la gramatica libre de contexto
(require nanopass/base)

(define-language LF
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (type (t)))
  (Expr (e body)
        x
        (quot c)
        (begin e* ... e)
        (primapp pr e* ...)
        (if e0 e1 e2)
        (lambda ([x* t*] ...) body)
        (let ([x* t* e*] ...) body)
        (letrec ([x* t* e*] ...) body)
        (list e* e)
        (e0 e1 ...)))

(define (type? t)
 (or (eq? t 'Int) (eq? t 'Bool) (eq? t 'String) (eq? t 'Char) (eq? t 'Lambda)))

(define (constant? x)
  (or (number? x) (boolean? x)))

(define (variable? x)
  (and (symbol? x) (not (memq x '(and or + - * / > < car cdr length)))))

(define (primitive? x)
  (or (procedure? x) (memq x '(and or + - * / > < car cdr length))))

(define-parser parse-LF LF)


;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------


;; 1. Definir el proceso curry-let encargado de currificar las expresiones let y letrec.

;;Para este ejercicio necesitamos crear un lenguaje L7 para poder trabajar con las expresiones let y letrec
(define-language L7
  (extends LF)
  (Expr (e body)
        (- (let ([x* t* e*] ...) body)
           (letrec ([x* t* e*] ...) body))
        (+ (let ([x t e]) body)
           (letrec ([x t e]) body))))

(define-parser parse-L7 L7)

;;Usaremos recursion usando rest_let para quitar la primer asignación de de let o letrec
(define-pass curry-let : LF (ir) -> L7 ()
  (Expr : Expr (ir) -> Expr ()
        ;;Si el match casa la expresion let entonces
        [(let ((,x* ,t* ,[e*]) ...) ,[body])
         (let rest_let ([bindingx* x*] [bindingt* t*] [bindinge* e*])
           (if (= (length bindingx*) 1)
               ;;Preguntamos si es igual a 1, de ser asi regresamos la expresion
             `(let ((,(first bindingx*) ,(first bindingt*) ,(first bindinge*))) ,body)
               ;;De lo contrario hacemos recursion con el resto del cuerpo de let
             `(let ((,(first bindingx*) ,(first bindingt*) ,(first bindinge*)))
                  ,(rest_let (rest bindingx*) (rest bindingt*) (rest bindinge*)))))]
        [(letrec ((,x* ,t* ,[e*]) ...) ,[body])
         (let rest_let ([bindingx* x*] [bindingt* t*] [bindinge* e*])
           (if (= (length bindingx*) 1)
             `(letrec ((,(first bindingx*) ,(first bindingt*) ,(first bindinge*))) ,body)
             `(letrec ((,(first bindingx*) ,(first bindingt*) ,(first bindinge*)))
                  ,(rest_let (rest bindingx*) (rest bindingt*) (rest bindinge*)))))]))

;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

;; 2. Definir el proceso identify-assigments en el que se detecten los let utilizados para definir funciones y se remplazan por letrec.

(define-pass identify-assigments : L7 (ir) -> L7 ()
  (Expr : Expr (ir) -> Expr ()
        [(let ((,x ,t ,e)) ,[body])
         ;;Dado a L7, identify solo se necesita verificar las expresiones Lambda
           (if (symbol=? t 'Lambda)
               `(letrec ([,x ,t ,e]) ,body)
               `(let ([,x ,t ,e]) ,body))]))


;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------


;; 3. Definir el proceso un-anonymous encargado de asignarle un identificador a las funciones anónimas (lambda).

;;Definicion del lenguaje 8
(define-language L8
  (extends L7)
  (Expr (e body)
        (+ (letfun ([x t e]) body))))

(define-parser parse-L8 L8)

;; Funcion auxiliar para convertir un CHAR a INT
(define (char->int x)
  (string->number (string x)))

;; Función auxiliar que genera nombres
(define (get-nombre x)
  (let ([lx (string->list (symbol->string x))])
    (if (char-numeric? (last lx))
        (string->symbol (string-join (map (λ (x) (if (number? x)
                                                     (number->string x)
                                                     (string x))) (append (take lx (- (length lx) 1)) (list (+ (char->int (last lx)) 1)))) ""))
        (string->symbol (string-join (map (λ (x) (if (number? x)
                                                     (number->string x)
                                                     (string x))) (append lx '(0))) "")))))

;; Funcion auxiliar que modifica expresiones letfun para evitar conflictos
(define-pass rename-foo : L8 (ir) -> L8 ()
  (Expr : Expr (ir) -> Expr ()
        [(letfun ([,x ,t ,e]) ,body)
         (let ([nuevoFoo (get-nombre x)] [nuevaE (rename-foo e)])
           `(letfun ([,nuevoFoo ,t ,nuevaE]) ,nuevoFoo))]))

;;Funcion un-anonymous simplificada
(define-pass un-anonymous : L7 (ir) -> L8 ()
  (Expr : Expr (ir) -> Expr ()
        [(lambda ([,x* ,t*] ...) ,[body])
         (rename-foo `(letfun ([,'foo ,'Lambda (lambda ([,x* ,t*] ...) ,body)])
                  ,'foo))]))


;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#| 4. Define el proceso verify-arity. Este proceso funciona como verificador de la sintaxis de las expresiones y
consiste en verificar que el número de parámetros corresponde con la aridad de las primitivas.
|#

;;Funcion auxiliar para verificar aridad en simbolos
(define (verf-arity op args)
  (cond
    [(symbol=? op '+) (> (length args) 1)]
    [(symbol=? op '-) (> (length args) 1)]
    [(symbol=? op '*) (> (length args) 1)]
    [(symbol=? op '/) (> (length args) 1)]
    [(symbol=? op 'length) (= (length args) 1)]
    [(symbol=? op 'car) (= (length args) 1)]
    [(symbol=? op 'cdr) (= (length args) 1)]))

;;Funcion verificador de aridad
(define-pass verify-arity : L8 (ir) -> L8 ()
  (Expr : Expr (ir) -> Expr ()
        [(primapp ,pr ,[e*] ...)
         (if (verf-arity pr e*)
             `(primapp ,pr ,e* ...)
             (error ("Fallo en la Aridad")))]))



;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#| 5. Define el proceso verify-vars. Este proceso funciona como verificador de la sintaxis de las expresiones y
consiste en verificar que la expresión no tenga variables libres,
|#

(define-pass verify-vars : L8 (ir) -> L8 ()
  (Expr : Expr (ir [env '()]) -> Expr ()
        [,x (if (memq x env)
                x
                (error (string-append "Variable libre " (symbol->string x))))]
        [(let ([,x ,t ,[e]]) ,[Expr : body (cons x env) -> body])
         `(let ([,x ,t ,e]) ,body)]
        [(letrec ([,x ,t ,[Expr : e (cons x env) -> e]]) ,[Expr : body (cons x env) -> body])
         `(letrec ([,x ,t ,e]) ,body)]
        [(lambda ([,x* ,t*] ...) ,[Expr : body (append x* env) -> body])
         `(lambda ([,x* ,t*] ...) ,body)]
        [(letfun ([,x ,t ,[e]]) ,[Expr : body (cons x env) -> body])
         `(letfun ([,x ,t ,e]) ,body)]))


;;Pruebas
;; Ejercicio 01

(printf "\n ------------------Ejercicio 1-------------------")
(printf "\n Expresion usada (let ([x Int (quot 4)] [y Int (quot 6)]) (primapp + x y))")
(printf "\n Expresion esperada '(let ((x Int (quot 4))) (let ((y Int (quot 6))) (primapp + x y)))")
(printf "\n Resultado:")
(curry-let (parse-LF '(let ([x Int (quot 4)] [y Int (quot 6)]) (primapp + x y))))

;; Ejercicio 02

(printf "\n\n ------------------Ejercicio 2-------------------")
(printf "\n Expresion usada '(let ([foo Lambda (lambda ([x Int]) x)]) (foo (quot 5)))")
(printf "\n Expresion esperada '(letrec ((foo Lambda (lambda ((x Int)) x))) (foo (quot 5)))")
(printf "\n Resultado:")
(identify-assigments (parse-L7 '(let ([foo Lambda (lambda ([x Int]) x)]) (foo (quot 5)))))

;; Ejercicio 03

(printf "\n\n ------------------Ejercicio 3-------------------")
(printf "\n Expresion usada '(lambda ([x Bool]) (if x (quot 1) (quot 2))))")
(printf "\n Expresion esperada '(letfun ((foo0 Lambda (lambda ((x Bool)) (if x (quot 1) (quot 2))))) foo0) ")
(printf "\n Resultado:")
(un-anonymous (parse-L7 '(lambda ([x Bool]) (if x (quot 1) (quot 2)))))

;; Ejercicio 04

(printf "\n\n ------------------Ejercicio 4-------------------")
(printf "\n Expresion usada '(primapp + (quot 2) (quot 3)))")
(printf "\n Expresion esperada '(primapp + (quot 2) (quot 3)) ")
(printf "\n Resultado: ")
(verify-arity (parse-L8 '(primapp + (quot 2) (quot 3))))

;; Ejercicio 05

(printf "\n\n ------------------Ejercicio 5-------------------")
(printf "\n Expresion usada '(let ([x Int (quot 4)]) (primapp + (quot 1) x))")
(printf "\n Expresion esperada '(let ((x Int (quot 4))) (primapp + (quot 1) x))")
(printf "\n Resultado: ")
(verify-vars (parse-L8 '(let ([x Int (quot 4)]) (primapp + (quot 1) x))))

#|
(printf "\n Expresion usada '(primapp + (quot 4) x)")
(printf "\n Expresion esperada error: Variable libre x")
(printf "\n Resultado: ")
(verify-vars (parse-L8 '(primapp + (quot 4) x)))
|#