#lang nanopass

(require nanopass/base)

;; Definimos el lenguaje fuente
(define-language LF
  (terminals
   (variable (x)) ;;Done
   (primitive (pr))
   (constant (c))
   (string (s))
   (char (c1))
   (type (t)) ;; Done
   (lista (l))) ;; Done
  ;; e ::= x | pr | c | s | t | l | begin (e,...,e) | if(e,e) | if(e,e,e)
  (Expr (e body)
    x
    pr
    c
    s
    c1
    t
    l
    (begin e* ... e)
    (if e0 e1)
    (if e0 e1 e2)
    (fun ((x* t*) ...) t body* ... body) ;; fun ((x Int) (y Float)) Float (asign z (* (+ x y) 3)) (asign z 10))
    (let ((x* t* e*) ...) body* ... body)
    (funF x ((x* t*) ...) t body* ... body)
    (e0 e1 ...) ;; Aplicacion (pr e0 e1) --- (+ 1 #true)
    (pr e* ... e)))

;; Definimos los predicados (faltan)
(define (variable? x)
  (symbol? x))

(define (type? t)
 (or (equal? t 'Int) (equal? t 'Bool) (eq? t 'String) (eq? t 'Char))) ;; T ::= Int | Bool | String | Char

(define (lista? x)
  (list? x))

(define (primitive? x)
  (or (procedure? x) (memq x '(and or + - * /))))

(define (constant? x)
  (or (number? x) (boolean? x) (char? x)))

;; Proceso del compilador encargado de hacer explicitas las expresiones
;; begin en el cuerpo de fun, let y funF
(define-pass make-explicit : LF (ir) -> LF ()
  (Expr : Expr (ir) -> Expr ()
    [,c `',c]
    [(fun ([,x* ,t*] ...) ,t ,[body*] ... ,[body])
     `(fun ([,x* ,t*] ...) t (begin ,body* ... ,body))]
    [(let ([,x* ,t* ,[e*]] ...) ,[body*] ... ,[body])
     `(let ([,x* ,t* ,e*] ...) (begin ,body* ... ,body))]
    [(funF ,x ([,x* ,t*] ...) ,t ,[body*] ... ,[body])
     `(funF x ([,x* ,t*] ...) t (begin ,body* ... ,body))]))


;; Definimos el parser del lenguaje fuente (LF)
(define-parser parse-LF LF)

;; Definimos el lenguaje sin if's de una rama
(define-language LNI
  (extends LF)
  (Expr (e body)
        (- (if e0 e1))))

;; El parser de remove-if
(define-parser parse-LNI LNI)

;; El pass para eliminar if's de una sola rama
(define-pass remove-one-armed-if : LF (ir) -> LNI()
  (Expr : Expr (ir) -> Expr()
        [(if ,[e0] ,[e1]) `(if ,e0 ,e1 (void))])) ;; Solo caza este patrón

;; Lenguaje que no tiene cadenas de texto
(define-language LNI2
  (extends LNI)
  (terminals
   (- (string (s))))
  (Expr (e body)
        (- s)))

;; El parser para el lenguaje sin cadenas de texto
(define-parser parse-LNI2 LNI2)

;; El pass para eliminar cadenas de texto
(define-pass remove-string : LNI(ir) -> LNI2 ()
  (definitions
    (define (aLista x)
      (string->list x))) ;; Podemos definir funciones dentro del pass
  (Expr : Expr (ir) -> Expr()
        [,s `(lista ,(aLista s))]))

;; Ejemplos.

;; Algunos ejemplos de expresiones de LF
;; Ejemplos aritmético booleanos
(parse-LF '(+ 1 2))
(parse-LF '(let (x Int (+ 4 1)) (* x 8)))
(parse-LF '(let (x Int 0) (if (> x 0) 1 #t)))
;; Ejemplos funciones
(parse-LF '(fun ((x Int) (y Int)) Int (let (w Int (+ x 1)) (* x 2 y))))
(parse-LF '(fun ((x Int) (y Int)) Int (+ x 1) (+ y 2))) ;; Caso de body*

(remove-string (parse-LNI '("Hello")))
(remove-one-armed-if (parse-LF '(if (> x 0) 1)))