#lang nanopass

(define-language L8
  (terminals
   (variable (x))
   (primitive (pr))
   (constant (c))
   (type (t)))
  (Expr (e body)
        x
        pr
        c
        t
        (quot c)
        (primapp pr e* ...)
        (begin e* ... e)
        (if e0 e1 e2)
        (lambda ([x* t*] ...) body* ... body)
        (let ([x t e]) body* ... body)
        (letrec ([x t e]) body* ... body)
        (letfun ([x t e]) body* ... body)
        (list e* ...)
        (e0 e1 ...)))

;; Predicado para variables
(define (variable? x)
  (and (symbol? x) (not (memq x '(and or + - * / > < car cdr length)))))

;; Predicado para tipos
(define (type? t)
 (or (equal? t 'Int)
     (equal? t 'Bool)
     (equal? t 'String)
     (equal? t 'Char)
     (equal? t 'Lambda)
     (equal? t 'List)
     (and (list? t) (equal? (first t) 'List) (equal? (second t) 'of) (type? (third t)))
     (and (list? t) (type? (first t))  (equal? (second t) '-> ) (type? (third t)))  ))

;; Predicado para constantes
(define (constant? x)
  (or (number? x) (boolean? x)))

;; Predicado para primitivos
(define (primitive? x)
  (or (procedure? x) (memq x '(and or + - * / > < car cdr length))))

(define-parser parse-L8 L8)

;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------

(define-language L9
  (extends L8)
  (Expr (e body)
        (- (lambda ([x* t*] ...) body* ... body)
           (e0 e1 ...))
        (+ (lambda ([x t]) body* ... body)
           (e0 e1))))

(define-parser parse-L9 L9)

; Se encarga de currificar las expresiones lambda así como las aplicaciones de función.
(define-pass curry : L8 (ir) -> L9 ()
  (Expr : Expr (e) -> Expr ()
        [(lambda ([,x* ,t*] ...) ,[body])
         (let f ([jx* x*]
                 [jt* t*])
           (if (equal? (length jx*) 1)
               `(lambda ([,(car jx*) ,(car jt*)]) ,body)
               `(lambda ([,(car jx*) ,(car jt*)]) ,(f (cdr jx*) (cdr jt*)))))]
        [(,[e0] ,[e1] ...)
         (let f ([be0 e0]
                 [be1 e1])
           (if (equal? (length be1) 0)
               `,be0
               (f `(,be0 ,(car be1)) (cdr be1))))]))

;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------


(define-language L10
  (extends L9)
  (Expr (e body)
        (- (quot c))
        (+ (const t c))))

(define-parser parse-L10 L10)

; Se encarga de colocar las anotaciones de tipos correspondientes
; a las constantes de nuestro lenguaje.
(define-pass type-const : L9 (ir) -> L10 ()
  (Expr : Expr (e) -> Expr ()
        [(quot ,c)
         (cond
           [(boolean? c) `(const Bool ,c)]
           [(number? c) `(const Int ,c)]
           [(char? c) `(const Char ,c)])]))

;;-------------------------------------------------------------------------------------------------------------------------------------------------------------------------
; Función auxiliar para verificar si t1 es unificable con t2 sin regresar el unificador.
(define (unify t1 t2)
	(if (and (type? t1) (type? t2))
		(cond 
                  [(equal? t1 t2) #t] ; Si son iguales son unificables.
                  [(and (equal? 'List t1) (list? t2)) (equal? (car t2) 'List)] ; Si ambos son del tipo 'List
                  [(and (equal? 'List t2) (list? t1)) (equal? (car t1) 'List)] ; Si ambos son del tipo 'List
                  [(and (list? t1) (list? t2)) (and (unify (car t1) (car t2)) (unify (caddr t1) (caddr t2)))]
                  ; Llamada recursiva en listas
                  [else #f])
                (error "Se esperaban 2 tipos")))

;; Función auxiliar para buscar la variable en el contexto.
(define (get x ctx)
  (cond
    [(empty? ctx) (error "La variable no está en el contexto.")]
    [(equal? x (caar ctx)) (cdr(car ctx))]
    [else (get x (cdr ctx))])) ; Recursión sobre el resto del contexto.

;; Función auxiliar para agregar una variable y su tipo en el contexto.
(define (add-to-ctx x t ctx)
  (cons (cons x t) ctx))

;; Funcion que verifica que dada una lista de tipos args, estos tipos sean los correctos para la operacion pr
(define (check-args-types pr args )
    (case pr
        [(+ - * /) (andmap (lambda (x) (equal? x 'Int)) args) ]  ;; Estas primitivas son sobre enteros
        [(car cdr length) (andmap (lambda (x) (and (list? x) (equal? (car x) 'List))) args) ] ))  ;; Estas operaciones van sobre listas

;; Algoritmo J que infiere el tipo para expresiones de lenguaje Exp.
;; A continuación se implementan las reglas de los constructores del lenguaje.
(define (J expr ctx)
  (nanopass-case (L10 Expr) expr
                 ;; Variables
                 [,x (get x ctx)] ; Buscamos en el ambiente si es que son variables.
                 ;; Constantes
                 [(const ,t ,c) t] ; En constantes ya tenemos el tipo en sí misma.
                 ;; Expresión Begin.
                 [(begin ,e* ... ,e) (J e ctx)] ; Regresamos el tipo de la última expresión.
                 ;; Expresión Primapp.
                 [(primapp ,pr ,e* ...)
                  (if (check-args-types pr  (map (lambda (x) (J x ctx)) e*) )
                      (case pr
                        [(+ - * / length) 'Int]
                        [(car) (caddr (car  (map (lambda (x) (J x ctx)) e*)))]
                        [(cdr) (car  (map (lambda (x) (J x ctx)) e*) )])
                      (error 'J "Los tipos de ~a no son compatbles para la operacion ~a" e* pr))]
                 
                 ;; Expresión de tipo if, e0 tiene que ser Bool y e1 y e2 tienen que ser unificables.
                 [(if ,e0 ,e1 ,e2)
                  ; Calculamos el tipo de cada sub-expresión.
                  (let* ([t0 (J e0 ctx)]
                        [t1 (J e1 ctx)]
                        [t2 (J e2 ctx)])
                    (if (and (unify t0 'Bool) (unify t1 t2)) ; Verificamos si los tipos son correctos.
                        t1
                        (error "Verificar los tipos de las expresiones y que sean unificables.")))]
                 ;; Expresión Lambda en donde el tipo es t -> type(body).
                 [(lambda ([,x ,t]) ,body)
                  (let* ([new-ctx (add-to-ctx x t ctx)] ; Contexto con la nueva variable.
                         [type (J body new-ctx)]) ; El tipo con la nueva variable.
                    (list t '-> type))]
                 
                 ;; Expresión de tipo let, el tipo se obtiene el tipo del body y se agrega al contexto.
                 [(let ([,x ,t ,e]) ,body)
                  (if
                   (unify t (J e ctx))
                   (J body (add-to-ctx x t ctx))
                   (error "Ocurrió un error al unificar"))]

                 ;; Mismo caso que el let anterior, pero extendemos el contexto.
                 [(letrec ([,x ,t ,e]) ,body)
                  (let*
                      ([extended-ctx (add-to-ctx x t ctx)]
                       [extended-type (J e extended-ctx)]
                       [body-type (J body extended-ctx)])
                    ; Verificamos que el tipo de e sea unificable.
                    (if (unify t extended-type)
                        body-type
                        (error "El tipo especificado no corresponde con el tipo de la expresión.")))]
                 
                 ;; Expresión de tipo letfun, devolvemos el tipo del body agregando al contexto.
                 [(letfun ([,x ,t ,e]) ,body)
                  (if (and (unify t (J e ctx)) (equal? '-> (cadr t)))
                      (J body (add-to-ctx x t ctx))
                      (error "Ocurrió un error al unificar"))]

                 ;; Expresiones de tipo lista.
                 [(list ,e* ...)
                  (cond
                    [(empty? e*) 'List]
                    [else
                     (let* (
                            [elems-type (map (lambda (x) (J x ctx)) e*)]
                            [T-type (foldr (lambda (x acc) (if (equal? 'List x) acc x)) (car elems-type) (cdr elems-type))]
                            [homogenea? (andmap (lambda (x) (unify T-type x)) elems-type)])
                       (if homogenea?
                           (list 'List 'of T-type)
                           (error "Los tipos de elementos de la lista no son iguales.")))])]
                            
                 ;; Expresión que aplica una función a una expresión.
                 [(,e0 ,e1)
                  (let* ([t0 (J e0 ctx)]
                         [t1 (J e1 ctx)])
                    (if (and (list? t0) (equal? (cadr t0) '->) (unify (car t0) t1))
                        (caddr t0)
                        (error "No es posible hacer la aplicación de función ya que los tipos son incompatibles")))]
                 
                 [else "Expresión incompatible."]))

; Se encarga de quitar la anotación de tipo Lambda y sutituirlas por el tipo `(T -> T)
(define-pass type-infer : L10 (e) -> L10 ()
  (Expr : Expr (e) -> Expr ()
        [(let ([,x ,t ,[e]]) ,[body])
         (let ([update-type
                (case t
                  [(list) (J e '())]
                  [else t])])
           `(let ([,x ,update-type ,e]) ,body))]
        [(letrec ([,x ,t ,[e]]) ,[body])
         (let ([update-type
                (case t
                  [(list) (J e '())]
                  [(Lambda) (J e '())]
                  [else t])])
           `(letrec ([,x ,update-type ,e]) ,body))]
        [(letfun ([,x ,t ,e]) ,[body])
         `(letfun ([,x ,(J e '()) ,e]) ,body)]))


;; Prueba Curry.
;(curry (parse-L8 '(foo x y)))
;(curry (parse-L8 '(lambda ([x Int] [y Int]) (primapp + x y))))

;; Prueba type-const.
;(type-const (parse-L9 '(quot 5)))

;; Prueba función J.
(J (parse-L10 '(lambda ([x Int]) x)) '())
(J (parse-L10 '(const Int 5)) '())
(J (parse-L10 '(primapp cdr (list (const Int 6) (const Int 5)))) '())
(J (parse-L10 '(list)) '())
(J (parse-L10 '(list (const Bool #t) (const Bool #f))) '())

;; Prueba función type-infer.
(type-infer (parse-L10 '(let ([x List (list (const Int 1) (const Int 2) (const Int 3) (const Int 4))]) x)))
(type-infer (parse-L10 '(letrec ([foo Lambda (lambda ([x Int]) x)]) (foo 5))))
(type-infer (parse-L10 '(let ([x List (list)]) x)))
(type-infer (parse-L10 '(let ([x List (list 1 2 3 4)]) x)))