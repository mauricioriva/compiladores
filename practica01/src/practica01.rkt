#lang racket

;; Ejercicio 1
(define (anios-dias k)
  (* k 365))

(define (dias-horas k)
  (* k 24))

(define (horas-minutos k)
  (* k 60))

(define (minutos-segundos k)
  (* k 60))

(define (anios-horas k)
  (dias-horas (anios-dias k)) )

(define (anios-minutos k)
  (horas-minutos (anios-horas k)))

(define (anios-segundos k)
  (minutos-segundos (anios-minutos k)))

(define (anios-a-hrs-min-seg k)
  (string-append "Horas: "
                 (string-append (number->string (anios-horas k))
                                (string-append " Minutos: "
                                               (string-append (number->string (anios-minutos k))
                                                              (string-append " Segundos: " (number->string (anios-segundos k))))))))

;; Ejercicio 2
(define (fibonacci x)
  (cond
    [(equal? x 0) 0]
    [(equal? x 1) 1]
    [else (+ (fibonacci (- x 1)) (fibonacci (- x 2))) ]))

;; Ejercicio 3a)

;; Definición de una estructura hoja.
(define-struct leaf (value) #:transparent)

;; Definición de una estructura árbol.
(define-struct node (value left right) #:transparent)

;; Función principal que manda a llamar a la auxiliar con el divisor
;; más pequeño que será 2.
(define (div-tree n)
  (div-tree-aux n 2)
  )

;; Auxiliar para ir iterando los divisores.
(define (div-tree-aux n div)
  ; Auxiliar calcula el termino dividido.
  (let ([termino-div (/ n div)])
  (cond
    [(= n div) (leaf n)]
    
    ; En caso de que hayamos encontrado un divisor creamos el nodo y calculamos
    ; lo restante haciendo la llamada recursiva.
    [(= (modulo n div) 0) (node n (leaf div) (div-tree termino-div))]
    
    ; En otro caso hacemos la llamada recursiva con el siguiente divisor.
    [else (div-tree-aux n (+ div 1))]))
)

;; Ejercicio 3b)
(define (not-divide x l)
  (cond
    [(exact-integer? (/ l x)) #f]
    [else #t]
   ))

(define (filtra-lista x lista)
  (filter (lambda (n) (not-divide x n)) lista))

(define (nlist n)
  (cond
    [(< n 1) '()]
    [else (append (nlist (- n 1)) (list n))]))

(define (nlist-no-one k) (rest (nlist k)))

(define (prime-fac list)
  (cond
    [(empty? list) list]
    [else (append (first list) (prime-fac (filtra-lista (first list) (rest list))) )]))

;; Ejercicio 4

;; T := e | n T T
;; e es el arbol vacio y n un entero

(define-struct e () #:transparent)
(define-struct tree (n l r) #:transparent)

(define (new-level root left right original res)
  (if (empty? left)
      res
      (if (empty? right)
          (new-level root (cdr left) original original res)
          (new-level root left (cdr right) original (cons (tree root (car left) (car right)) res)))))

(define (enumerative-aux n l)
  (if (zero? n)
      (cons (e) l)
      (let ([l1 (cons (e) l)])
            (enumerative-aux (- n 1) (append (new-level l l1 l1 l1 empty) (new-level 2 l1 l1 l1 empty))))))

(define (enumerative n)
  (enumerative-aux n empty))
