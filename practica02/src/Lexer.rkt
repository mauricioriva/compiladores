#lang nanopass

;; Bibliotecas chidas para lexear
(require parser-tools/lex
         parser-tools/lex-plt-v200
         (prefix-in : parser-tools/lex-sre);Operadores
         (prefix-in re- parser-tools/lex-sre)
         parser-tools/yacc)

(provide (all-defined-out));Exporta todos los identificadores que están definidos en el  nivel
;de fase relevante dentro del módulo de exportación, y que tienen el mismo contexto léxico

(define-tokens a (NUM VAR BOOLEAN))
(define-empty-tokens b (LP RP LL RL LC RC ; Signos de agrupación.
                        + - * / = ; Signos de operaciones.
                        IF THEN ELSE LET IN END APP AND OR ; Palabras reservadas.
                        EOF FUNF FUN : => INT BOOLE FUNC))

; Abreviación para números enteros.
(define-lex-abbrev digits (:+ (char-set "0123456789")))

; sre : S-regular expressions
(define minHS-lexer
           (lexer
             ["if"
              ; =>
              (token-IF)]

             ["then"
              ; =>
              (token-THEN)]

             ["else"
              ; =>
              (token-ELSE)]

             ["fun"
              ; =>
              (token-FUN)]

             ["funF"
              ; =>
              (token-FUNF)]

             ["let"
              ; =>
              (token-LET)]

             ["in"
              ; =>
              (token-IN)]

             ["end"
              ; =>
              (token-END)]

             ["app"
              ; =>
              (token-APP)]
             
             ["("
              ; =>
              (token-LP)]

             [")"
              ; =>
              (token-RP)]

             ["{"
              ; =>
              (token-LL)]

             ["}"
              ; =>
              (token-RL)]

             ["["
              ; =>
              (token-LC)]

             ["]"
              ; =>
              (token-RC)]

             [":"
              ; =>
              (token-:)]

             ["=>"
              ; =>
              (token-=>)]

             [(:or "#t" "#f")
              ; =>
              (token-BOOLEAN lexeme)]

             ["Int"
              ; =>
              (token-INT)]

             ["Bool"
              ; =>
              (token-BOOLE)]

             ["Func"
              ; =>
              (token-FUNC)]
             
             [(concatenation (:+ (char-range #\a #\z) (char-range #\A #\Z)) (? (:seq digits)))
              ;=>
              (token-VAR (string->symbol lexeme))]

             [(:seq digits)
              ;=>
              (token-NUM (string->number lexeme))]

             ["+"
              ; =>
              (token-+)]

             ["-"
              ; =>
              (token--)]

              ["/"
              ; =>
              (token-/)]

              ["*"
              ; =>
              (token-*)]

              ["="
               ; =>
               (token-=)]

             [whitespace
              ; =>
              (minHS-lexer input-port)]

             [(eof)
              (token-EOF)]))

;; Ejemplos:
#|

(let ((input (open-input-string "(182 * 23)")))
  (minHS-lexer input))

(let ((input (open-input-string "#t")))
  (minHS-lexer input))

(let ((input (open-input-string "69")))
  (minHS-lexer input))

(let ((input (open-input-string "if (#t) then {#t} else {#f}")))
  (minHS-lexer input))

(let ((input (open-input-string "funF (var1 [var2:Bool]:Bool) => 34")))
  (minHS-lexer input))

(let ((input (open-input-string "Var1")))
  (minHS-lexer input))

(let ((input (open-input-string "Bool")))
  (minHS-lexer input))
|#
