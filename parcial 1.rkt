#lang eopl


;;Punto 1
(define sumarHojas
(lambda (x)
(cond
[(null? x) 0]
[(number? x) x]
[(symbol?(car x)) (+ (sumarHojas (cadr x)) (sumarHojas(caddr x)) (sumarHojas (cadddr x)))
]
[else 'error]
) 
)
)

(define y '(a 7 (b 8 3 2) (e 1 () 4) ))

;;Punto 2
(define sumarCiertasHojas
(lambda (x f)
(cond
[(null? x) 0]
[(and(number? x) (f x)) x]
[(and(number? x) (not (f x))) 0]
[(symbol? (car x)) (+ (sumarCiertasHojas (cadr x) f) (sumarCiertasHojas(caddr x) f) (sumarCiertasHojas (cadddr x) f))
]
[else 'error]
) 
)
)

;;Abstraccion de datos

(define-datatype arbol-t arbol-t?
  (empty-arb)
  (leaf (n number?))
  (non-empty-arb (n symbol?) (arb1 arbol-t?) (arb2 arbol-t?) (arb3 arbol-t?))
  )

;;Creando un Parser
(define Parse
  (lambda (x)
    (cond
      [(null? x ) empty-arb]
      [(number? x) (leaf x)]
      [(non-empty-arb (car x) (Parse(cadr x)) (Parse(caddr x)) (Parse(cadddr x)))]

      ) 
    )
  )

;;Creando un Unparse
;;Transforma sintaxis abstracta en concreta 
 
(define Ternario-UnParse
( lambda (L)
   (cases arbol-t L
     (empty-arb () '())
     (leaf (n) n)
     (non-empty-arb (s arb1 arb2 arb3)
                    (list s (Ternario-UnParse arb1) (Ternario-UnParse arb2) (Ternario-UnParse arb3))
                    )
       
       )
   ))