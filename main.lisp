;;;; Ejercicios de LISP 9/2020
;;;; Salomón Fereres y Adrián Blázquez 
;;;; Se usa https://www.tutorialspoint.com/execute_lisp_online.php
;;;; https://www.tutorialspoint.com/lisp/

; 2/7 no funcionan
; SETEQUAL, IMPARES no funcionan

;;;; alpha, beta
;;;; if (CAR(beta)=eq) return T; else F;
(DEFUN FIRSTP (ALPHA BETA)
    "FIRSTP toma dos argumentos, un símbolo y una lista, devolviendo T si el primer argumento es igual al primer elemento del segundo argumento."
    (EQUAL ALPHA (CAR BETA))
)

(write(FIRSTP 'A '(A B C))) ; T
(write(FIRSTP 'MARTES '(LUNES MARTES MIERCOLES))) ; NIL

(DEFUN FIRSTP2 (ALPHA BETA)
    "FIRSTP toma dos argumentos, un símbolo y una lista, devolviendo T si el primer argumento es igual al primer elemento del segundo argumento."
    (if (NULL ALPHA)
        ALPHA
        (EQUAL ALPHA (CAR BETA))
    )
)

;;;; LISTA
;;;; if (LIST != NIL && LIST.isList()) for (LISTA(i)) LISTANUEVA(i) = LIST(LISTA(i), LISTA(i)) 
(DEFUN DUPLICAR (LISTA)
    "FIRSTP toma como argumento una lsita y devuleve la lista cuyos elementos son pares (lista de dos elementos) compuestos por los elementos de la primera."
    (COND 
        ( (NULL LISTA) (NIL) )
        (T 
            (LIST (CAR LISTA) ( CAR LISTA) )
            (DUPLICAR ( CDR (LISTA) ) )
        )
    )
)

; (write(STEP (DUPLICAR '(A B C)))) ; EVAL: undefined function LISTA

(DEFUN DUPLICAR2 (l)
    "FIRSTP toma como argumento una lsita y devuleve la lista cuyos elementos son pares (lista de dos elementos) compuestos por los elementos de la primera."
    (if (NULL l)
        l
        (append (CONS (LIST (CAR l) (CAR l) ) ( DUPLICAR2 (CDR l) ) )  )
    )
)

(write(STEP (DUPLICAR2 '(A B C)))) ; ((A A) (B B) (C C))


;;;; N, i = 0
;;;; if (N > 0) { for (n = N, n--, n > 0) ListaNueva(i) = N - i; i++; }  else NIL;
(DEFUN COUNTDOWN (N)
    "COUNTDOWN toma como argumento un número N positivo y genera una lista de enteros desde N hasta 1."
    (COND 
        ( (<= N 0) NIL)
        (T 
            (CONS N (COUNTDOWN ( - N 1 ) )    ; Introduzco en la posición N el literal N 
            )
        )
    )
)

 (write(step(COUNTDOWN 3))) ; (3 2 1)
 (write(step(COUNTDOWN -1))) ; NIL
 
 ; Hasta que CDR sea NUL
 (DEFUN REVERSE2 (LISTA2)
    "REVERSE toma como argumento una lista y devuelve la lista invertida."
    (COND 
        ( (NULL LISTA2) NIL)
        ( T 
            ( CONS (REVERSE2 ( CDR LISTA2 )) CAR (LISTA2)    ; Introduzco en la posición N el literal N 
            )
        )
    )
)

 ; (write(step(REVERSE2 '(A B C)))) ; - COND: variable CAR has no value


 (DEFUN SUBSTITUE (X Y L)
    (COND
        ( (NULL L) L)
        ( (NULL X) X)
        ( (NULL Y) Y)
        ( (EQ X Y) L)
        ( (NULL (MEMBER(X L)) ) L )
        ( (NULL (MEMBER(Y L)) ) L )
        ( ( EQ (CAR L) Y) (CONS X SUBSTITUTE( X Y (CDR L))))
        (T (CONS (CAR L) SUBSTITUTE ( X Y (CDR L))))
    )
)
 (write(step(SUBSTITUTE 'D 'A '(A B A C)))) ; (D B D C)
 (write(step(SUBSTITUTE 'N 'M '(A B A C)))) ; (A B A C)
)

 ; (write(step(SUBSTITUTE2 'D 'A '(A B A C)))) ; EVAL: undefined function L
 
 ; Hay dos condiciones,  Si los dos CAR no son iguales, entonces NIL. 
 ; Si no se aplica la fucnión sobre lo restante
 (DEFUN SETEQUAL (L1 L2)
    " SUBSITUTE toma dos listas como argumentos y devuelve T si tos los elementos de la primera lsita están incluidos en la segundo y a la inversa. En caso contratio devuelve NIL."
    (COND 
        ( (NULL L1) NIL) ; Si la lista es nula
        ( (NULL L2) NIL) ; Si la lista es nula
        ;( (not (EQUAL CAR(L1) (CAR(L2)))) NIL) ; Si Y es el CAR(L) se construye la lista aplciando cdr
        ((MEMBER (CAR L1) L2) SETEQUAL((CDR (L1)) L2)) ; Si CAR L1 está en L2 se comprueba si el resto de L1 está en L2
    )
)

 ; (write(step(SETEQUAL '(B C C A) '(A B C)))) ; - COND: variable CAR has no value
 ; (write(SETEQUAL '(B C C A) '(A B C))) ; - COND: variable CAR has no value
 ; (write(SETEQUAL '(MARTES LUNES) '(LUNES MARTES MIERCOLES))) ; - SYSTEM::%EXPAND-FORM: (CDR (L1)) should be a lambda expression
 
 (DEFUN SUBSTITUE (X Y L)
    (COND
        ( (NULL L) L)
        ( (NULL X) X)
        ( (NULL Y) Y)
        ( (EQ X Y) L)
        ( (NULL (MEMBER(X L)) ) L )
        ( (NULL (MEMBER(Y L)) ) L )
        ( ( EQ (CAR L) Y) (CONS X SUBSTITUTE( X Y (CDR L))))
        (T (CONS (CAR L) SUBSTITUTE ( X Y (CDR L))))
    )
)
; (write(step(SUBSTITUTE 'D 'A '(A B A C)))) ; (D B D C)
 ;(write(step(SUBSTITUTE 'N 'M '(A B A C)))) ; (A B A C)
 
(DEFUN SETEQUAL (A B)
    (COND 
        ((NULL A) A)
        ((NULL B) B)
        (T (AND (CONTAINS A B)) (CONTAINS B A))
    )
)

(DEFUN CONTAINS(A B)
    (COND
        ((NULL A) T)
        ((MEMBER (CAR A) B) (CONTAINS (CDR A) B ))
        (T NIL)
    )
)

  ; structuirea parecida de encontrar que en sustituir, pero no construir una entera sino una con sólo unos
(defun IMPARES(A)
    (COND
        ((NULL A) A)
        ((NULL (CDR A)) (CAR A))
        (T (CONS (CAR A) (IMPARES (CDDR A) ) )  )
    )
)

(trace IMPARES)

(write(IMPARES '(A B C)))
(write(IMPARES '(A B C D E F)))
(write(IMPARES '()))
