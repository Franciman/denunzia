; We want to write a very simple programming language with an interesting type system
; We will defining the syntax of our language. This is the core, and we use de brujin indices to deal with scoping

; BuiltinValues := Int | String

; Term := BuiltinValues
;       | Variable
;       | Term Term                     [ function application, left associative ]
;       | `lambda` Identifier `.` Term  [ lambda abstraction ]

; Type := Int
;       | String
;       | Type -> Type

; The terms are represented by these objects and their combinations.
; The are all pairs whose first element is a tag specifying the type of object we are dealing with

(define-record-type ident
    (make-ident v)
    ident?
    (v ident-value))
    
(define-record-type fun-app    
    (make-fun-app t1 t2)
    fun-app?
    (t1 fun-app-left)
    (t2 fun-app-right))
    
(define-record-type lam    
    (make-lam param body)
    lam?
    (param lam-param)
    (body lam-body))
    
; De Brujin indices tools 

; Remove an element (checked with eq?) from the list
(define (remove-from-list elm l)
    (cond ((null? l) l)
          ((eq? elm (car l)) (remove-from-list (cdr l)))
          (else (cons (car l) (remove-from-list (cdr l))))))

(define (free-vars term)
    (cond ((ident? term) (list (ident-value term)))
          ((fun-app? term) (append (free-vars (fun-app-left term))
                                   (free-vars (fun-app-right term))))
          ((lam? term) (remove-from-list (lam-param term) (free-vars (lam-body term))))))
          
(define (make-context vars start-index)
    (cond ((null? vars) '())
          (else (cons (cons (car vars) start-index) (make-context (cdr vars) (+ start-index 1))))))
              
                                   
(define (named->de-brujin term))
