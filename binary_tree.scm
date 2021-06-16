; Red-Black binary tree
(define-record-type rb-node
    (make-rb-node val color left right)
    rb-node?
    (val rb-node-value)
    (color rb-node-color)
    (left rb-node-left)
    (right rb-node-right))
    
(define (rb-node-upd-color node new-color)
    (make-rb-node (rb-node-value node)    
                  new-color
                  (rb-node-left node)
                  (rb-node-right node)))
                  
(define (rb-node-upd-left node new-left)
  (make-rb-node (rb-node-value node)    
                (rb-node-color node)
                new-left
                (rb-node-right node)))
                
(define (rb-node-upd-right node new-right)
    (make-rb-node (rb-node-value node)    
                  (rb-node-color node)
                  (rb-node-left node)               
                  new-right))
                
(define (rb-node-upd-children node new-left new-right)                
    (make-rb-node (rb-node-value node)
                  (rb-node-color node)
                  new-left
                  new-right))

    
; Null sentinel value    
(define rb-null 'null-node)

(define (rb-null? n) (eq? 'null-node n))
    
; A tree is either an rb-node or a rb-null
(define (rb-tree? t)
    (or (rb-null? t) (rb-node? t)))
    
; Sentinels are black    
(define (rb-tree-color t)
    (cond ((rb-null? t) 'black)
          ((rb-node? t) (rb-node-color t))))
          
(define (node-is-red t) (eq? (rb-tree-color t) 'red))          
(define (node-is-black t) (eq? (rb-tree-color t) 'black))          
          
; When inserting, we insert a red node, and then recolor          
(define (new-insert-node val) (make-rb-node val 'red rb-null rb-null))

; Compare function
(define (compare a b)
    (cond ((< a b) -1)
          ((> a b) 1)
          ((= a b) 0)))

; Balance after an insert in the left child
(define (rotate-left tree)
    (let* ((right (rb-node-right tree))
           (right-left (rb-node-left right)))
         (rb-node-upd-left right (rb-node-upd-right tree right-left))))
         
(define (rotate-right tree)
    (let* ((left (rb-node-left tree))
           (left-right (rb-node-right left)))
         (rb-node-upd-right left (rb-node-upd-left tree left-right))))
         

; Check if there is a left left violation
(define (left-left-violation tree)
    (if (node-is-red (rb-node-left tree))
        (node-is-red (rb-node-left (rb-node-left tree)))
        #f))
        
; Fix left left violation, somehow        
(define (fix-left-left tree)
    ; If the right child is red, it is easy, we just do recoloring
    (cond ((node-is-red (rb-node-right tree)
                (rb-node-upd-children (rb-node-upd-color tree 'red)
                                  (rb-node-upd-color (rb-node-left tree) 'black)
                                  (rb-node-upd-color (rb-node-right tree) 'black))))
          ; Otherwise only a right rotation can save our day, and after we recolor
          (else (let ((new-tree (rotate-right tree)))
                     (rb-node-upd-color 
                         (rb-node-upd-right (rb-node-upd-color (rb-node-right new-tree) 'red))
                         'black)))))

; Check for a left right violation
(define (left-right-violation tree)    
    (if (node-is-red (rb-node-left tree))
        (node-is-red (rb-node-right (rb-node-left tree)))
        #f))
; Fix left right violation, somehow        
(define (fix-left-right tree)        
    ;we perform a left rotation, to get to the left-left case
    (let ((new-left (rotate-left (rb-node-left tree))))
         (fix-left-left (rb-node-upd-left tree new-left))))

(define (balance-insert-left tree)
    ; First check if we have red parent - red child violation
    ; We are not sure we can directly fix this violation, we have to go two
    ; levels up to fix violations, so we directly check if there is a two levels down violation,
    ; otherwise there is nothing to do.
    ; There are two possible violations:
    ; - Left child is red, and has a red left child
    ; - Left child is red, and has a red right child
    (cond ((left-left-violation tree) fix-left-left) 
          ((left-right-violation tree) fix-left-right)
          ; If no red-red violation, there is no other violation, so we can stop
          (else tree)))

(define (right-right-violation tree)
    (if (node-is-red (rb-node-right tree))
        (node-is-red (rb-node-right (rb-node-right tree)))
        #f))

(define (fix-right-right tree)
    ; If the right child is red, it is easy, we just do recoloring
    (cond ((node-is-red (rb-node-left tree)
                (rb-node-upd-children (rb-node-upd-color tree 'red)
                                  (rb-node-upd-color (rb-node-left tree) 'black)
                                  (rb-node-upd-color (rb-node-right tree) 'black))))
          ; Otherwise only a left rotation can save our day, and after we recolor
          (else (let ((new-tree (rotate-left tree)))
                     (rb-node-upd-color 
                         (rb-node-upd-left (rb-node-upd-color (rb-node-left new-tree) 'red))
                         'black)))))

        
(define (right-left-violation tree)
    (if (node-is-red (rb-node-right tree))
        (node-is-red (rb-node-left (rb-node-right tree)))
        #f))
        
; Fix left right violation, somehow        
(define (fix-right-left tree)        
    ;we perform a right rotation, to get to the right-right case
    (let ((new-right (rotate-right (rb-node-right tree))))
         (fix-right-right (rb-node-upd-right tree new-right))))

        
        
(define (balance-insert-right tree)
    (cond ((right-right-violation tree) fix-right-right)
          ((right-left-violation tree) fix-right-left)
          (else tree)))
          
(define (rb-tree-insert-helper val tree)
    (let ((new-node (new-insert-node val)))
         (cond ((rb-null? tree) new-node)
               ((< (compare val (rb-node-value tree)) 0)
                (balance-insert-left (rb-node-upd-left tree
                                         (rb-tree-insert-helper val (rb-node-left tree)))))
                                                  
               ((> (compare val (rb-node-value tree)) 0)
                (balance-insert-right (rb-node-upd-right tree
                                          (rb-tree-insert-helper val (rb-node-right tree)))))
               (else tree))))
               
; We need to recolor the root to black               
(define (rb-tree-insert val tree)
    (rb-node-upd-color (rb-tree-insert-helper val tree) 'black))               
               
; Returns a pair (minimum new-tree-with-min-removed)               
(define (find-min-and-remove tree)
    (cond ((rb-null? (rb-node-left tree)) (cons tree (rb-node-right tree)))               
          (else (let* ((res (find-min-and-remove (rb-node-left tree)))
                       (minimum (car res))
                       (new-left-tree (cdr res)))
                      (cons minimum (rb-node-upd-left tree new-left-tree))))))
               
(define (rb-tree-remove val tree)               
    (cond ((rb-null? tree) tree)
          ((< (compare val (rb-node-value tree)) 0)
           (rb-node-upd-left tree (rb-tree-remove val (rb-node-left tree))))
                      
          ((> (compare val (rb-node-value tree)) 0)
           (rb-node-upd-right tree (rb-tree-remove val (rb-node-right tree))))
             ; We want to put the minimum of the right subtree here
          ((rb-null? (rb-node-right tree)) (rb-node-left tree))
          (else (let* ((res (find-min-and-remove (rb-node-right tree)))
                       (right-min (car res))
                       (new-right (cdr res)))
                      (make-rb-node (rb-node-value right-min)
                                    (rb-node-color right-min)
                                    (rb-node-left (rb-node-left tree))
                                    new-right)))))
                                     
(define (rb-tree-lookup val tree)
    (cond ((rb-null? tree) #f)                                     
          ((< (compare val (rb-node-value tree)) 0) (rb-tree-lookup val (rb-node-left tree)))
          ((> (compare val (rb-node-value tree)) 0) (rb-tree-lookup val (rb-node-right tree)))
          (else (rb-node-value tree))))
