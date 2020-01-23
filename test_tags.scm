; illegal input:
;; if you uncomment each any of this lines- the reader suppose to throw exception
; '(#{x}=(#{x} (#{x}=2)))
; '(#{x}=1 #{y}=3 #{x}=5)
; '(#{x}= (#{y}= (#{x}=3)))

; legal:
;; just need to be compiled, see that not throwing exception
(define tmp0 #{x}= (#{x} (#{y}=2)))
(define tmp1 '(#{x}=1 #{y}=3 #{z}=5))
(define tmp2 #{x}=(#{y}=(#{z}=3)))
(define tmp3 '(#{x} (#{x}=(#{z}=3 #{x}))))
(define tmp4 '(#{lst}=(a b . #{lst})))


;case 0:
(car(car(cdr(car(cdr(car(cdr(car(cdr #{lst}=( a #{lst}  b))))))))))

; case 1:
(define l1 #{x}= ('a 'b #{x}))

(define l2 (list #{x}=(1 2 #{y})
                #{y}=(3 4 #{x}))
)                

(car l1)
(car (car l2))

(car (cdr (car (cdr (cdr l1)))))

(car (car (cdr (cdr (car l2)))))
(car (car (cdr (cdr (car (cdr l2))))))


;case 2:
(define l3 (list #{x}=(1 2 . #{y})
                #{y}=(3 4 #{x}))
) 

(car (cdr (cdr (car l3))))
(car (car (cdr (cdr (car (cdr l3))))))


;case 3:
(define l4 
    (list    
        #{db1}=(1 . #{db4}) 
        #{db5}=(5 . #{db1})
        #{db3}=(3 . #{db5})
        #{db6}=(6 . #{db3})
        #{db9}=(9 . #{db6})
        #{db4}=(4 . #{db9})
))

(equal?
    (car (cdr (cdr (cdr (cdr (cdr (cdr (car l4))))))))
    (car (car l4))
)

;case 4:
(define t
    #{a}=(1 . #{b}=((#{b} . 2) 3 . #{a}))
)
(car t)
(car(cdr(cdr t)))

(car (cdr(car(car(cdr t)))))
(cdr (car(car(car(cdr t)))))
(car (cdr (cdr(car(car(cdr t))))))


; output:
; a
; (quote a)
; 1
; (quote b)
; 3
; 1
; 3
; 1
; #t
; 1
; 3
; 3
; 2
; 1
