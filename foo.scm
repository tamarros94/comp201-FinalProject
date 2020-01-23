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