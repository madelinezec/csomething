type program = 
    | Program of decls

and decls =
    | Typ of id * decls_prime
    | Epsilon

and decls_prime =
    | Vdecls  

    
