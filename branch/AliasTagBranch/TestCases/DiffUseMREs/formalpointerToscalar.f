
        subroutine foo(p,t)
             double precision, pointer :: p
             double precision, target :: t
             double precision :: t
             p=>t
             x = p
        end subroutine
