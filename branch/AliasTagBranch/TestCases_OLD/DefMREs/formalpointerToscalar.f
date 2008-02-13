
        subroutine foo(p,t)
             double precision, pointer :: p
             double precision, target :: t
             double precision :: t
             p=>t
             p = x
        end subroutine
