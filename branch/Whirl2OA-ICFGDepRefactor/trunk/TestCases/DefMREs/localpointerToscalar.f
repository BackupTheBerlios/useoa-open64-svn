
      subroutine foo
           
           double precision, pointer :: p
           double precision, target :: t
           double precision :: x
           p=>t
           p = x
      end subroutine
