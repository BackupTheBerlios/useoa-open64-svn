



      subroutine foo
           double precision, pointer :: p
           double precision, target :: t
           double precision :: x
           p=>t
           x = p
      end subroutine 
 
