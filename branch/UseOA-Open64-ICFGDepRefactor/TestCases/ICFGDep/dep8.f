



      subroutine foo(p)
           double precision, pointer :: p(:)
           double precision, dimension(5), target :: t
           double precision :: x
           integer i
           p=>t
           x = p(i)
      end subroutine 
 
