
      subroutine foo(p)
           double precision, pointer :: p(:)
           double precision, dimension(5), target :: t
           double precision :: x
           integer i
           p=>t
           p(i) = x
      end subroutine
