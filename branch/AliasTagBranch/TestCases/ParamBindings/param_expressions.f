
      subroutine foo
          double precision :: a,b,c
          double precision, dimension(5) :: d
          integer :: i
          call bar(a, b+c, 2, d(i))

!                          Formal   => Actual
!                          bar::*p  => MemRefNode(foo::a)
!                          bar::*q  => MemRefNode(foo::UnnamedRef(b+c))          
!                          bar::*r  => MemRefNode(foo::UnnamedRef(2))
!                          bar::*s()=> MemRefNode(foo::d())          

      end subroutine

      subroutine bar(p,q,r,s)
          double precision :: p,q,r
          double precision, dimension(5) :: s
          integer :: k
          p=q+r+s(k)
      end subroutine
