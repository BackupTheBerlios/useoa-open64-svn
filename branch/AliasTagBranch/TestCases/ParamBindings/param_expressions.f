

! Example:
! ========

      subroutine foo
          double precision :: a,b,c
          double precision, dimension(5) :: d
          integer :: i
          call bar(a, b+c, 2, d(i))


      end subroutine

      subroutine bar(p,q,r,s)
          double precision :: p,q,r
          double precision, dimension(5) :: s
          integer :: k
          p=q+r+s(k)
      end subroutine




 
! ParamBindings (Formal => Actual) : 
! ================================== 
! 
! - CallHandle = call bar(a, b+c, 2, d(i))
!
!     1.  p    => MemRefNode(&a)
!     2.  q    => MemRefNode(&UnnamedRef(b+c))
!     3.  r    => MemRefNode(&UnnamedRef(2))
!     4.  s()  => MemRefNode(&d())
!

