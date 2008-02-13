!
! Jean2.f90
!
! displays possible MOD. LMOD imprecision for x
!

 subroutine foo(m,t) 
 integer :: m,t

   t = m

 end subroutine

 subroutine head() 
   integer x
   integer p,q

   call foo(x,q)

   p = 5

   call foo(p,q)
   
 end subroutine


