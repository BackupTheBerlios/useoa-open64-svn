
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! multiple_calls11a.f
! checking activity analysis through multiple calls
!
! CallGraph (left-to-right)
!
!  cat------f1--------\
!     \----------f2----foo-----b1-------\
!                        \--------b2----bar
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!              Example:
!              =======
!

      subroutine cat()
          integer :: i,j,k,m
c$openad INDEPENDENT(i)

          m = 1
                           ! 

          call foo(i,k)    ! formal x in foo(i,k) should be vary going in
                           ! formal y in foo(i,k) should be vary coming out
                           ! k should be vary, now

                           ! k is vary and useful here, so active.
          m = 2

          call foo(m,i)    ! formal x in foo(m,i) is not vary
                           ! formal y in foo(m,i) get's def'd from m (not vary coming out)
                           ! i should no longer be vary

          j = k            ! j becomes vary


c$openad DEPENDENT(j)
      end subroutine

          
      subroutine foo(x,y)
          integer :: x,y
          integer :: a,b,c,d
          
           a = x           !formal x and a in foo(i,k) are vary
                           !formal x and a in foo(m,i) are not vary

           b = 2

           call bar(a,d)   ! foo(i,k) d becomes vary
                           ! foo(m,i) d does not vary
                           ! since d is never useful, call is never active

           b = 3

           call bar(a,c)   ! foo(i,k) c becomes vary
                           ! foo(m,i) c does not vary
                           ! since c is useful in foo(i,k), call is active

           b = 5

           y = c + a       ! foo(i,k) y becomes vary
                           ! foo(m,i) y does not vary

      end subroutine

      subroutine bar(f,g)
          integer :: f,g
          
          g = 2*f          !

      end subroutine

!
! ==========================================================================
