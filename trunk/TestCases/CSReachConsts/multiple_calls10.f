
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! multiple_calls10.f
! checking reaching constants through multiple calls
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
          integer :: i,j,k
c$openad INDEPENDENT(i)
          i = 9            ! formal x in foo(i,k) should get 9
          k = 7            !
          j = 1
          call foo(i,k)    ! k <== 30
                           ! formal x in foo(k,i) should get 30
          j = 2
          call foo(k,i)    ! i <== 93
          j = i
c$openad DEPENDENT(j)
      end subroutine

          
      subroutine foo(x,y)
          integer :: x,y
          integer :: a,b,c,d
          
           a = x           !formal x and a in foo(i,k) should get 9
                           !formal x and a in foo(k,i) should get 30

!                          fwd: a is Vary
           b = 2

           call bar(d,a)   ! foo(i,k) d <== 18, foo(k,i) d <== 60

           b = 3
!                          bwd: a should no longer be useful
           a = a + 1       ! foo(i,k) a <== 10, foo(k,i) a <== 31
!                          fwd: a should no longer be vary
           b = 4

           call bar(c,a)   ! foo(i,k) c <== 20, foo(k,i) c <== 62

           b = 5
!                          bwd: a,c is Useful
           y = c + a       ! foo(i,k) y <== 30, foo(k,i) y <== 93

c$openad DEPENDENT(y)
      end subroutine

      subroutine bar(g,f)
          integer :: g,f
          
          g = 2*f          !

      end subroutine

!
! ==========================================================================
