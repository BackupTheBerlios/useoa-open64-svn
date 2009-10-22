
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! multiple_calls8.f
! checking reaching constants through multiple calls
!
! CallGraph (left-to-right)
!
!  cat------f1--------\
!     \----------f2----foo-----b1-------\
!  dog-------f3-------/   \--------b2----bar
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!              Example:
!              =======
!

      subroutine cat()
          integer :: i,j,k
c$openad INDEPENDENT(i)
          i = 9            !formal x in foo should get 9
          j = 8
          k = 1
          call foo(i,j)    ! j <== 18 + 4 = 22
          j = 9            !formal x in foo should get 9
          k = 2
          call foo(j,i)    ! i <== 18 + 4 = 22
          k = j
c$openad DEPENDENT(k)
      end subroutine

      subroutine dog(p,q)
          integer :: p,q,r
          p = 9            !formal x in foo should get 9
          r = 1
          call foo(p,q)    ! q <== 18 + 4 = 22
          r = q

      end subroutine
          
      subroutine foo(x,y)
          integer :: x,y
          integer :: a,b,c,d,e,h
          
           a = x           !formal x should have 9, a should get 9
                           !formal f in bar should get 9

!                          fwd: a is Vary
           b = 2

           call bar(a,d)   ! d <== 18

           b = 3
!                          bwd: a should no longer be useful
           a = 4
!                          fwd: a should no longer be vary
           b = 5

           call bar(9,c)   !formal f in bar should get 9, c <== 18

           b = 6
!                          bwd: a,c is Useful
           y = c + a           
!
!           call bar(a,h)
!
!           b = 7

c$openad DEPENDENT(y)
      end subroutine

      subroutine bar(f,g)
          integer :: f,g
          
          g = 2*f          !formal f should be 9, g should get 18

      end subroutine

!
! ==========================================================================
