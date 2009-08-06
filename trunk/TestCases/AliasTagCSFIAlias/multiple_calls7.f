
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! multiple_calls7.f
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
          k = 1
          call foo(i,j)
          k = 2
          call foo(j,i)
          k = j
c$openad DEPENDENT(k)
      end subroutine

      subroutine dog(p,q)
          integer :: p,q,r

          r = 1
          call foo(p,q)
          r = q

      end subroutine
          
      subroutine foo(x,y)
          integer :: x,y
          integer :: a,b,c,d,e,h
          
           a = x
!                          fwd: a is Vary
           b = 2

           call bar(a,d)

           b = 3
!                          bwd: a should no longer be useful
           a = 4
!                          fwd: a should no longer be vary
           b = 5

           call bar(e,c)

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
          
          g = 2*f
      end subroutine

!
! ==========================================================================
