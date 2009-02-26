
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! multiple_calls3.f
!
!                   ICFGActivity
!                   ============
! Features:
!            [X] a kill of a useful/vary variable 
! Testing :
!            [X] ActiveStmts: None
!            [X] ActiveMemRefExprs: none
!            [X] ActiveSyms:  none
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!              Example:
!              =======
!

      subroutine foo()
          double precision ::x,a,b,c,y
c$openad INDEPENDENT(x)
          
           a = x
!                          fwd: a is Vary
           b = 2

           b = 3
!                          bwd: a should no longer be useful
           a = 4
!                          fwd: a should no longer be vary
           b = 5

           b = 6
!                          bwd: a is Useful
           y = a           

c$openad DEPENDENT(y)
      end subroutine


