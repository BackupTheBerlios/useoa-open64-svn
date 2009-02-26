
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! multiple_calls3.f
!
!                   ICFGActivity
!                   ============
! Features:
!            [X] a kill of a useful/vary variable between successive calls to 
!                same procedure.  We see it active throughout due to 
!                context-insensative, flow-insensative alias analysis?. 
! Testing :
!            [X] ActiveStmts: All Statements are Active
!            [X] ActiveMemRefExprs: x,a,y
!            [X] ActiveSyms:  x,a,y
!
! Note:    All Definitions are MayDefs ?????? 
!          BK: this Note was here from TestCases/Activity/multiple_calls2.f
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

           call bar(b)

           b = 3
!                          bwd: a should no longer be useful
           a = 4
!                          fwd: a should no longer be vary
           b = 5

           call bar(c)

           b = 6
!                          bwd: a is Useful
           y = a           

c$openad DEPENDENT(y)
      end subroutine

      subroutine bar(f)
          double precision :: f
          
          f = 2*f
      end subroutine

!
! ==========================================================================
!
!              Intutive Analysis
!              ==================
!  Intutive analysis first flatten the procedure calls. Then using
!  manual analysis, we are trying to find out value flow path between
!  Independent and Dependent variables.
!
!  Below shows the Intutive analysis result using 3-tuple information
!  [Program Point(PP), stmt, Path]
!
!  We explored all possible paths from independent variable. In order
!  to avoid confusion, we keep track of each and every path seperately.
!
!  please consider that x1, b4 indicate symbols and program point at which
!  they are defined. This helps us to keep track of recent definition
!  along various paths under examination.
!
!
! PP Stmt   Paths
!
! 1. a=x     x1->a1
!
! 2. b=2     x1->a1
!
! 3. f=b     x1->a1
!
! 4. f=2*f   x1->a1
!
! 5. b=f     x1->a1
!
! 6. b=3     x1->a1
!
! 7. a=4    
!
! 8. b=5            a7->y13    
!
! 9. f=c            a7->y13
!
!10. f=2*f          a7->y13
!
!11. c=f            a7->y13
!
!12. b=6            a7->y13
!
!13. y=a            a7->y13      [no active path]
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
