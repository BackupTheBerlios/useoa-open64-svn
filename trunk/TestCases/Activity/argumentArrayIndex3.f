! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ICFGActivity
!                   ============
! Features:      Active Path through Parameters at procedure call.
!                (testing flow-sensitivity and context-insensitivity)
!
! Testing :      [X] ActiveStmts => All Statements are active.
!
!                    [XX] "call foo(p(k),q(l))" is false active due
!                          to multiple calls to procedure foo().
!
!                [X] ActiveMemRefExprs => *a, *b, *x(), p(),*y,q()
!
!                    [XX] p(), q() are false active, because of 
!                         multiple calls to foo().
!
!                [X] ActiveSyms => a,b,x,p,y,q
! 
!
! Status/Issue : [X] I dont know why NamedRef("a") shows 
!                    up in the output of ICFGActivity analysis.
!                  
!                [X] Should Call Statements be active ?
!
! Note:    All Definitions are MayDefs
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!                  * Example:
!                  ==========

      subroutine foo(a,b)
       double precision a
       double precision b
       b=a*2    
      end subroutine

c$openad XXX Template ad_template.f
      subroutine head(x,y)
       double precision, dimension(2) :: x
       double precision y
       double precision, dimension(2) :: p,q
       integer k,l
c$openad INDEPENDENT(x)
       call foo(x(k),y)    
       call foo(p(k),q(l)) 
c$openad DEPENDENT(y)
      end subroutine


! ==================================================================

!              * Intutive Analysis
!              ====================
!
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
!  please consider that a1, b3 indicate symbols and program point at which
!  they are defined. This helps us to keep track of recent definition
!  along various paths under examination.
!
! PP Stmt      Path
! 1. a=*x()    x(),0->a1
! 2. b=*y      x(),0->a1
! 3. b=a*2     x(),0->a1->b3
! 4. *x()=a    x(),0->a1->b3,     x(),0->a1->x(),4   [Active Path]
! 5. *y=b      x(),0->a1->b3->y5, x(),0->a1->x(),4   [Active Path]
! 6. a=p()     
! 7. b=q()
! 8. b=a*2
! 9. p()=a
!10. q()=b


! ========================================================================

!                * ICFGActivity Analysis
!                =======================
! 
!
!     subroutine foo(a,b) 
!                          [u: *a,*b] [v: *a,*b]  [iA: *a,*b]
!
!       b=a*2              [Stmt: Active]
!
!                          [u: *a,*b] [v: *a,*b]  [iA: *a,*b]
!     end subroutine
!
!     subroutine head(x,y) 
!
!                             [u: y,*y,q,q(),p,p(),*x()]  
!                             [v: *x,*x(),p,p()]  
!                             [iA: *x(),p,p()]
!
!       call foo(x(k),y)      [Stmt: Active]
!
!                             [u: y,*y,q,q(),p,p(),*x()] 
!                             [v: *x,*x(),p,p(),*y,q,q()] 
!                             [iA: *x(),p,p(),*y,q,q()]
!
!       call foo(p(k),q(l))   [Stmt: Active]
!
!                             [u: y,*y,q,q()]
!                             [v: *x,*x(),p,p(),*y,q,q()] 
!                             [iA: *y,q()]
!
!      end subroutine
!
! ===========================================================


