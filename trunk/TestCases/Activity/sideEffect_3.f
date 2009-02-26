! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ICFGActivity
!                   ============
! Features:      2-level nested call.
!
! Testing :      [X] ActiveStmts => All Statements are active.
!
!                [X] ActiveMemRefExprs => *c,*d,*a,*b,*x(),*y()
!
!                [X] ActiveSyms => a,b,c,d,x,y
!
!
! Status/Issue : [X] Should Call Statements be active ?
!
! Note:    All Definitions are MayDefs
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

                  ! Example:
                  ! ========

       subroutine bar(c,d)
           double precision :: c,d
           d=c*c   
       end subroutine

       subroutine foo(a,b)
          double precision :: a,b
          call bar(a,b)  
       end subroutine

       subroutine head()
          double precision, dimension(1) :: x
          double precision, dimension(1) :: y

c$openad INDEPENDENT(x)

          call foo(x(1),y(1)) 

c$openad DEPENDENT(y)

       end subroutine


! ======================================================================
!               Intutive Analysis
!               =================
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
!  please consider that indicate symbols and program point at which
!  they are defined. This helps us to keep track of recent definition
!  along various paths under examination.
!
! 1. a=x(1)       x1()->a1
! 2. b=y(1)       x1()->a1
! 3. c=a          x1()->a1->c3
! 4. d=b          x1()->a1->c3
! 5. d=c*c        x1()->a1->c3->d5
! 6. a=c          x1()->a1->c3->d5,           x1()->a1->c3->a6
! 7. b=d          x1()->a1->c3->d5->b7,       x1()->a1->c3->a6
! 8. x(1)=a       x1()->a1->c3->d5->b7,       x1()->a1->c3->a6->x8()
! 9. y(1)=b       x1()->a1->c3->d5->b7->y9(), [Active Path]
!                 x1()->a1->c3->a6->x8()

! =====================================================================
!                ICFGActivity Analysis
!                =====================

!      subroutine bar(c,d)
!
                         ! [u: *c,*d]    [v: *c]        [iA: *c]
!
!          d=c*c         [Active Stmt]
!
                         ! [u: *d]       [v: *c,*d]     [iA: *d]
!       end subroutine
!
!
!       subroutine foo(a,b)
!
                         ! [u: *a,*b]    [v: *a]        [iA: *a]
!
!         call bar(a,b)  [Active Stmt]
!
                         ! [u: *b]       [v: *a,*b]     [iA: *b]
!       end subroutine
!
!       subroutine head()
!
                    ! [u: y,y(),x,x()] [v: x,x()]      [iA:x,x()] 
!
!         call foo(x(1),y(1))  [Active Stmt]
!
                    ! [u: y,y()]     [v: x,x(),y,y()]  [iA:y,y()]
!
!       end subroutine
!
! =========================================================================
