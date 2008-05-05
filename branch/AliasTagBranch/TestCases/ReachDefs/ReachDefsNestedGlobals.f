! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ReachDefs
!                   ==========
! Features:
!           1. ReachDefs through global variable.
!              - Global Variables are defined in one procedure 
!                and used inside other procedure.
!
! Testing :
!          [X] ReachDefs[Stmt]
!
! Status /Issues: 
!          [X] Reaching Definition for the first statement in the
!              bar() is missing. i.e. (t=barx) => StmtHandle(0)
!              - Can we ignore this issue for Intraprocedural analysis ?
!
! Note: 1. Intraprocedural ReachDefs Analysis.
!       2. Except strictlyLocals, all other Definitions are May
!          
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


       module globals
         double precision gx
         double precision gy
       end module

       SUBROUTINE bar(barX,barY)
         double precision barx
         double precision bary
         double precision t
         t=barx              
         barx=bary           ! (t=barx)
         bary=t              ! (t=barx), (barx=bary)

! Exit ReachDefs : (t=barx), (barx=bary), (bary=t)

       end subroutine

       SUBROUTINE foo( )
         use globals
         call bar(gx,gy)      

! Exit ReachDefs : (call bar(gx,gy))

       end subroutine

       subroutine head(x,y)
         use globals
         double precision, dimension(2) :: x
         double precision, dimension(1) :: y

         gx=x(1)     
         y(1)=gx             ! (gx=x(1))
         y(2)=gy             ! (gx=x(1)), (y(1)=gx)

! Exit ReachDefs : (gx=x(1)), (y(1)=gx), (y(2)=gy)
                            
       end subroutine




