! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChainsXAIF
!                   ==============
! Features:
!           1. UDDUChains through global variable.
!              - Global Variables are defined in one procedure 
!                and used inside other procedure.
!
! Testing :
!          [X] For each distinct UDMemRefChain and DUMemRefChain
!              create unique ChainID.
!          [X] Map Use and Def MemRefto the correct ChainID.
!
! Status /Issues:
!
! Note:
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



       module globals
         double precision gx
         double precision gy
       end module

       SUBROUTINE bar(barX,barY)
         double precision barx
         double precision bary
         double precision t
         t=barx                     ! Defs = {t},     Uses = {barx}
         barx=bary                  ! Defs = {barx},  Uses = {bary}
         bary=t                     ! Defs = {bary},  Uses = {t}
       end subroutine

       SUBROUTINE foo( )
         use globals
         call bar(gx,gy)            ! Defs = { },     Uses = { }
       end subroutine

       subroutine head(x,y)
         use globals
         double precision, dimension(2) :: x
         double precision, dimension(1) :: y

         gx=x(1)                     ! Defs = {gx},   Uses = {x(1)}
         y(1)=gx                     ! Defs = {y(1)}, Uses = {gx}
         y(2)=gy                     ! Defs = {y(2)}, Uses = {gy}
       end subroutine



                  ! UDDUChainsXAIF :
           ! [Please also refer UD-DUChainsMemRefs below]
           !===============================================


         ! Procedure(bar)
         ! --------------

! ChainID     => UD-DUChains
! --------------------------
! [0]     => ( )
! [1]     => ( )
! [2]     => (StmtHandle(0))
! [3]     => (t=barx), StmtHandle(0)
! [4]     => (bary=t)


! MemRefHandle => ChainID
! -----------------------
! (barx)     =>     2  
! (bary)     =>     2  
! (t)        =>     3  
! (t)        =>     4  
! (barx)     =>     2  
! (bary)     =>     2  



               ! Procedure(foo)
               ! --------------
! None




               ! Procedure(head)
               !

! ChainID     => UD-DUChains
! --------------------------
! [0]     =>     ( )
! [1]     =>     ( )
! [2]     =>     (StmtHandle(0))
! [5]     =>     (gx=x(1)), StmtHandle(0)
! [6]     =>     (y(1)=gy), StmtHandle(0)



! MemRefHandle => ChainID
! -----------------------
! x(1)     => 2
! gx       => 5
! gy       => 2
! gx       => 6
! (y(1))   => 2
! (y(2))   => 2
!

! ========================================================

                  ! UDDUChains
                  ! =============


                  ! Procedure(bar)
                  ! --------------

! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (barx)            => StmtHandle(0)
! (bary)            => StmtHandle(0)
! (t)               => (t=barx), StmtHandle(0)


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (t)        => (bary=t)
! (barx)     => StmtHandle(0)
! (bary)     => StmtHandle(0)
!


                   ! Procedure(head)
                   ! ---------------


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! x(1)     => StmtHandle(0)
! gx       => (gx=x(1)), StmtHandle(0)
! gy       => StmtHandle(0)


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! gx       => (y(1)=gy), StmtHandle(0)
! (y(1))   => StmtHandle(0)
! (y(2))   => StmtHandle(0)
!

