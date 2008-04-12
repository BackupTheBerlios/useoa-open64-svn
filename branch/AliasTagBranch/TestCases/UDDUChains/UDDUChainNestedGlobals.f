! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChains
!                   ==========
! Features:
!           1. UDDUChains through global variable.
!              - Global Variables are defined in one procedure 
!                and used inside other procedure.
!
! Testing :
!          [X] UDChain[Stmt]
!          [X] DUChain[Stmt]
!          [X] UDChain[MemRefHandle]
!          [X] DUChain[MemRefHandle]
!
! Status /Issues: 
!
! Note: 1. Imprecise information because of Intraprocedural UDDUChains Analysis.
!       2. All Definitions are May
!       3. If you would like to find UDDUChains for the callsite stmt,
!          e.g. call xyz(), you must ask following 2 questions:
!          3.1. what are the reaching defs to Use-SideEffects of xyz() ?
!          3.2  where exactly MOD-SideEffects of xyz() reach ?
!
!       4. If you would like to find UDDUChains for non-callsites stmt,
!          you must ask following 2 questions:
!          4.1 what are the reaching defs to the Uses of stmt ?
!          4.2 where exactly DEFs of Stmt reach ?
!          
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



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




! Analysis:
! =========

                ! Procedure(bar):
                ! ---------------

! UDChains<Stmt>
! <Stmt>     => set<Stmt>
! ====================================
! StmtHandle(0)       => (barx=bary), (bary=t)
! t=barx              => (StmtHandle(0))
! barx=bary           => (StmtHandle(0))
! bary=t              => (t=barx), StmtHandle(0)


! DUChains<Stmt>
! <Stmt>     => set<Stmt>
!===================================
! StmtHandle(0)       =>  (t=barx), (barx=bary), (bary=t)
! t=barx              =>  (bary=t)
! barx=bary           =>  StmtHandle(0)
! bary=t              =>  StmtHandle(0)


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






                     ! Procedure(foo):
                     ! ---------------

! UDChains<Stmt>
! <Stmt>     => set<Stmt>
! ====================================
! StmtHandle(0)     => (call bar(gx,gy))
! call bar(gx,gy)   => StmtHandle(0)
!

! DUChains<Stmt>
! <Stmt>     => set<Stmt>
!===================================
! StmtHandle(0)       =>  call bar(gx,gy)
! call bar(gx,gy)     =>  StmtHandle(0)


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! No UDChainMemRef
!

! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! No DUChainMemRef
!




                     ! Procedure(head):
                     ! ----------------

! UDChains<Stmt>
! <Stmt>     => set<Stmt>
! ====================================
! StmtHandle(0)       => (gx=x(1)), (y(1)=gx), (y(2)=gy)
! gx=x(1)             => StmtHandle(0)
! y(1)=gx             => (gx=x(1)), StmtHandle(0)
! y(2)=gy             => (StmtHandle(0))
!

! DUChains<Stmt>
! <Stmt>     => set<Stmt>
!===================================
! StmtHandle(0)       =>  (gx=x(1)), y(1)=gx, y(2)=gy
! gx=x(1)             =>  (y(1)=gx), StmtHandle(0)
! y(1)=gx             =>  StmtHandle(0)
! y(2)=gy             =>  StmtHandle(0)




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


