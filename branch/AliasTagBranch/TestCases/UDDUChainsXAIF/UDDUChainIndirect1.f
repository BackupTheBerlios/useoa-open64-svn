! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChainsXAIF
!                   ==============
! Features:
!           1. UDDUChains through pointer dereference.
!              - The exact location that pointer is pointing at is
!                unknwon statically.
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


! Example:
! ========
     
       subroutine foo
            double precision, pointer :: p
            double precision, target  :: t
            double precision, target  :: a
            double precision :: m, n
            integer i

            if( i < 5 ) then
                p=>t               ! Defs = {p},      Uses = { }
            else
                p=>a               ! Defs = {p},      Uses = { }
            end if

            p=5                    ! Defs = {*p},     Uses = { }
            m=t                    ! Defs = {m},      Uses = {t}
            a=3                    ! Defs = {a},      Uses = { }
            n=a                    ! Defs = {n},      Uses = {a}
       end subroutine



                ! UDDUChainsXAIF :
           ! [Please also refer UD-DUChainsMemRefs below]
           !===============================================

! ChainID     => UD-DUChains
! --------------------------
! [0]         =>     ( )
! [1]         =>     ( )
! [2]         =>     (StmtHandle(0))
! [3]         =>     (p=>t), StmtHandle(0)
! [4]         =>     (a=3), StmtHandle(0)
! [5]         =>     (*p=5), (m=t), (a=3), (n=a)
! [6]         =>     (m=t), (a=3), (n=a)
! [7]         =>     (n=a)



! MemRefHandle => ChainID
! -----------------------
! (t)     =>     3
! (a)     =>     4
! (p)     =>     5
! (p)     =>     5
! (*p)    =>     6
! (m)     =>     2
! (a)     =>     7
! (n)     =>     2




                    ! UDDUChainsMemRefs :
                    !====================


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (t)     => (p=>t) (StmtHandle(0))
! (a)     => (a=3)  (StmtHandle(0))


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (p)     => (*p=5), (m=t), (a=3), (n=a)
! (p)     => (*p=5), (m=t), (a=3), (n=a)
! (*p)    => (m=t), (a=3), (n=a)
! (m)     => (StmtHandle(0))
! (a)     => (n=a)
! (n)     => (StmtHandle(0))
 
