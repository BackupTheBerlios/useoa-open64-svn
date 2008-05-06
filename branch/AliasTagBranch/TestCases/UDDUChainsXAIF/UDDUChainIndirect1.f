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
! [3]         =>     (p=5), StmtHandle(0)
! [4]         =>     (p=5), (a=3)
! [5]         =>     (m=t), (n=a), StmtHandle(0)
! [6]         =>     (n=a)



! MemRefHandle => ChainID
! -----------------------
! (i)     =>     2
! (p)     =>     1
! (p)     =>     1
! (p)     =>     5
! (m)     =>     1
! (t)     =>     3
! (a)     =>     6
! (n)     =>     1
! (a)     =>     4




                    ! UDDUChainsMemRefs :
                    !====================


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (t)     => (p=5) (StmtHandle(0))
! (a)     => (p=5) (a=3) 

! (p)     => (p=>t), (p=>a), is missing because pointer UDChains not
!                             working


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (p)     => [] pointer DUChains not yet working
! (p)     => [] pointer DUChains not yet working
! (p)    =>  (m=t), (n=a), StmtHandle(0)
! (m)     => (StmtHandle(0))
! (a)     => (n=a)
! (n)     => (StmtHandle(0))
 
