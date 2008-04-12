! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChains
!                   ==========
! Features:
!           1. UDDUChains through pointer dereference.
!              - The exact location that pointer is pointing at is
!                unknwon statically.
!
! Testing :
!          [X] UDChain[Stmt]
!          [X] DUChain[Stmt]
!          [X] UDChain[MemRefHandle]
!          [X] DUChain[MemRefHandle]
!
! Status /Issues:  1. No UDDUChains for Pointer Assignments because
!                     ReachDefs does not have that information.
!
! Note:  1. Intraprocedural UDDUChains as of today April 9th 2008.
!        2. StmtHandle(0) indicate may-UDChains. At this point all
!           AliasSets are May and therefore, it is abvious that we
!           get StmtHandle(0) everywhere in the UDChains.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Example:
! --------
      
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




! Please consider that following analysis is intutively correct
! analysis and does not match with the actual output of UDDUChains
! we get as of today April 9th 2008. Please see Status/Issues above


!Analysis:
!=========

! UDChains<Stmt>
! <Stmt>     => set<Stmt>
! ====================================
! if(i<5)    =>     (StmtHandle(0))
! (m=t)      =>     (StmtHandle(0)), (*p=5)
! (n=a)      =>     (StmtHandle(0)), (*p=5), (a=3)


! DUChains<Stmt>
! <Stmt>     => set<Stmt>
!===================================
! (p=>t)     =>     (*p=5), (m=t), (a=3), (n=a)
! (p=>a)     =>     (*p=5), (m=t), (a=3), (n=a) 
! (*p=5)     =>     (m=t), (a=3), (n=a)
! (m=t)      =>     (StmtHandle(0))
! (a=3)      =>     (n=a)
! (n=a)      =>     (StmtHandle(0))



! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (t)     => (p=>t)
! (a)     => (a=3)


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (p)     => (*p=5), (m=t), (a=3), (n=a)
! (p)     => (*p=5), (m=t), (a=3), (n=a)
! (*p)    => (m=t), (a=3), (n=a)
! (m)     => (StmtHandle(0))
! (a)     => (n=a)
! (n)     => (StmtHandle(0))


