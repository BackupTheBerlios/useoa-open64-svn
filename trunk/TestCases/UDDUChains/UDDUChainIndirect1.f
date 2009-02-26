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

! FIXME:
! PLM, May 4th 2008
! Pointer Definition UDDUChains missing
! We need to use getUseMREs and getDefMREs for precise Use and Defs
! in the ManagerUDDUChains, but then the question is how to retrive
! MemRefHandles from Memory Reference Expressions.
! Need to Talk to Michelle.


! FIXME:
! PLM, May 4th 2008
! talk to Michelle why p=5 Reaches n=a and therefore not killed
! by a=3 and thus forms UDDUChains.


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

            p=5                    ! Defs = {*p},     Uses = {p}   

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
! (m=t)      =>     (StmtHandle(0)), (p=5)
! (n=a)      =>     (p=5), (a=3)
! StmtHandle(0)=>   (p=5)


! DUChains<Stmt>
! <Stmt>     => set<Stmt>
!===================================
! StmtHandle(0) =>  (i<5), (m=t)
! (p=>t)        =>  (p=5), (m=t), (a=3), (n=a)
! (p=>a)        =>  (p=5), (m=t), (a=3), (n=a) 
! (p=5)         =>  (m=t), (n=a), StmtHandle(0)
! (a=3)         =>  (n=a)


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (I)     => (StmtHandle(0))
! (t)     => (StmtHandle(0)), (p=5)
! (a)     => (p=5), (a=3)


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


