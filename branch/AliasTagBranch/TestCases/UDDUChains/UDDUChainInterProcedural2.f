! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChains
!                   ==========
! Features:
!           1. UDDUChains Interprocedural with multiple callsites for a function
!              - Actual parameters are modelled as pass by reference 
!                on fortran side.
!
! Testing :
!          [X] UDChain[Stmt]
!          [X] DUChain[Stmt]
!          [X] UDChain[MemRefHandle]
!          [X] DUChain[MemRefHandle]
!
! Status /Issues: 
!
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
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Example:
! --------

       subroutine head(x, f)
       integer :: x
       integer :: f
       integer m, p, v


       m = 2                    ! Defs = {m},    Uses = { }
       p = 5                    ! Defs = {p},    Uses = { }
       call bar(m,p,v,x)        ! MODs = {m,p},  Uses = {x}
       m = 5                    ! Defs = {m},    Uses = { }
       p = 2                    ! Defs = {p},    Uses = { }
       call bar(p,m,v,x)        ! MODs = {m,p},  Uses = {x}
       f = m + p + v + x        ! Defs = {f},    Uses = {m,p,v,x}

       end subroutine


       subroutine bar(a,b,c,d)
       integer a,b,c,d

       c = a * b                ! Defs = {c},     Uses = {a,b}      
       b = c - d                ! Defs = {b},     Uses = (c,d)
       return
       end subroutine






!Analysis:
!=========

              ! Procedure(head):
              ! ================

! UDChains<Stmt>
! <Stmt>     => set<Stmt>
! ======================================================
! call bar(m,p,v,x)      => StmtHandle(0), (m=2), (p=5)
!
! call bar(p,m,v,x)      => Stmtandle(0), (m=5), (p=2),
!                           (m=2), (p=5), (call bar(m,p,v,x))
!
! f = m + p + v + x      => StmtHandle(0), (m=5), (p=2),
!                           (m=2), (p=5), (call bar(m,p,v,x))
!                           (call bar(p,m,v,x))
!
! StmtHandle(0)          => (f=m+p+v+x), (call bar(m,p,v,x))
!                           (call bar(p,m,v,x))


! DUChains<Stmt>
! <Stmt>     => set<Stmt>
!===================================
! StmtHandle(0)        => (call bar(m,p,v,x)), (call bar(p,m,v,x))
!                         (f=m+p+v+x)
!
! m = 2                => (call bar(m,p,v,x)), (call bar(p,m,v,x))
!                         (f=m+p+v+x)
!
! p = 5                => (call bar(m,p,v,x)), (call bar(p,m,v,x))
!                         (f=m+p+v+x)
!
! call bar(m,p,v,x)    => (call bar(p,m,v,x)), (f=m+p+v+x),
!                         StmtHandle(0)
!
! m = 5                => (call bar(p,m,v,x)), (f=m+p+v+x)
!
! p = 2                => (call bar(p,m,v,x)), (f=m+p+v+x)
!
! call bar(p,m,v,x)    => (f=m+p+v+x), StmtHandle(0)
!
! f = m + p + v + x    => (StmtHandle(0))



! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (x)     =>    StmtHandle(0)
!
! (x)     =>    StmtHandle(0)
!
! (m)     =>    (m=5), (m=2), (call bar(m,p,v,x)),
!               (m=2), (p=5), (call bar(p,m,v,x)),
!               StmtHandle(0)
!
! (p)     =>    (m=5), (p=2), (call bar(m,p,v,x)),
!               (m=2), (p=5), (call bar(p,m,v,x)), 
!               StmtHandle(0)
!
! (v)     =>    (StmtHandle(0)), (call bar(m,p,v,x)),
!               (call bar(p,m,v,x))
!
! (x)     =>    (StmtHandle(0))


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (m)     =>    (call bar(m,p,v,x)), (call bar(p,m,v,x))
!               (f=m+p+v+x)
!
! (p)     =>    (call bar(m,p,v,x)), (call bar(p,m,v,x))
!               (f=m+p+v+x)
!
! (m)     =>    (call bar(p,m,v,x)), (f=m+p+v+x)
!
! (p)     =>    (call bar(p,m,v,x)), (f=m+p+v+x)
!
! (f)     =>    (StmtHandle(0))






              ! Procedure(bar):
              ! ================

! UDChains<Stmt>
! <Stmt>     => set<Stmt>
! ================================================
! StmtHandle(0)  => (c=a*b), (b=c-d)
! (c=a*b)        => StmtHandle(0)
! (b=c-d)        => StmtHandle(0), (c=a*b)


! DUChains<Stmt>
! <Stmt>     => set<Stmt>
!===========================================
! StmtHandle(0)  => (c=a*b), (b=c-d)
! (c=a*b)        => (b=c-d), (StmtHandle(0))
! (b=c-d)        => (StmtHandle(0))


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (a)     =>  (StmtHandle(0))
! (b)     =>  (StmtHandle(0))
! (c)     =>  (c=a*b), StmtHandle(0)
! (d)     =>  (StmtHandle(0))


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (c)     =>  (b=c-d), (StmtHandle(0))
! (b)     =>  (StmtHandle(0))



