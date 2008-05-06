! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChainsXAIF
!                   ==============
! Features:
!           1. UDDUChains Interprocedural single callsite for a function.
!              - Actual Parameters are modelled as pass by reference 
!                on fortran side.
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

       subroutine head(x, f)
       integer :: x
       integer :: f
       integer m, p, v


       m = 2                        ! Defs = {m}     , Uses = { }
       p = 5                        ! Defs = {p}     , Uses = { }
       call bar(m,p,v,x)            ! MODs  = {m,p}  , Uses  = {x}
       f = m + p + v + x            ! Defs = {f}     , Uses = {m,p,v,x}

       end subroutine


       subroutine bar(a,b,c,d)
       integer a,b,c,d

       c = a * b                    ! Defs = {c}     , Uses = {a,b}
       b = c - d                    ! Defs = {b}     , Uses = {c,d}
       return

       end subroutine




              ! Procedure(head)
              ! ===============         

              ! UDDUChainsXAIF
              ! ==============

! ChainID     =>     Chains
! [0]         => ( )
! [1]         => ( )
! [2]         => StmtHandle(0)
! [3]         => call bar(m,p,v,x)
! [4]         => (m=2)
! [5]         => (p=5), call bar(m,p,v,x)
! [6]         => call bar(m,p,v,x), (f=m+p+v+x)


! MemRefHandle   => ChainID
! (m)            => 6
! (p)            => 6     
! (&x)           => 2     
! (f)            => 2     
! (x)            => 2     
! (v)            => 3     
! (m)            => 4     
! (p)            => 5     






! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (x)     => StmtHandle(0)
! (x)     => StmtHandle(0)
! (v)     => (call bar(m,p,v,x))
! (m)     => (m=2)
! (p)     => (p=5), call bar(m,p,v,x)


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (m)     =>  call bar(m,p,v,x), (f=m+p+v+x)
! (p)     =>  call bar(m,p,v,x), (f=m+p+v+x)
! (f)     =>  (StmtHandle(0))




                 ! Procedure(bar)
                 ! ==============

! ChainID     =>     Chains
! [0]         => ( )
! [1]         => ( )
! [2]         => (StmtHandle(0))
! [7]         => StmtHandle(0), (c=a*b)
! [8]         => StmtHandle(0), (b=c-d)



! MemRefHandle   => ChainID
! (c)            => 8
! (a)            => 2   
! (b)            => 2   
! (b)            => 2   
! (c)            => 7   
! (d)            => 2   



! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (a)     => StmtHandle(0)
! (b)     => StmtHandle(0)
! (c)     => StmtHandle(0), (c=a*b)
! (d)     => StmtHandle(0)


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (c)     =>  StmtHandle(0), (b=c-d)
! (b)     =>  (StmtHandle(0))

