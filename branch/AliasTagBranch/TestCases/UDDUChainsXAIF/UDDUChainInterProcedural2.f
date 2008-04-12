! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChainsXAIF
!                   ==============
! Features:
!           1. UDDUChains Interprocedural with multiple callsites for a function
!              - Actual parameters are modelled as pass by reference 
!                on fortran side.
!
! Testing :
!          [X] For each distinct UDMemRefChain and DUMemRefChain
!              create unique ChainID.
!          [X] Map Use and Def MemRefto the correct ChainID.
!
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



              ! Procedure(head) 
              ! ===============


! [0]     =>   ( )
! [1]     =>   ( )
! [2]     =>   (StmtHandle(0))
! [3]     =>   (StmtHandle(0)), (call bar(m,p,v,x)),
!              (call bar(p,m,v,x))
! [4]     =>   (m=5), (m=2), (call bar(m,p,v,x)), 
!              (m=2), (p=5), (call bar(p,m,v,x)),
!              StmtHandle(0)
! [5]     =>   (call bar(m,p,v,x)), (call bar(p,m,v,x))
!              (f=m+p+v+x)
! [6]     =>   (call bar(p,m,v,x)), (f=m+p+v+x)




! (x)     =>     2   
! (x)     =>     2   
! (m)     =>     4   
! (p)     =>     4   
! (v)     =>     3    
! (x)     =>     2    
! (m)     =>     5    
! (p)     =>     5    
! (m)     =>     6    
! (p)     =>     6    
! (f)     =>     2    





              ! Procedure(head)
              ! ===============

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






                ! Procedure(bar)
                ! ==============


! [7]     =>  (c=a*b), StmtHandle(0)
! [8]     =>  (b=c-d), (StmtHandle(0))



! (a)     =>  2   
! (b)     =>  2  
! (c)     =>  7   
! (d)     =>  2   
! (c)     =>  8   
! (b)     =>  2   





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







             
