! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ReachDefs
!                   ==========
! Features:
!           1. ReachDefs Interprocedural with multiple callsites for a function
!              - Actual parameters are modelled as pass by reference 
!                on fortran side.
!
! Testing :
!          [X] ReachDefs[Stmt]
!
! Status /Issues: 
!
!
! Note: 1. Intraprocedural ReachDefs Analysis.
!       2. Except strictlyLocal, all other Definitions are May
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Example:
! --------

       subroutine head(x, f)
       integer :: x
       integer :: f
       integer m, p, v


       m = 2                 
       p = 5                 
       call bar(m,p,v,x)     ! StmtHandle(0)
       m = 5               
       p = 2                
       call bar(p,m,v,x)     ! StmtHandle(0)
       f = m + p + v + x     ! StmtHandle(0), (m=5), (p=2),
                             ! call bar(m,p,v,x), 
                             ! call bar(p,m,v,x)

       return                ! StmtHandle(0), (m=5), (p=2),
                             ! call bar(m,p,v,x),
                             ! call bar(p,m,v,x)

       end subroutine


       subroutine bar(a,b,c,d)
       integer a,b,c,d

       c = a * b              ! StmtHandle(0) 
       b = c - d              ! StmtHandle(0), (c=a*b)
       return                 ! StmtHandle(0), (c=a*b), (b=c-d)
       end subroutine






