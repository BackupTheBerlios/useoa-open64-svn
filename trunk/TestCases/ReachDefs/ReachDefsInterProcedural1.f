! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ReachDefs
!                   ==========
! Features:
!           1. ReachDefs Interprocedural single callsite for a function.
!              - Actual Parameters are modelled as pass by reference 
!                on fortran side.
!
! Testing :
!          [X] ReachDefs[Stmt]
!
! Status /Issues: 
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
        p = 5                ! (m=2)
        call bar(m,p,v,x)    ! (m=2) (p=5)
        f = m + p + v + x    ! (m=2), (p=5), (call bar(m,p,v,x))

! Exit ReachDefs : (m=2), (p=5), (call bar(m,p,v,x))

       end subroutine


       subroutine bar(a,b,c,d)
       integer a,b,c,d

       c = a * b             
       b = c - d             ! (c=a*b)

! Exit ReachDefs : (c=a*b), (b=c-d)

       end subroutine


