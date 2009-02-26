!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! constprop_proc1.f90
!
! most precise results (not here yet)
! p should be constant before the call, and not after the call
! v should be constant after the call, but not before the call
! m should be constant before and after the call
!
! current == FIAliasAliasTags, is FI and CI, and causes the following:
!
!    any non-local (and formal parameters are non-local because they do not
!            have their own storage space) is mayDef and cannot define
!            a reaching constant (although they may hold a reaching const
!            as seen in *b in subroutine bar below)
!
! Note: Even if we use ICFGDFSolver, the analysis is still 
!       Intraprocedural.
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



       subroutine head(x, f)
       integer :: x
       integer :: f
       integer m, p, v


       ! all BOTTOM
       m = 2

       ! all BOTTOM, m=2
       p = 5
       
       ! all BOTTOM, m=2, p=5
       call bar(m,p,v,x)

       ! most precise: all BOTTOM, m=2, v=10
       ! current     : all BOTTOM, m=2
       f = m + p + v + x

       ! most precise: all BOTTOM, m=2, v=10
       ! current     : all BOTTOM, m=2
       end subroutine


       subroutine bar(a,b,c,d)
       integer a,b,c,d

       ! all BOTTOM, *a=2, *b=5
       c = a * b

       ! most precise: all BOTTOM, *a=2, *b=5, *c=10
       ! current     : all BOTTOM, *a=2, *b=5
       b = c - d

       ! most precise: all BOTTOM, *a=2, *c=10
       ! current     : all BOTTOM, *a=2
       return

       ! most precise: all BOTTOM, *a=2, *c=10
       ! current     : all BOTTOM, *a=2
       end subroutine

