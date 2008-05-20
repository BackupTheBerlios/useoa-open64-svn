!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! Due to context insensativity across calls, no constants get through 
! either call
!
! Due to context insensitive alias analysis, m, p and v get placed 
! into the same Loc Id set, so they mayOverlap each other.
! 
! Then we made the 'fix' to FIAlias where NamedRefs and local UnnamedRefs
! would also be placed within their own MUSTALIAS sets:
!   BEFORE the fix, all ReachConsts sets were BOTTOM
!   After the fix, we see that after m=2, we have the <m,VALUE,2> 
!     within the ReachConsts set that reaches p=5.  However, since
!     m and p MayAlias, only <p,VALUE,5> reaches the call to bar.
!
! Note: Current ReachConsts results are intraprocedural
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
       call bar(m,p,v,4)

       !                 most precise: all BOTTOM, m=2, p=6, v=10, temp=4
       ! context insensativity causes: all BOTTOM, temp=4
       !                 Current     : all BOTTOM
       p = 2

       !                 most precise: all BOTTOM, m=2, p=2, v=10, temp=4
       ! context insensativity causes: all BOTTOM, p=2, temp=4
       !                 Current     : all BOTTOM, p=2
       m = 5

       !                 most precise: all BOTTOM, m=5, p=2, v=10, temp=4
       ! context insensativity causes: all BOTTOM, m=5, p=2, temp=4
       !                 Current     : all BOTTOM, p=2, m=5
       call bar(p,m,v,4)


       !                    most precise: all BOTTOM, m=6, p=2, v=10, temp=4
       ! call-return interference causes: all BOTTOM, temp=4
       !                 Current        : all BOTTOM
       f = m + p + v + x

       !                    most precise: all BOTTOM, m=6, p=2, v=10, temp=4
       !    context insensativity causes: all BOTTOM, temp=4
       !                 Current        : all BOTTOM
       end subroutine


       subroutine bar(a,b,c,d)
       integer a,b,c,d
       
       ! in context: call#1: most precise: all BOTTOM, *a=2, *b=5, *d=4
       ! in context: call#2: most precise: all BOTTOM, *a=2, *b=5, *d=4, *c=10
       ! no context:                       all BOTTOM, *d=4
       c = a * b

       ! in context: call#1: most precise: all BOTTOM, *a=2, *b=5, *d=4, *c=10
       ! in context: call#2: most precise: all BOTTOM, *a=2, *b=5, *d=4, *c=10
       ! no context:                       all BOTTOM, *d=4
       b = c - d

       ! in context: call#1: most precise: all BOTTOM, *a=2, *b=6, *d=4, *c=10
       ! in context: call#2: most precise: all BOTTOM, *a=2, *b=6, *d=4, *c=10
       ! no context:                       all BOTTOM, *d=4
       return

       ! in context: call#1: most precise: all BOTTOM, *a=2, *b=6, *d=4, *c=10
       ! in context: call#2: most precise: all BOTTOM, *a=2, *b=6, *d=4, *c=10
       ! no context:                       all BOTTOM, *d=4

       end subroutine

