! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChainsXAIF
!                   ==============
! Features:
!           1. UDDUChains inside Loop and Conditionals
!              Definition occurs along both the conditional path (for y)
!              Definition occurs along single conditional path (for x)
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

       program constprop_loop
       integer :: x, y, z
       integer :: i

       y = 2                           ! Defs = {y},     Uses = { }
       do i = 1, 10                    ! Defs = {i, i},  Uses = {i,i}
          x = 2                        ! Defs = {x},     Uses = { }
          if ( x .ge. 0 ) then         ! Defs = { },     Uses = {x}
               y = 5                   ! Defs = {y},     Uses = { }
               x = y + y               ! Defs = {x},     Uses = {y,y}
          else
               y = 3                   ! Defs = {y},     Uses = { }
          endif
          z = x+y                      ! Defs = {z},     Uses = {x,y}
       end do

       end program





                   ! UDDUChainsXAIF :
           ! [Please also refer UD-DUChainsMemRefs below]
           !===============================================

! ChainID     => UD-DUChains
! --------------------------

! [0]     =>     ( )
! [1]     =>     ( )
! [2]     =>     (StmtHandle(0))
! [3]     =>     (i=1), (i=i+1)
! [4]     =>     (x=2)
! [5]     =>     (y=5)
! [6]     =>     (x=2), (x=y+y)
! [7]     =>     (y=5), (y=3)
! [8]     =>     (i<=10),  (i=i+1)
! [9]     =>     (if(x.ge.0)), (z=x+y)
! [10]    =>     (x=y+y),    (z=x+y)
! [11]    =>     (z=x+y)




! MemRefHandle => ChainID
! -----------------------

! (y)     =>     1
! (i)     =>     8
! (i)     =>     3     
! (i)     =>     8
! (i)     =>     3
! (x)     =>     9
! (x)     =>     4
! (y)     =>     10
! (x)     =>     11
! (y)     =>     5 
! (y)     =>     5
! (y)     =>     11
! (z)     =>     1
! (x)     =>     6
! (y)     =>     7




                 ! UDDUChains:
                 ! ===========


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (i)     =>     (i=1), (i=i+1)
! (i)     =>     (i=1), (i=i+1)
! (x)     =>     (x=2) 
! (y)     =>     (y=5)
! (y)     =>     (y=5)
! (x)     =>     (x=2), (x=y+y)
! (y)     =>     (y=5), (y=3)


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (y)     =>     ( )
! (i)     =>     (i<=10),    (i=i+1)
! (i)     =>     (i<=10),    (i=i+1)
! (x)     =>     if(x.ge.0), (z=x+y)
! (y)     =>     (x=y+y),    (z=x+y)
! (x)     =>     (z=x+y)
! (y)     =>     (z=x+y)
! (z)     =>     ( )


