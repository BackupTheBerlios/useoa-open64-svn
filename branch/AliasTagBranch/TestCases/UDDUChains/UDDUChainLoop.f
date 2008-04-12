! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChains
!                   ==========
! Features:
!           1. UDDUChains inside Loop and Conditionals
!              Definition occurs along both the conditional path (for y)
!              Definition occurs along single conditional path (for x)
!
! Testing :
!          [X] UDChain[Stmt]
!          [X] DUChain[Stmt]
!          [X] UDChain[MemRefHandle]
!          [X] DUChain[MemRefHandle]
!
! Status /Issues: 
!
! Note:  1. Intraprocedural UDDUChains as of today April 9th 2008.
!        2. StmtHandle(0) indicate may-UDChains. At this point all
!           AliasSets are May and therefore, it is abvious that we
!           get StmtHandle(0) everywhere in the UDChains.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Example:
! --------

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





!Analysis:
!=========

! UDChains<Stmt>
! <Stmt>     => set<Stmt>
! ====================================
! (i<=10)    =>     (StmtHandle(0)), (i=1), (i=i+1)
!
! (i=i+1)    =>     (StmtHandle(0)), (i=1), (i=i+1)
!
! if(x.ge.0) =>     (StmtHandle(0)), (x=2), (x=y+y)
!
! (x=y+y)    =>     (StmtHandle(0)), (y=5), (y=3), (y=2)
!
! (z=x+y)    =>     (StmtHandle(0)), (x=2), (x=y+y),
!                   (y=5), (y=3), (y=2)
!



! DUChains<Stmt>
! <Stmt>     => set<Stmt>
!===================================
! StmtHandle(0)  => (i<=10), (i=i+1), if(x.ge.0), 
!                   (x=y+y), (z=x+y)
!
! (y=2)          => (x=y+y), (z=x+y)
!
! (i=1)          => (i<=10), (i=i+1)
!
! (i=i+1)        => (i<=10), (i=i+1)
!                
! (x=2)          => if(x.ge.0), (z=x+y)
!
! (y=5)          => (x=y+y), (z=x+y)
!
! (x=y+y)        => if(x.ge.0), (z=x+y)
!
! <y=3>          => (x=y+y), (z=x+y)
!


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (i)     =>     (StmtHandle(0)), (i=1), (i=i+1)
! (i)     =>     (StmtHandle(0)), (i=1), (i=i+1)
! (x)     =>     (StmtHandle(0)), (x=2), (x=y+y)
! (y)     =>     (StmtHandle(0)), (y=5), (y=3), (y=2)
! (y)     =>     (StmtHandle(0)), (y=5), (y=3), (y=2)
! (x)     =>     (StmtHandle(0)), (x=2), (x=y+y)
! (y)     =>     (StmtHandle(0)), (y=5), (y=3), (y=2)
!


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (y)     =>     (x=y+y),    (z=x+y)
! (i)     =>     (i<=10),    (i=i+1)
! (i)     =>     (i<=10),    (i=i+1)
! (x)     =>     if(x.ge.0), (z=x+y)
! (y)     =>     (x=y+y),    (z=x+y)
! (x)     =>     if(x.ge.0), (z=x+y)
! (y)     =>     (x=y+y),    (z=x+y)
! (z)     =>     ( )

