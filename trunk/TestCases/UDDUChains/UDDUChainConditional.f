! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChains
!                   ==========
! Features:
!           1. UDDUChains inside in the presence of conditional
!              - Definition occurs along both the conditional path (for y)
!              - Definition occurs along single conditional path (for x)
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

!Example:
!========  
       program constprop_if1
          integer :: x, y, z

          x = 2                     ! Defs = {x},   Uses = { }
          if ( x .ge. 0 ) then      ! Defs = { },   Uses = {x}
               y = 5                ! Defs = {y},   Uses = { }
               x = y + y            ! Defs = {x},   Uses = {y,y} 
          else                  
               y = 3                ! Defs = {y},   Uses = { }
          endif 
          z = x+y                   ! Defs = {z},   Uses = {x,y}

       end program



!Analysis:
!=========

! UDChains<Stmt>
! <Stmt>     => set<Stmt>
! ====================================
! if<x.ge.0> => <x=2>
! <x=y+y>    => <y=5>
! <z=x+y>    => <y=5>, <x=y+y>, <x=2>, <y=3>



! DUChains<Stmt>
! <Stmt>     => set<Stmt>
!===================================
! <x=2>         => if<x.ge.0>, <z=x+y> 
! <y=5>         => <x=y+y>, <z=x+y>
! <x=y+y>       => <z=x+y>
! <y=3>         => <z=x+y>



! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! <x>   => <x=2>
! <y>   => <y=5>
! <y>   => <y=5>
! <x>   => <x=2>, <x=y+y>
! <y>   => <y=5>, <y=3>



! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! <x>   => if<x.ge.0>, <z=x+y>
! <y>   => <x=y+y>, <z=x+y>
! <x>   => <z=x+y>
! <y>   => <z=y+y>
! <z>   => < >

