! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChainsXAIF
!                   ==============
! Features:
!           1. UDDUChains inside in the presence of conditional
!              - Definition occurs along both the conditional path (for y)
!              - Definition occurs along single conditional path (for x)
!
! Testing :
!          [X] For each distinct UDMemRefChain and DUMemRefChain 
!              create unique ChainID.
!          [X] Map Use and Def MemRefto the correct ChainID.
!              
!
! Status /Issues:
!
! Note:    [X] Only strictlyLocal NamedRef are Must, everything else is May
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Example:
! ========
  

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




! Analysis:
! =========

                   ! UDDUChainsXAIF : 
           ! [Please also refer UD-DUChainsMemRefs below]
           !===============================================

! ChainID     => UD-DUChains
! --------------------------
! [0]         => < >
! [1]         => < >
! [2]         => <StmtHandle(0)>
! [3]         => <x=2>
! [4]         => <y=5>
! [5]         => <x=2>, <x=y+y>
! [6]         => <y=5>, <y=3>
! [7]         => if(x.ge.0), <z=x+y>
! [8]         => <x=y+y>, <z=x+y>
! [9]         => <z=x+y>



! MemRefHandle => ChainID
! -----------------------
! <x>          => 7
! <x>          => 3     
! <y>          => 8     
! <x>          => 9     
! <y>          => 4    
! <y>          => 4     
! <y>          => 9
! <z>          => 1
! <x>          => 5
! <y>          => 6


                    ! UDDUChainsMemRefs :
                    !====================


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! <x>   => <StmtHandle(0)>, <x=2>
! <y>   => <StmtHandle(0)>, <y=5>
! <y>   => <StmtHandle(0)>, <y=5>
! <x>   => Stmtandle(0), <x=2>, <x=y+y>
! <y>   => <StmtHandle(0)>, <y=5>, <y=3>



! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! <x>   => if<x.ge.0>, <z=x+y>
! <y>   => <x=y+y>, <z=x+y>
! <x>   => <z=x+y>
! <y>   => <z=y+y>
! <z>   => < >


