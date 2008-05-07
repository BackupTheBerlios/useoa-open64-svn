! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChainsXAIF
!                   ==============
! Features:
!           1. UDDUChains through structure pointer dereference.
!
!
! Testing :
!          [X] For each distinct UDMemRefChain and DUMemRefChain
!              create unique ChainID.
!          [X] Map Use and Def MemRefto the correct ChainID.
!
! Status /Issues:
!
! Note:    [X] Only strictlyLocal NamedRef are Must, everything else is May
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! FIXME:
! PLM May 5th 2008
! Pointer UDDUChains missing i.e. firstPtr=>first have no uses
! i.e. first%labor. Do we need that information ? How do we 
! represent that information for UDDUChainsXAIF ? (Need discussion with
! Jean)


! Example:
! ========


       module myModule
          type repair_bill
               real parts(20)
               real labor
               real pointer insurance
          end type repair_bill
       end module


       subroutine foo()
         use myModule

         type(repair_bill), pointer :: firstPtr
         type(repair_bill), target :: first
         real x

         firstPtr=>first        ! Defs = {firstPtr}      , Uses = { }
         firstPtr%labor = 5.3   ! Defs = {firstPtr%labor}, Uses = { }
         x = firstPtr%labor     ! Defs = {x}             , Uses = {firstPtr%labor}

       end subroutine





                   ! UDDUChainsXAIF :
           ! [Please also refer UD-DUChainsMemRefs below]
           !===============================================

! ChainID  => UD-DUChains
! --------------------------
! [0]      => ( )
! [1]      => ( )
! [2]      => (StmtHandle(0))
! [3]      => (firstPtr%labor = 5.3), StmtHandle(0)
! [4]      => (x=firstPtr%labor), StmtHandle(0)




! MemRefHandle => ChainID
! ------------------------------
! (firstPtr%labor)     => 4
! (firstPtr%labor)     => 3
! (x)                  => 1
! (firstPtr)           => 1 






                    ! UDDUChainsMemRefs :
                    !====================


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (firstPtr%labor)    => (firstPtr%labor = 5.3), StmtHandle(0)


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (firstPtr%labor)     => (x=firstPtr%labor), StmtHandle(0)
! (x)                  => [ ]
! firstPtr             => [ ]

