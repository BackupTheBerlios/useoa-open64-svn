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
! Note:  I think ChainID 4 for the DUChain is just spurious because
!        we have StmtHandle(0) everywhere in the UDChain.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


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
! [3]      => (StmtHandle(0))
! [4]      => (firstPtr%labor = 5.3), (x=firstPtr%labor), StmtHandle(0)
! [5]      => (x=firstPtr%labor), StmtHandle(0)
! [6]      => (x=firstPtr%labor)     




! MemRefHandle => ChainID
! ------------------------------
! (firstPtr)           =>     4
! (firstPtr%labor)     =>     5
! (x)                  =>     2
! (firstPtr%labor)     =>     6






                    ! UDDUChainsMemRefs :
                    !====================


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (firstPtr)          => (firstPtr%labor = 5.3), (x=firstPtr%labor), StmtHandle(0)
! (firstPtr%labor)    => (x=firstPtr%labor), StmtHandle(0)
! (x)                 => (StmtHandle(0))


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (firstPtr%labor)     =>     (x=firstPtr%labor)


