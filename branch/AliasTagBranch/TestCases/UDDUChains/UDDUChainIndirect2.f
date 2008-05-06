! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   UDDUChains
!                   ==========
! Features:
!           1. UDDUChains through structure pointer dereference.
!
!
! Testing :
!          [X] UDChain[Stmt]
!          [X] DUChain[Stmt]
!          [X] UDChain[MemRefHandle]
!          [X] DUChain[MemRefHandle]
!
! Status /Issues:  No UDChains for Pointer Assignments because ReachDefs
!                  does not provide that information.
!
! Note:  1. Intraprocedural UDDUChains as of today April 9th 2008.
!        2. StmtHandle(0) indicate may-UDChains. At this point all
!           AliasSets are May and therefore, it is abvious that we
!           get StmtHandle(0) everywhere in the UDChains.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! FIXME:
! PLM May 4th 2008
! Missing Structure Pointer UDDUChains.


! Example:
!=========

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
         firstPtr%labor = 5.3   ! Defs = {firstPtr%labor}, Uses ={firstPtr}
         x = firstPtr%labor     ! Defs = {x}             , Uses = {firstPtr%labor, firstPtr},

       end subroutine 




! Please consider that following analysis is intutively correct
! analysis and does not match with the actual output of UDDUChains
! we get as of today April 9th 2008. Please see Status/Issues above

!Analysis: 
!=========

! UDChains<Stmt>
! <Stmt>     => set<Stmt>
! ====================================
! StmtHandle(0)             =>  (firstPtr%labor = 5.3)
! ( x = firstPtr%labor )    =>  (StmtHandle(0)), (firstPtr=>first), 
!                               (firstPtr%labor = 5.3)

! DUChains<Stmt>
! <Stmt>     => set<Stmt>
!===================================
! StmtHandle(0)             => ( x = firstPtr%labor )
! ( firstPtr=>first )       => (firstPtr%labor = 5.3), (x=firstPtr%labor)
! ( firstPtr%labor = 5.3 )  => (x=firstPtr%labor)


! UDChains<MemRefHandle>  [Please see Uses per statement]
! <Use MemRefHandle>  =>  set<Stmt>
!=======================================================
! (firstPtr)          => (firstPtr->first)
! (firstPtr%labor)    => (firstPtr%labor=5.3), (firstPtr=>first),
!                        (StmtHandle(0))


! DUChains<MemRefHandle>  [Please see Defs per statement]
! <Def MemRefHandle>  =>  set<Stmt>
!========================================================
! (firstPtr%labor)     =>     (x=firstPtr%labor), StmtHandle(0)
! (firstPtr=>first)    =>     (firstPtr%labor)=5.3, (x=firstPtr%labor)

