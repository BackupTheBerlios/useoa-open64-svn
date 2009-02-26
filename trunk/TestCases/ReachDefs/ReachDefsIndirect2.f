! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   ReachDefs
!                   ==========
! Features:
!           1. ReachDefs through structure pointer dereference.
!
!
! Testing :
!          [X] ReachDefs[Stmt]
!
! Status /Issues:  
!
! Note:  1. Intraprocedural ReachDefs as of today April 9th 2008.
!        2. StmtHandle(0) indicate may-ReachDefs. Only strictlyLocals
!           are must, all others AliasSets are May and therefore, it
!           is abvious that we get StmtHandle(0) for them.
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

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

         firstPtr=>first         
         firstPtr%labor = 5.3   
         x = firstPtr%labor      ! (firstPtr=>first), 
                                 ! (firstPtr%labor=5.3)


         return                  ! (firstPtr=>first),
                                 ! (firstPtr%labor=5.3)
                                 ! (x=firstPtr%labor)

       end subroutine 





