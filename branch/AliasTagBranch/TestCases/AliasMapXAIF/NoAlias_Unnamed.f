! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. UnnamedRef for the actual parameter as expression
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status /Issues :  No Issues
! 
! Note: To Date April 8th 2008, all memory references are "May".
!
! Author : Priyadarshini Malusare, Argonne National Laboratory, April 8th 2008
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Example :
! ==========


        subroutine foo
             double precision :: a,b
             call bar(a+b)    
        end subroutine
 
        subroutine bar(c)
             double precision :: c
             c=5
        end subroutine



! Analysis :
! =========


          ! ======== AliasMapXAIF Results ================
          !
          !   [  MemRefHandle => SetId ]
          !   ===========================
          !
          ! MemRefHandle(A+B) => SetId(1)
          ! MemRefHandle(A)   => SetId(2)
          ! MemRefHandle(B)   => SetId(3)

          !
          !   [  SetId  =>  Virtual Address ]
          ! ==================================
          ! SetId(1) => { LocTuple(8:8, May) }
          ! SetId(2) => { LocTuple(3:3, Must) }
          ! SetId(3) => { LocTuple(5:5, Must) }

         
