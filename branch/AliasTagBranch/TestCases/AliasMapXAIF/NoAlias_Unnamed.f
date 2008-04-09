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


          ! ========= AliasTagFIAlias Results =========
          !
          !   [ MemRefHandle  =>  AliasTag]
          !   =============================
          ! MemRefHandle(A+B) => (5, May)
          ! MemRefHandle(A)   => (2, May)
          ! MemRefHandle(B)   => (3, May)
          ! MemRefHandle(C)   => (5, May)



          ! ======== AliasMapXAIF Results ================
          !
          !   [  MemRefHandle => SetId ]
          !   ===========================
          !
          ! MemRefHandle(A+B) => SetId(1)
          ! MemRefHandle(A)   => SetId(2)
          ! MemRefHandle(B)   => SetId(3)
          ! MemRefHandle(C)   => SetId(4)

          !
          !   [  SetId  =>  Virtual Address ]
          ! ==================================
          ! SetId(1) => { LocTuple(5:5, May) }
          ! SetId(2) => { LocTuple(2:2, May) }
          ! SetId(3) => { LocTuple(3:3, May) }
          ! SetId(4) => { LocTuple(5:5, May)}

         
