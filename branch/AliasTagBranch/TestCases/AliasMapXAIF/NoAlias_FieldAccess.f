! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. Field Access
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status : No Issues
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

! Example :
! =========

         module myModule

            type repair_bill   
              real parts(20)   
              real labor   
            end type repair_bill   

         end module
  
         subroutine foo
            use myModule
 
            type(repair_bill) red_ferrari   
            real x

            x = red_ferrari%labor

         end subroutine


! Analysis :
! =========

         ! ========= AliasTagFIAlias Results =========
         !
         !   [ MemRefExpr => AliasTags ]
         !   ===========================
         ! NamedRef("red_ferrari")                      => ((1,2), May)
         ! FieldAccess(NamedRef(red_ferrari), labor")   => (2, May)
         ! NamedRef("x")                                => (3, May)
         !
         !   [ MemRefHandle => AliasTags ]
         !   =============================
         ! MemRefHandle("foo::red_ferrari%labor") => (2, May)
         ! MemRefHandle("foo::x")                 => (3, May)
  



                     

          ! ======== AliasMapXAIF Results ============== 
          ! 
          !   [  MemRefHandle => SetId ]
          !   ===========================
          ! MemRefHandle("foo::red_ferrari%labor") => SetId(1)
          ! MemRefHandle("foo::x")                 => SetId(2)
          !
          !   [  SetId  =>  Virtual Address ]
          ! ================================== 
          ! SetId(1) => { LocTuple(2:2, May) }
          ! SetId(2) => { LocTuple(3:3, May) }



