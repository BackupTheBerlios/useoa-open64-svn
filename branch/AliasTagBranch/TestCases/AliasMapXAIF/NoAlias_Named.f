! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. Local Scalar
!           2. Global Scalar
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status : No Issues
!
! Note: To Date April 8th 2008, all memory references are "May".
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!



! Example :
! ==========


        module myModule
             double precision global_var
        end module

        subroutine foo
             use myModule
             double precision :: local_var

             local_var = global_var  
        end subroutine



! Analysis :
! ==========

        ! ========= AliasTagFIAlias Results =========
        !
        !   [ MemRefHandle => AliasTags ]           
        !   =============================
        ! MemRefHandle("local_var")    => (2, May)
        ! MemRefHandle("global_var")   => (1, May)





        ! ======== AliasMapXAIF Results ==============
        !
        !   [  MemRefHandle => SetId ]
        !   ===========================
        ! MemRefHandle("local_var")  => SetId(1)
        ! MemRefHandle("global_var") => SetId(2)
        !
        !   [  SetId  =>  Virtual Address ]
        ! ==================================
        ! SetId(1) => { LocTuple(2:2, May }
        ! SetId(2) => { LocTuple(2:2, May }

