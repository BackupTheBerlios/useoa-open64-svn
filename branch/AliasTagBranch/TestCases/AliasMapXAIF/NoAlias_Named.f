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
        !   [  MemRefExpr => AliasTags ]
        !   ============================
        ! NamedRef("local_var")        => (1, Must)   
        ! NamedRef("global_var")       => (2, Must)
        ! 
        !   [ MemRefHandle => AliasTags ]           
        !   =============================
        ! MemRefHandle("local_var")    => (1, Must)
        ! MemRefHandle("global_var")   => (2, Must)





        ! ======== AliasMapXAIF Results ==============
        !
        !   [  MemRefHandle => SetId ]
        !   ===========================
        ! MemRefHandle("local_var")  => SetId(1)
        ! MemRefHandle("global_var") => SetId(2)
        !
        !   [  SetId  =>  Virtual Address ]
        ! ==================================
        ! SetId(1) => { LocTuple(1:1, Must }
        ! SetId(2) => { LocTuple(2:2), Must }

