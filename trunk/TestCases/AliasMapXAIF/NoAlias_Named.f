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
! Note: * Only StrictlyLocal memory references are Must, all other 
!         memory references are "May".
!
!       * All the MemRefHandles for which AliasTagsSet is empty
!         are Mapped to AliasMapXAIF setId = 0, [jean's Suggestion, 
!         April 2008].

!
! Author : Priyadarshini Malusare, Argonne National Laboratory, April 8th 2008
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


        ! ======== AliasMapXAIF Results ==============
        !
        !   [  MemRefHandle => SetId ]
        !   ===========================
        ! MemRefHandle("local_var")  => SetId(1)
        ! MemRefHandle("global_var") => SetId(2)
        !
        !   [  SetId  =>  Virtual Address ]
        ! ==================================
        ! SetId(1) => { LocTuple(3:3, Must }
        ! SetId(2) => { LocTuple(1:1, May }

