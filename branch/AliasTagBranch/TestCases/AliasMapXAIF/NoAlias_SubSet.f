! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!                   AliasMapXAIF
!                   ============
! Features:
!           1. Complete Array Reference
!           2. Array Section Reference
!
! Testing :
!          [X] Virtual LocTuple range.
!          [X] Partial Flags
!
! Status : OpenAnalysis still model complete Arrays refereces as SubSetRefs.
!          Therefore, AliasMaPXAIF does not correctly model virtual addresses 
!          for the arrays as complete refereces.
!          It needs special handling of complete array references.
!
! Priority: ??
!
! !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


! Example :
! ==========


        subroutine foo
            double precision, dimension(5) :: local_array1
            double precision, dimension(3) :: local_array2
            local_array2= local_array1(1:3)
        end subroutine


! Analysis :
! ==========

        ! ========= AliasTagFIAlias Results =========
        !
        !   [  MemRefExpr => AliasTags ]
        !   ============================
        ! NamedRef("local_array1")              => ((1,2), Must)
        ! SubSetRef(NamedRef("local_array1"))   => (2, May)
        ! NamedRef("local_array2")              => ((3,4), Must)
        ! SubSetRef(NamedRef("local_Array2"))   => (4, May)
        !
        !
        !   [ MemRefHandle => AliasTags ]
        !   =============================
        ! MemRefHandle("local_array1[]")    => (2, May)
        ! MemRefHandle("local_array2[]")    => (4, May)






        ! ======== AliasMapXAIF Results ==============
        !
        !   [  MemRefHandle => SetId ]
        !   ===========================
        ! MemRefHandle("local_array1[]")  => SetId(1)
        ! MemRefHandle("global_array2[]") => SetId(2)
        !
        !
        !   [  SetId  =>  Virtual Address ]
        ! ==================================
        ! SetId(1) => { LocTuple(2:2, Must }
        ! SetId(2) => { LocTuple(4:4), Must }


