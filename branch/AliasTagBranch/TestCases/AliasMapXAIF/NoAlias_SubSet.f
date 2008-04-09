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
! Status / Issues : 
!          OpenAnalysis still model complete Arrays refereces as SubSetRefs.
!          Therefore, AliasMaPXAIF does not correctly model virtual addresses 
!          for the arrays as complete refereces.
!          It needs special handling of complete array references.
!
!          I discussed this issue in the meeting with Michelle on
!          April 8th 2008, Michelle suggests that we would like to do 
!          whole array analysis only if that would make a significant
!          difference for OpenAD.
!
! Note :   To Date April 8th 2008, all memory references are "May".
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
        !   [ MemRefHandle => AliasTags ]
        !   =============================
        ! MemRefHandle("local_array1[]")    => AliasTagResults(3, May)
        ! MemRefHandle("local_array2[]")    => AliasTagResults(4, May)






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
        ! SetId(1) => { LocTuple(3:3, May }
        ! SetId(2) => { LocTuple(4:4, May }


