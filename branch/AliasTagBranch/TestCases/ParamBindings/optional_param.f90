


! Example:
! ========

       MODULE Sort

       CONTAINS

          RECURSIVE SUBROUTINE Module_merge_sort (a, ascend)

            IMPLICIT NONE

            INTEGER, DIMENSION(:), INTENT(INOUT) :: a
            LOGICAL, INTENT(IN), OPTIONAL :: ascend
            LOGICAL :: up

            IF (PRESENT(ascend)) THEN
                up = ascend
            ELSE
                up = .TRUE.
            ENDIF

          END SUBROUTINE Module_merge_sort

       END MODULE Sort



       PROGRAM Merge_sort

          USE Sort
          INTEGER, DIMENSION(:), ALLOCATABLE :: array
          INTEGER n

          ALLOCATE( array(n) )

          CALL Module_merge_sort(array)

          CALL Module_merge_sort(array, .FALSE.)

       END PROGRAM Merge_sort






! ParamBindings (Formal => Actual) : 
! ================================== 
! 
! - CallHandle = CALL Module_merge_sort(array)
! 
!     1.  array(:) => MemRefNode(&a(:))
! 
!
!
! - CallHandle =  CALL Module_merge_sort(array, .FALSE.)
!
!     1.  array(:) => MemRefNode(&a(:))
!     2.  ascend   => MemRefNode(&UnnamedRef(.FALSE.))
!


