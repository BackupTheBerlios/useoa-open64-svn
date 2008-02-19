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

!           Formal  =>  Actual
!           Module_merge_sort(*array(:)) => MemRefNode(Module_merge_sort::a(:))


  CALL Module_merge_sort(array, .FALSE.)

!           Formal  =>  Actual
!           Module_merge_sort(*array(:)) => MemRefNode(Merge_sort::a(:))
!           Module_merge_sort(*ascend)   => MemRefNode(UnnamedRef(.FALSE.))


END PROGRAM Merge_sort

