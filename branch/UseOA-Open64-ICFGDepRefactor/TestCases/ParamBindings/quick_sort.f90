module read_data_module

interface read_data

  module procedure read_integer
  module procedure read_real

end interface read_data

contains

SUBROUTINE read_real(File_Name,Raw_Data,How_Many)
IMPLICIT NONE
CHARACTER (LEN=*) , INTENT(IN) :: File_Name
INTEGER , INTENT(IN) :: How_Many
REAL , INTENT(OUT) , &
  DIMENSION(:) :: Raw_Data
INTEGER :: I
  OPEN(FILE=File_Name,UNIT=1)
  DO I=1,How_Many
    READ (UNIT=1,FMT=*) Raw_Data(I)
  ENDDO
END SUBROUTINE read_real

SUBROUTINE read_integer(File_Name,Raw_Data,How_Many)
IMPLICIT NONE
CHARACTER (LEN=*) , INTENT(IN):: File_Name
INTEGER , INTENT(IN) :: How_Many
INTEGER , INTENT(OUT) , &
  DIMENSION(:) :: Raw_Data
INTEGER :: I
  OPEN(FILE=File_Name,UNIT=1)
  DO I=1,How_Many
    READ (UNIT=1,FMT=*) Raw_Data(I)
  ENDDO
END SUBROUTINE read_integer

end module read_data_module

module sort_data_module

interface sort_data

  module procedure sort_integer
  module procedure sort_real

end interface sort_data

contains

SUBROUTINE sort_real(Raw_Data,How_Many)
IMPLICIT NONE
INTEGER , INTENT(IN) :: How_Many
REAL , INTENT(INOUT)  , &
  DIMENSION(:) :: Raw_Data
  CALL QuickSort(1,How_Many)

CONTAINS

RECURSIVE SUBROUTINE QuickSort(L,R)
IMPLICIT NONE
INTEGER , INTENT(IN) :: L,R
INTEGER :: I,J
REAL :: V,T

  i=l
  j=r
  v=raw_data( int((l+r)/2) )
  do
    do while (raw_data(i) < v )
        i=i+1
    enddo
    do while (v < raw_data(j) )
        j=j-1
    enddo
    if (i<=j) then
      t=raw_data(i)
      raw_data(i)=raw_data(j)
      raw_data(j)=t
      i=i+1
      j=j-1
    endif
    if (i>j) exit
  enddo

  if (l<j) then 
    call quicksort(l,j)
  endif

  if (i<r) then
    call quicksort(i,r)
  endif

END SUBROUTINE QuickSort

END SUBROUTINE sort_real

SUBROUTINE sort_integer(Raw_Data,How_Many)
IMPLICIT NONE
INTEGER , INTENT(IN) :: How_Many
INTEGER , INTENT(INOUT)  , &
  DIMENSION(:) :: Raw_Data
  CALL QuickSort(1,How_Many)

CONTAINS

RECURSIVE SUBROUTINE QuickSort(L,R)
IMPLICIT NONE
INTEGER , INTENT(IN) :: L,R
INTEGER :: I,J
INTEGER :: V,T

  i=l
  j=r
  v=raw_data( int((l+r)/2) )
  do
    do while (raw_data(i) < v )
        i=i+1
    enddo
    do while (v < raw_data(j) )
        j=j-1
    enddo
    if (i<=j) then
      t=raw_data(i)
      raw_data(i)=raw_data(j)
      raw_data(j)=t
      i=i+1
      j=j-1
    endif
    if (i>j) exit
  enddo

  if (l<j) then 
    call quicksort(l,j)
  endif

  if (i<r) then
    call quicksort(i,r)
  endif

END SUBROUTINE QuickSort

END SUBROUTINE sort_integer

end module sort_data_module

module print_data_module

interface print_data

  module procedure print_integer
  module procedure print_real

end interface print_data

contains

SUBROUTINE print_real(Raw_Data,How_Many)
IMPLICIT NONE
INTEGER , INTENT(IN) :: How_Many 
REAL , INTENT(IN) , &
  DIMENSION(:) :: Raw_Data
INTEGER :: I
  OPEN(FILE='RSORTED.DAT',UNIT=2)
  DO I=1,How_Many
    WRITE(UNIT=2,FMT=*) Raw_Data(I)
  END DO
  CLOSE(2)
END SUBROUTINE print_real

SUBROUTINE print_integer(Raw_Data,How_Many)
IMPLICIT NONE
INTEGER , INTENT(IN) :: How_Many
INTEGER , INTENT(IN) , &
  DIMENSION(:) :: Raw_Data
INTEGER :: I
  OPEN(FILE='ISORTED.DAT',UNIT=2)
  DO I=1,How_Many
    WRITE(UNIT=2,FMT=*) Raw_Data(I)
  END DO
  CLOSE(2)
END SUBROUTINE print_integer

end module print_data_module

PROGRAM ch2603

use read_data_module
use sort_data_module
use print_data_module

IMPLICIT NONE
INTEGER :: How_Many
CHARACTER (LEN=20) :: File_Name
INTEGER , ALLOCATABLE , DIMENSION(:) :: integer_data
REAL    , ALLOCATABLE , DIMENSION(:) :: real_Data

  PRINT * , ' How many data items are there?'
  READ  * , How_Many
  PRINT * , ' What is the file name?'
  READ '(A)',File_Name 
  ALLOCATE(integer_data(How_Many))
  CALL Read_Data(File_Name,integer_data,How_Many)
  CALL Sort_Data(integer_data,How_Many)
  CALL Print_Data(integer_data,How_Many)
  PRINT *, ' Phase 1 ends.'
  PRINT *, ' data written to file name ISORTED.DAT'
  DEALLOCATE(integer_data)

  PRINT * , ' How many data items are there?'
  READ  * , How_Many
  PRINT * , ' What is the file name?'
  READ '(A)',File_Name 
  ALLOCATE(real_data(How_Many))
  CALL Read_Data(File_Name,real_data,How_Many)
  CALL Sort_Data(real_data,How_Many)
  CALL Print_Data(real_data,How_Many)
  PRINT *, ' Program ends.'
  PRINT *, ' data written to file name RSORTED.DAT'
END PROGRAM ch2603


