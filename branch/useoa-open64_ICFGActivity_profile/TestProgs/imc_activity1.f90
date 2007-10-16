!======================================================================
! imc_activity1.f90
! 
! Tests activity analysis on the InterMPICFG. 
! If y in independent and f is dependent then x is not active.
!======================================================================
      program main
!     use mpi
!     Use the following include if the mpi module is not available
      include "mpif.h"
   
      integer n, myid, numprocs, i, ierr, x, y, z, f
c$openad INDEPENDENT(y)
c$openad DEPENDENT(f)

!     MPI initializations
      call MPI_INIT(ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, myid, ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, numprocs, ierr)

      x = 5

!     Have the root processor send a constant to all other processors
      if ( myid .eq. 0 ) then
         x = 0
         do 40 i = 1,numprocs-1
            call MPI_SEND(x, 1, MPI_INTEGER, i, i, MPI_COMM_WORLD, ierr)
 40      continue

!     Workers receive the constant and then use it
      else

        call MPI_RECV(x, 1, MPI_INTEGER, 0, MPI_ANY_TAG, MPI_COMM_WORLD, status, ierr)
        z = x + 2

      endif

      f = y + 2 + z

      call MPI_FINALIZE(ierr)
      stop
      end

!      include "mpiSEstubs.f90"

!
!     MPI_INIT stub for SideEffects
      subroutine MPI_INIT(ierror)
      integer, intent(out) :: ierror

      ierror = 5 ! my choice

      return
      end
!
!
!     MPI_FINALIZE stub for SideEffects
      subroutine MPI_FINALIZE(ierror)
      integer, intent(out) :: ierror

      ierror = 5 ! my choice

      return
      end
!
!     MPI_COMM_SIZE stub for SideEffects
      subroutine MPI_COMM_SIZE(comm, nsize, ierror)
      integer, intent(out) :: nsize, ierror
      integer, intent(in) :: comm

      nsize = comm
      ierror = comm

      return
      end
!
!     MPI_COMM_RANK stub for SideEffects
      subroutine MPI_COMM_RANK(comm, rank, ierror)
      integer, intent(out) :: rank, ierror
      integer, intent(in) :: comm

      rank = comm
      ierror = comm

      return
      end

!     MPI_SEND stub for SideEffects
      subroutine MPI_SEND(buf, count, datatype, dest, tag, comm, ierror)
      include "stub_global.h"
      integer, intent(out) :: ierror
      integer, intent(in) :: comm, tag, dest, datatype, count
      integer, intent (in) :: buf

      ierror = comm + tag + dest + datatype + count + buf + mpi_global_var
      !ierror = tag
      !ierror = dest
      !ierror = datatype
      !ierror = count
      mpi_global_var = mpi_global_var + buf + tag + dest + datatype + count

      return
      end
!


!     MPI_RECV stub for SideEffects
      subroutine MPI_RECV(buf,count,datatype,source,tag,comm,status,ierror)
!     MPI_STATUS_SIZE is 4 in mpif.h
      include "stub_global.h"
      integer, intent(out) :: status, ierror
      integer, intent(in) :: comm, tag, source, datatype, count
      integer, intent(out) :: buf
      
      ierror = comm + tag + source + datatype + count + mpi_global_var
      !ierror = tag
      !ierror = source
      !ierror = datatype
      status = comm + tag + source + datatype + count + mpi_global_var
      buf = comm + tag + source + datatype + count + mpi_global_var

      return
      end

