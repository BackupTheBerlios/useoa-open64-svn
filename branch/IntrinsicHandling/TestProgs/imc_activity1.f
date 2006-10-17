!======================================================================
! imc_activity2.f90
! 
! Tests activity analysis on the InterMPICFG. 
! If y in independent and f is dependent
! x is active because mpi_global_var is dep and indep.
!======================================================================
      program main
!     use mpi
!     Use the following include if the mpi module is not available
      include "mpif.h"
!      include "stub_global.h"
   
      integer n, myid, numprocs, i, ierr
      double precision x, y, z, f
c$openad INDEPENDENT(y)


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

        call MPI_RECV(x, 1, MPI_INTEGER, 0, MPI_ANY_TAG, MPI_COMM_WORLD,
     +                status, ierr)
        z = x + 2

      endif

      f = y + 2 + z

      call MPI_FINALIZE(ierr)
      stop
c$openad DEPENDENT(f)
      end

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

!     MPI_RECV stub for SideEffects
      subroutine MPI_RECV(buf,count,datatype,source,tag,comm,
     $                    status,ierror)
!     MPI_STATUS_SIZE is 4 in mpif.h
!     include "stub_global.h"
      integer, intent(out) :: status(4), ierror
      integer, intent(in) :: comm, tag, source, datatype, count
      double precision, intent(out) :: buf

      ierror = comm + tag + source + datatype + count 
      status = comm + tag + source + datatype + count 
      buf = 0.0


      return
      end
!

!     MPI_SEND stub for SideEffects
      subroutine MPI_SEND(buf, count, datatype, dest, tag,
     $                                comm, ierror)
!     include "stub_global.h"
      integer, intent(out) :: ierror
      integer, intent(in) :: comm, tag, dest, datatype, count
      double precision, intent (in) :: buf

      ierror = comm + tag + dest + datatype + count 

!     mpi_global_var = buf

      return
      end
!


