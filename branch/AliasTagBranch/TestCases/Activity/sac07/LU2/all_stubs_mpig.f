!-----------------------------------------------
!
! all_stubs_mpig.f
!
! contains all of the MPI subroutine stubs
! needed by all of the bencharks
!
! Contents (in order)
!                   _
!     MPI_SEND       \
!     MPI_ISEND       \
!                     |
!     MPI_RECV        |
!     MPI_IRECV       |
!                      >  use mpi_global_var
!     MPI_BCAST       |
!                     |
!     MPI_REDUCE      |
!     MPI_ALLREDUCE  /
!                   
!
!     MPI_INIT
!     MPI_COMM_RANK
!     MPI_COMM_SIZE
!     MPI_COMM_DUP
!     MPI_COMM_SPLIT
!     MPI_BARRIER
!     MPI_WAIT
!     MPI_WAITALL
!     MPI_FINALIZE
!     MPI_ABORT
!     MPI_
!     MPI_
!     MPI_
!-----------------------------------------------
 

!     MPI_SEND stub for SideEffects
      subroutine MPI_SEND(buf, count, datatype, dest, tag,
     $                                comm, ierror)
      integer, intent(out) :: ierror
      integer, intent(in) :: comm, tag, dest, datatype, count
      integer, intent (in) :: buf
      include "stub_global.h"

      ierror = comm
      ierror = tag
      ierror = dest
      ierror = datatype
      ierror = count
      mpi_global_var = mpi_global_var + buf

      return
      end subroutine MPI_SEND
!

!     MPI_ISEND stub for SideEffects
      subroutine MPI_ISEND(buf, count, datatype,dest,tag,comm,
     $                     request,ierror)
      integer, intent(out) :: request, ierror
      integer, intent(in) :: comm, tag, dest, datatype, count
      integer, intent (in) :: buf
      include "stub_global.h"

      ierror = comm
      ierror = tag
      ierror = dest
      ierror = datatype
      request = count
      mpi_global_var = mpi_global_var + buf

      return
      end
!

!     MPI_RECV stub for SideEffects
      subroutine MPI_RECV(buf,count,datatype,source,tag,comm,
     $                    status,ierror)
!     MPI_STATUS_SIZE is 4 in mpif.h
      integer, intent(out) :: status(4), ierror
      integer, intent(in) :: comm, tag, source, datatype, count
      integer, intent(out) :: buf
      include "stub_global.h"

      ierror = comm
      ierror = tag
      ierror = source
      ierror = datatype
      status(1) = count
      buf = mpi_global_var

      return
      end subroutine MPI_RECV
!

!     MPI_IRECV stub for SideEffects
      subroutine MPI_IRECV(buf,count,datatype,source,tag,comm,
     $                     request,ierror)
!     MPI_STATUS_SIZE is 4 in mpif.h
      integer, intent(out) :: request, ierror
      integer, intent(in) :: comm, tag, source, datatype, count
      integer, intent(out) :: buf
      include "stub_global.h"

      ierror = comm
      ierror = tag
      ierror = source
      request = datatype
      request = count
      buf = mpi_global_var

      return
      end subroutine MPI_IRECV
!

!     MPI_BCAST stub for SideEffects
      subroutine MPI_BCAST(buf,count,datatype,root,comm,ierror)
!     MPI_STATUS_SIZE is 4 in mpif.h
      integer, intent(out) :: ierror
      integer, intent(in) :: comm, root, datatype, count
      integer, intent(inout) :: buf
      include "stub_global.h"

      ierror = comm
      ierror = count
      ierror = root
      ierror = buf
      ierror = datatype
      mpi_global_var = mpi_global_var + buf
      buf = mpi_global_var

      return
      end subroutine MPI_BCAST
!

!     MPI_REDUCE stub for SideEffects
      subroutine MPI_REDUCE(sbuf,rbuf,cnt,type,op,root,comm,ierror)
      integer, intent(out) :: ierror
      integer, intent(in) :: comm, root, cnt, type, op
      integer, intent(out) :: rbuf
      integer, intent(in) :: sbuf
      include "stub_global.h"

      ierror = comm
      ierror= cnt
      ierror = type;
      ierror = op;
      ierror = root
      mpi_global_var = mpi_global_var + sbuf
      rbuf = mpi_global_var

      return
      end subroutine MPI_REDUCE
!

!     MPI_ALLREDUCE stub for SideEffects
      subroutine MPI_ALLREDUCE(sbuf,rbuf,cnt,type,op,comm,ierror)
      integer, intent(out) :: ierror
      integer, intent(in) :: comm, cnt, type, op
      integer, intent(out) :: rbuf
      integer, intent(in) :: sbuf
      include "stub_global.h"

      ierror = comm
      ierror= cnt
      ierror = type;
      ierror = op;
      mpi_global_var = mpi_global_var + sbuf;
      rbuf = mpi_global_var;

      return
      end subroutine MPI_ALLREDUCE
!

!     MPI_INIT stub for SideEffects
      subroutine MPI_INIT(ierror)
      integer, intent(out) :: ierror

      ierror = 5 ! my choice

      return
      end subroutine MPI_INIT
!

!     MPI_COMM_RANK stub for SideEffects
      subroutine MPI_COMM_RANK(comm, rank, ierror)
      integer, intent(out) :: rank, ierror
      integer, intent(in) :: comm

      rank = comm
      ierror = comm

      return
      end subroutine MPI_COMM_RANK
!

!     MPI_COMM_SIZE stub for SideEffects
      subroutine MPI_COMM_SIZE(comm, nsize, ierror)
      integer, intent(out) :: nsize, ierror
      integer, intent(in) :: comm

      nsize = comm
      ierror = comm

      return
      end subroutine MPI_COMM_SIZE
!

!     MPI_COMM_DUP stub for SideEffects
      subroutine MPI_COMM_DUP(comm, newcomm, ierror)
      integer, intent(out) :: newcomm, ierror
      integer, intent(in) :: comm

      newcomm = comm
      ierror = comm

      return
      end subroutine MPI_COMM_DUP
!

!     MPI_COMM_SPLIT stub for SideEffects
      subroutine MPI_COMM_SPLIT(comm, color, key, newcomm, ierror)
      integer, intent(out) :: newcomm, ierror
      integer, intent(in) :: comm, color, key

      newcomm = comm
      ierror = color
      ierror = key

      return
      end subroutine MPI_COMM_SPLIT
!

!     MPI_BARRIER stub for SideEffects
      subroutine MPI_BARRIER(comm, ierror)
      integer, intent(out) :: ierror
      integer, intent(in) :: comm

      ierror = comm

      return
      end subroutine MPI_BARRIER
!

!     MPI_WAIT stub for SideEffects
      subroutine MPI_WAIT(request,status,ierror)
!     MPI_STATUS_SIZE is 4 in mpif.h
      integer, intent(out) :: status(4), ierror
      integer, intent(inout) :: request
      
      ierror = request
      status(1) = request
      request = 5

      return
      end subroutine MPI_WAIT
!

!     MPI_WAITALL stub for SideEffects
      subroutine MPI_WAITALL(count,arrayrequest,arraystatus,ierror)
!     MPI_STATUS_SIZE is 4 in mpif.h
      integer, intent(in) :: count
      integer, intent(out) :: arraystatus(4), ierror
      integer, intent(inout) :: arrayrequest

      ierror = count
      arraystatus(1) = arrayrequest
      arrayrequest = 5

      return
      end subroutine MPI_WAITALL
!

!     MPI_FINALIZE stub for SideEffects
      subroutine MPI_FINALIZE(ierror)
      integer, intent(out) :: ierror

      ierror = 5 ! my choice

      return
      end subroutine MPI_FINALIZE
!

!     MPI_ABORT stub for SideEffects
      subroutine MPI_ABORT(comm, errorcode, ierror)
      integer, intent(out) :: ierror
      integer, intent(in) :: comm, errorcode

      ierror = errorcode
      ierror = comm

      return
      end subroutine MPI_ABORT
!
