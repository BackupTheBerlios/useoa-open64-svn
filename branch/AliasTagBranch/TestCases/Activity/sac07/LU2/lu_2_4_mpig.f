c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine bcast_inputs

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer IERR

c---------------------------------------------------------------------
c   root broadcasts the data
c   The data isn't contiguous or of the same type, so it's not
c   clear how to send it in the "MPI" way. 
c   We could pack the info into a buffer or we could create
c   an obscene datatype to handle it all at once. Since we only
c   broadcast the data once, just use a separate broadcast for
c   each piece. 
c---------------------------------------------------------------------
      call MPI_BCAST(ipr, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
      call MPI_BCAST(inorm, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
      call MPI_BCAST(itmax, 1, MPI_INTEGER, root, MPI_COMM_WORLD, ierr)
      call MPI_BCAST(dt, 1, dp_type, root, MPI_COMM_WORLD, ierr)
      call MPI_BCAST(omega, 1, dp_type, root, MPI_COMM_WORLD, ierr)
      call MPI_BCAST(tolrsd, 5, dp_type, root, MPI_COMM_WORLD, ierr)
      call MPI_BCAST(nx0, 1, dp_type, root, MPI_COMM_WORLD, ierr)
      call MPI_BCAST(ny0, 1, dp_type, root, MPI_COMM_WORLD, ierr)
      call MPI_BCAST(nz0, 1, dp_type, root, MPI_COMM_WORLD, ierr)

      return
      end


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine blts ( ldmx, ldmy, ldmz,
     >                  nx, ny, nz, k,
     >                  omega,
     >                  v,
     >                  ldz, ldy, ldx, d,
     >                  ist, iend, jst, jend,
     >                  nx0, ny0, ipt, jpt)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   compute the regular-sparse, block lower triangular solution:
c
c                     v <-- ( L-inv ) * v
c
c---------------------------------------------------------------------

      implicit none

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      integer ldmx, ldmy, ldmz
      integer nx, ny, nz
      integer k
      double precision  omega
      double precision  v( 5, -1:ldmx+2, -1:ldmy+2, *),
     >        ldz( 5, 5, ldmx, ldmy),
     >        ldy( 5, 5, ldmx, ldmy),
     >        ldx( 5, 5, ldmx, ldmy),
     >        d( 5, 5, ldmx, ldmy)
      integer ist, iend
      integer jst, jend
      integer nx0, ny0
      integer ipt, jpt

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, m
      integer iex
      double precision  tmp, tmp1
      double precision  tmat(5,5)


c---------------------------------------------------------------------
c   receive data from north and west
c---------------------------------------------------------------------
      iex = 0
      call exchange_1( v,k,iex )


      do j = jst, jend
         do i = ist, iend
            do m = 1, 5

                  v( m, i, j, k ) =  v( m, i, j, k )
     >    - omega * (  ldz( m, 1, i, j ) * v( 1, i, j, k-1 )
     >               + ldz( m, 2, i, j ) * v( 2, i, j, k-1 )
     >               + ldz( m, 3, i, j ) * v( 3, i, j, k-1 )
     >               + ldz( m, 4, i, j ) * v( 4, i, j, k-1 )
     >               + ldz( m, 5, i, j ) * v( 5, i, j, k-1 )  )

            end do
         end do
      end do


      do j=jst,jend
        do i = ist, iend

            do m = 1, 5

                  v( m, i, j, k ) =  v( m, i, j, k )
     > - omega * ( ldy( m, 1, i, j ) * v( 1, i, j-1, k )
     >           + ldx( m, 1, i, j ) * v( 1, i-1, j, k )
     >           + ldy( m, 2, i, j ) * v( 2, i, j-1, k )
     >           + ldx( m, 2, i, j ) * v( 2, i-1, j, k )
     >           + ldy( m, 3, i, j ) * v( 3, i, j-1, k )
     >           + ldx( m, 3, i, j ) * v( 3, i-1, j, k )
     >           + ldy( m, 4, i, j ) * v( 4, i, j-1, k )
     >           + ldx( m, 4, i, j ) * v( 4, i-1, j, k )
     >           + ldy( m, 5, i, j ) * v( 5, i, j-1, k )
     >           + ldx( m, 5, i, j ) * v( 5, i-1, j, k ) )

            end do
       
c---------------------------------------------------------------------
c   diagonal block inversion
c
c   forward elimination
c---------------------------------------------------------------------
            do m = 1, 5
               tmat( m, 1 ) = d( m, 1, i, j )
               tmat( m, 2 ) = d( m, 2, i, j )
               tmat( m, 3 ) = d( m, 3, i, j )
               tmat( m, 4 ) = d( m, 4, i, j )
               tmat( m, 5 ) = d( m, 5, i, j )
            end do

            tmp1 = 1.0d+00 / tmat( 1, 1 )
            tmp = tmp1 * tmat( 2, 1 )
            tmat( 2, 2 ) =  tmat( 2, 2 )
     >           - tmp * tmat( 1, 2 )
            tmat( 2, 3 ) =  tmat( 2, 3 )
     >           - tmp * tmat( 1, 3 )
            tmat( 2, 4 ) =  tmat( 2, 4 )
     >           - tmp * tmat( 1, 4 )
            tmat( 2, 5 ) =  tmat( 2, 5 )
     >           - tmp * tmat( 1, 5 )
            v( 2, i, j, k ) = v( 2, i, j, k )
     >        - v( 1, i, j, k ) * tmp

            tmp = tmp1 * tmat( 3, 1 )
            tmat( 3, 2 ) =  tmat( 3, 2 )
     >           - tmp * tmat( 1, 2 )
            tmat( 3, 3 ) =  tmat( 3, 3 )
     >           - tmp * tmat( 1, 3 )
            tmat( 3, 4 ) =  tmat( 3, 4 )
     >           - tmp * tmat( 1, 4 )
            tmat( 3, 5 ) =  tmat( 3, 5 )
     >           - tmp * tmat( 1, 5 )
            v( 3, i, j, k ) = v( 3, i, j, k )
     >        - v( 1, i, j, k ) * tmp

            tmp = tmp1 * tmat( 4, 1 )
            tmat( 4, 2 ) =  tmat( 4, 2 )
     >           - tmp * tmat( 1, 2 )
            tmat( 4, 3 ) =  tmat( 4, 3 )
     >           - tmp * tmat( 1, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )
     >           - tmp * tmat( 1, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )
     >           - tmp * tmat( 1, 5 )
            v( 4, i, j, k ) = v( 4, i, j, k )
     >        - v( 1, i, j, k ) * tmp

            tmp = tmp1 * tmat( 5, 1 )
            tmat( 5, 2 ) =  tmat( 5, 2 )
     >           - tmp * tmat( 1, 2 )
            tmat( 5, 3 ) =  tmat( 5, 3 )
     >           - tmp * tmat( 1, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )
     >           - tmp * tmat( 1, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )
     >           - tmp * tmat( 1, 5 )
            v( 5, i, j, k ) = v( 5, i, j, k )
     >        - v( 1, i, j, k ) * tmp



            tmp1 = 1.0d+00 / tmat( 2, 2 )
            tmp = tmp1 * tmat( 3, 2 )
            tmat( 3, 3 ) =  tmat( 3, 3 )
     >           - tmp * tmat( 2, 3 )
            tmat( 3, 4 ) =  tmat( 3, 4 )
     >           - tmp * tmat( 2, 4 )
            tmat( 3, 5 ) =  tmat( 3, 5 )
     >           - tmp * tmat( 2, 5 )
            v( 3, i, j, k ) = v( 3, i, j, k )
     >        - v( 2, i, j, k ) * tmp

            tmp = tmp1 * tmat( 4, 2 )
            tmat( 4, 3 ) =  tmat( 4, 3 )
     >           - tmp * tmat( 2, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )
     >           - tmp * tmat( 2, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )
     >           - tmp * tmat( 2, 5 )
            v( 4, i, j, k ) = v( 4, i, j, k )
     >        - v( 2, i, j, k ) * tmp

            tmp = tmp1 * tmat( 5, 2 )
            tmat( 5, 3 ) =  tmat( 5, 3 )
     >           - tmp * tmat( 2, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )
     >           - tmp * tmat( 2, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )
     >           - tmp * tmat( 2, 5 )
            v( 5, i, j, k ) = v( 5, i, j, k )
     >        - v( 2, i, j, k ) * tmp



            tmp1 = 1.0d+00 / tmat( 3, 3 )
            tmp = tmp1 * tmat( 4, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )
     >           - tmp * tmat( 3, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )
     >           - tmp * tmat( 3, 5 )
            v( 4, i, j, k ) = v( 4, i, j, k )
     >        - v( 3, i, j, k ) * tmp

            tmp = tmp1 * tmat( 5, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )
     >           - tmp * tmat( 3, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )
     >           - tmp * tmat( 3, 5 )
            v( 5, i, j, k ) = v( 5, i, j, k )
     >        - v( 3, i, j, k ) * tmp



            tmp1 = 1.0d+00 / tmat( 4, 4 )
            tmp = tmp1 * tmat( 5, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )
     >           - tmp * tmat( 4, 5 )
            v( 5, i, j, k ) = v( 5, i, j, k )
     >        - v( 4, i, j, k ) * tmp

c---------------------------------------------------------------------
c   back substitution
c---------------------------------------------------------------------
            v( 5, i, j, k ) = v( 5, i, j, k )
     >                      / tmat( 5, 5 )

            v( 4, i, j, k ) = v( 4, i, j, k )
     >           - tmat( 4, 5 ) * v( 5, i, j, k )
            v( 4, i, j, k ) = v( 4, i, j, k )
     >                      / tmat( 4, 4 )

            v( 3, i, j, k ) = v( 3, i, j, k )
     >           - tmat( 3, 4 ) * v( 4, i, j, k )
     >           - tmat( 3, 5 ) * v( 5, i, j, k )
            v( 3, i, j, k ) = v( 3, i, j, k )
     >                      / tmat( 3, 3 )

            v( 2, i, j, k ) = v( 2, i, j, k )
     >           - tmat( 2, 3 ) * v( 3, i, j, k )
     >           - tmat( 2, 4 ) * v( 4, i, j, k )
     >           - tmat( 2, 5 ) * v( 5, i, j, k )
            v( 2, i, j, k ) = v( 2, i, j, k )
     >                      / tmat( 2, 2 )

            v( 1, i, j, k ) = v( 1, i, j, k )
     >           - tmat( 1, 2 ) * v( 2, i, j, k )
     >           - tmat( 1, 3 ) * v( 3, i, j, k )
     >           - tmat( 1, 4 ) * v( 4, i, j, k )
     >           - tmat( 1, 5 ) * v( 5, i, j, k )
            v( 1, i, j, k ) = v( 1, i, j, k )
     >                      / tmat( 1, 1 )


        enddo
      enddo

c---------------------------------------------------------------------
c   send data to east and south
c---------------------------------------------------------------------
      iex = 2
      call exchange_1( v,k,iex )

      return
      end


c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine buts( ldmx, ldmy, ldmz,
     >                 nx, ny, nz, k,
     >                 omega,
     >                 v, tv,
     >                 d, udx, udy, udz,
     >                 ist, iend, jst, jend,
     >                 nx0, ny0, ipt, jpt )

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   compute the regular-sparse, block upper triangular solution:
c
c                     v <-- ( U-inv ) * v
c
c---------------------------------------------------------------------

      implicit none

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      integer ldmx, ldmy, ldmz
      integer nx, ny, nz
      integer k
      double precision  omega
      double precision  v( 5, -1:ldmx+2, -1:ldmy+2, *), 
     >        tv(5, ldmx, ldmy),
     >        d( 5, 5, ldmx, ldmy),
     >        udx( 5, 5, ldmx, ldmy),
     >        udy( 5, 5, ldmx, ldmy),
     >        udz( 5, 5, ldmx, ldmy )
      integer ist, iend
      integer jst, jend
      integer nx0, ny0
      integer ipt, jpt

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, m
      integer iex
      double precision  tmp, tmp1
      double precision  tmat(5,5)


c---------------------------------------------------------------------
c   receive data from south and east
c---------------------------------------------------------------------
      iex = 1
      call exchange_1( v,k,iex )

      do j = jend, jst, -1
         do i = iend, ist, -1
            do m = 1, 5
                  tv( m, i, j ) = 
     >      omega * (  udz( m, 1, i, j ) * v( 1, i, j, k+1 )
     >               + udz( m, 2, i, j ) * v( 2, i, j, k+1 )
     >               + udz( m, 3, i, j ) * v( 3, i, j, k+1 )
     >               + udz( m, 4, i, j ) * v( 4, i, j, k+1 )
     >               + udz( m, 5, i, j ) * v( 5, i, j, k+1 ) )
            end do
         end do
      end do


      do j = jend,jst,-1
        do i = iend,ist,-1

            do m = 1, 5
                  tv( m, i, j ) = tv( m, i, j )
     > + omega * ( udy( m, 1, i, j ) * v( 1, i, j+1, k )
     >           + udx( m, 1, i, j ) * v( 1, i+1, j, k )
     >           + udy( m, 2, i, j ) * v( 2, i, j+1, k )
     >           + udx( m, 2, i, j ) * v( 2, i+1, j, k )
     >           + udy( m, 3, i, j ) * v( 3, i, j+1, k )
     >           + udx( m, 3, i, j ) * v( 3, i+1, j, k )
     >           + udy( m, 4, i, j ) * v( 4, i, j+1, k )
     >           + udx( m, 4, i, j ) * v( 4, i+1, j, k )
     >           + udy( m, 5, i, j ) * v( 5, i, j+1, k )
     >           + udx( m, 5, i, j ) * v( 5, i+1, j, k ) )
            end do

c---------------------------------------------------------------------
c   diagonal block inversion
c---------------------------------------------------------------------
            do m = 1, 5
               tmat( m, 1 ) = d( m, 1, i, j )
               tmat( m, 2 ) = d( m, 2, i, j )
               tmat( m, 3 ) = d( m, 3, i, j )
               tmat( m, 4 ) = d( m, 4, i, j )
               tmat( m, 5 ) = d( m, 5, i, j )
            end do

            tmp1 = 1.0d+00 / tmat( 1, 1 )
            tmp = tmp1 * tmat( 2, 1 )
            tmat( 2, 2 ) =  tmat( 2, 2 )
     >           - tmp * tmat( 1, 2 )
            tmat( 2, 3 ) =  tmat( 2, 3 )
     >           - tmp * tmat( 1, 3 )
            tmat( 2, 4 ) =  tmat( 2, 4 )
     >           - tmp * tmat( 1, 4 )
            tmat( 2, 5 ) =  tmat( 2, 5 )
     >           - tmp * tmat( 1, 5 )
            tv( 2, i, j ) = tv( 2, i, j )
     >        - tv( 1, i, j ) * tmp

            tmp = tmp1 * tmat( 3, 1 )
            tmat( 3, 2 ) =  tmat( 3, 2 )
     >           - tmp * tmat( 1, 2 )
            tmat( 3, 3 ) =  tmat( 3, 3 )
     >           - tmp * tmat( 1, 3 )
            tmat( 3, 4 ) =  tmat( 3, 4 )
     >           - tmp * tmat( 1, 4 )
            tmat( 3, 5 ) =  tmat( 3, 5 )
     >           - tmp * tmat( 1, 5 )
            tv( 3, i, j ) = tv( 3, i, j )
     >        - tv( 1, i, j ) * tmp

            tmp = tmp1 * tmat( 4, 1 )
            tmat( 4, 2 ) =  tmat( 4, 2 )
     >           - tmp * tmat( 1, 2 )
            tmat( 4, 3 ) =  tmat( 4, 3 )
     >           - tmp * tmat( 1, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )
     >           - tmp * tmat( 1, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )
     >           - tmp * tmat( 1, 5 )
            tv( 4, i, j ) = tv( 4, i, j )
     >        - tv( 1, i, j ) * tmp

            tmp = tmp1 * tmat( 5, 1 )
            tmat( 5, 2 ) =  tmat( 5, 2 )
     >           - tmp * tmat( 1, 2 )
            tmat( 5, 3 ) =  tmat( 5, 3 )
     >           - tmp * tmat( 1, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )
     >           - tmp * tmat( 1, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )
     >           - tmp * tmat( 1, 5 )
            tv( 5, i, j ) = tv( 5, i, j )
     >        - tv( 1, i, j ) * tmp



            tmp1 = 1.0d+00 / tmat( 2, 2 )
            tmp = tmp1 * tmat( 3, 2 )
            tmat( 3, 3 ) =  tmat( 3, 3 )
     >           - tmp * tmat( 2, 3 )
            tmat( 3, 4 ) =  tmat( 3, 4 )
     >           - tmp * tmat( 2, 4 )
            tmat( 3, 5 ) =  tmat( 3, 5 )
     >           - tmp * tmat( 2, 5 )
            tv( 3, i, j ) = tv( 3, i, j )
     >        - tv( 2, i, j ) * tmp

            tmp = tmp1 * tmat( 4, 2 )
            tmat( 4, 3 ) =  tmat( 4, 3 )
     >           - tmp * tmat( 2, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )
     >           - tmp * tmat( 2, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )
     >           - tmp * tmat( 2, 5 )
            tv( 4, i, j ) = tv( 4, i, j )
     >        - tv( 2, i, j ) * tmp

            tmp = tmp1 * tmat( 5, 2 )
            tmat( 5, 3 ) =  tmat( 5, 3 )
     >           - tmp * tmat( 2, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )
     >           - tmp * tmat( 2, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )
     >           - tmp * tmat( 2, 5 )
            tv( 5, i, j ) = tv( 5, i, j )
     >        - tv( 2, i, j ) * tmp



            tmp1 = 1.0d+00 / tmat( 3, 3 )
            tmp = tmp1 * tmat( 4, 3 )
            tmat( 4, 4 ) =  tmat( 4, 4 )
     >           - tmp * tmat( 3, 4 )
            tmat( 4, 5 ) =  tmat( 4, 5 )
     >           - tmp * tmat( 3, 5 )
            tv( 4, i, j ) = tv( 4, i, j )
     >        - tv( 3, i, j ) * tmp

            tmp = tmp1 * tmat( 5, 3 )
            tmat( 5, 4 ) =  tmat( 5, 4 )
     >           - tmp * tmat( 3, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )
     >           - tmp * tmat( 3, 5 )
            tv( 5, i, j ) = tv( 5, i, j )
     >        - tv( 3, i, j ) * tmp



            tmp1 = 1.0d+00 / tmat( 4, 4 )
            tmp = tmp1 * tmat( 5, 4 )
            tmat( 5, 5 ) =  tmat( 5, 5 )
     >           - tmp * tmat( 4, 5 )
            tv( 5, i, j ) = tv( 5, i, j )
     >        - tv( 4, i, j ) * tmp

c---------------------------------------------------------------------
c   back substitution
c---------------------------------------------------------------------
            tv( 5, i, j ) = tv( 5, i, j )
     >                      / tmat( 5, 5 )

            tv( 4, i, j ) = tv( 4, i, j )
     >           - tmat( 4, 5 ) * tv( 5, i, j )
            tv( 4, i, j ) = tv( 4, i, j )
     >                      / tmat( 4, 4 )

            tv( 3, i, j ) = tv( 3, i, j )
     >           - tmat( 3, 4 ) * tv( 4, i, j )
     >           - tmat( 3, 5 ) * tv( 5, i, j )
            tv( 3, i, j ) = tv( 3, i, j )
     >                      / tmat( 3, 3 )

            tv( 2, i, j ) = tv( 2, i, j )
     >           - tmat( 2, 3 ) * tv( 3, i, j )
     >           - tmat( 2, 4 ) * tv( 4, i, j )
     >           - tmat( 2, 5 ) * tv( 5, i, j )
            tv( 2, i, j ) = tv( 2, i, j )
     >                      / tmat( 2, 2 )

            tv( 1, i, j ) = tv( 1, i, j )
     >           - tmat( 1, 2 ) * tv( 2, i, j )
     >           - tmat( 1, 3 ) * tv( 3, i, j )
     >           - tmat( 1, 4 ) * tv( 4, i, j )
     >           - tmat( 1, 5 ) * tv( 5, i, j )
            tv( 1, i, j ) = tv( 1, i, j )
     >                      / tmat( 1, 1 )

            v( 1, i, j, k ) = v( 1, i, j, k ) - tv( 1, i, j )
            v( 2, i, j, k ) = v( 2, i, j, k ) - tv( 2, i, j )
            v( 3, i, j, k ) = v( 3, i, j, k ) - tv( 3, i, j )
            v( 4, i, j, k ) = v( 4, i, j, k ) - tv( 4, i, j )
            v( 5, i, j, k ) = v( 5, i, j, k ) - tv( 5, i, j )


        enddo
      end do

c---------------------------------------------------------------------
c   send data to north and west
c---------------------------------------------------------------------
      iex = 3
      call exchange_1( v,k,iex )
 
      return
      end
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine erhs

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   compute the right hand side based on exact solution
c
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k, m
      integer iglob, jglob
      integer iex
      integer L1, L2
      integer ist1, iend1
      integer jst1, jend1
      double precision  dsspm
      double precision  xi, eta, zeta
      double precision  q
      double precision  u21, u31, u41
      double precision  tmp
      double precision  u21i, u31i, u41i, u51i
      double precision  u21j, u31j, u41j, u51j
      double precision  u21k, u31k, u41k, u51k
      double precision  u21im1, u31im1, u41im1, u51im1
      double precision  u21jm1, u31jm1, u41jm1, u51jm1
      double precision  u21km1, u31km1, u41km1, u51km1

      dsspm = dssp


      do k = 1, nz
         do j = 1, ny
            do i = 1, nx
               do m = 1, 5
                  frct( m, i, j, k ) = 0.0d+00
               end do
            end do
         end do
      end do

      do k = 1, nz
         zeta = ( dble(k-1) ) / ( nz - 1 )
         do j = 1, ny
            jglob = jpt + j
            eta = ( dble(jglob-1) ) / ( ny0 - 1 )
            do i = 1, nx
               iglob = ipt + i
               xi = ( dble(iglob-1) ) / ( nx0 - 1 )
               do m = 1, 5
                  rsd(m,i,j,k) =  ce(m,1)
     >                 + ce(m,2) * xi
     >                 + ce(m,3) * eta
     >                 + ce(m,4) * zeta
     >                 + ce(m,5) * xi * xi
     >                 + ce(m,6) * eta * eta
     >                 + ce(m,7) * zeta * zeta
     >                 + ce(m,8) * xi * xi * xi
     >                 + ce(m,9) * eta * eta * eta
     >                 + ce(m,10) * zeta * zeta * zeta
     >                 + ce(m,11) * xi * xi * xi * xi
     >                 + ce(m,12) * eta * eta * eta * eta
     >                 + ce(m,13) * zeta * zeta * zeta * zeta
               end do
            end do
         end do
      end do

c---------------------------------------------------------------------
c   xi-direction flux differences
c---------------------------------------------------------------------
c
c   iex = flag : iex = 0  north/south communication
c              : iex = 1  east/west communication
c
c---------------------------------------------------------------------
      iex   = 0

c---------------------------------------------------------------------
c   communicate and receive/send two rows of data
c---------------------------------------------------------------------
      call exchange_3 (rsd,iex)

      L1 = 0
      if (north.eq.-1) L1 = 1
      L2 = nx + 1
      if (south.eq.-1) L2 = nx

      do k = 2, nz - 1
         do j = jst, jend
            do i = L1, L2
               flux(1,i,j,k) = rsd(2,i,j,k)
               u21 = rsd(2,i,j,k) / rsd(1,i,j,k)
               q = 0.50d+00 * (  rsd(2,i,j,k) * rsd(2,i,j,k)
     >                         + rsd(3,i,j,k) * rsd(3,i,j,k)
     >                         + rsd(4,i,j,k) * rsd(4,i,j,k) )
     >                      / rsd(1,i,j,k)
               flux(2,i,j,k) = rsd(2,i,j,k) * u21 + c2 * 
     >                         ( rsd(5,i,j,k) - q )
               flux(3,i,j,k) = rsd(3,i,j,k) * u21
               flux(4,i,j,k) = rsd(4,i,j,k) * u21
               flux(5,i,j,k) = ( c1 * rsd(5,i,j,k) - c2 * q ) * u21
            end do
         end do
      end do 

      do k = 2, nz - 1
         do j = jst, jend
            do i = ist, iend
               do m = 1, 5
                  frct(m,i,j,k) =  frct(m,i,j,k)
     >                   - tx2 * ( flux(m,i+1,j,k) - flux(m,i-1,j,k) )
               end do
            end do
            do i = ist, L2
               tmp = 1.0d+00 / rsd(1,i,j,k)

               u21i = tmp * rsd(2,i,j,k)
               u31i = tmp * rsd(3,i,j,k)
               u41i = tmp * rsd(4,i,j,k)
               u51i = tmp * rsd(5,i,j,k)

               tmp = 1.0d+00 / rsd(1,i-1,j,k)

               u21im1 = tmp * rsd(2,i-1,j,k)
               u31im1 = tmp * rsd(3,i-1,j,k)
               u41im1 = tmp * rsd(4,i-1,j,k)
               u51im1 = tmp * rsd(5,i-1,j,k)

               flux(2,i,j,k) = (4.0d+00/3.0d+00) * tx3 * 
     >                        ( u21i - u21im1 )
               flux(3,i,j,k) = tx3 * ( u31i - u31im1 )
               flux(4,i,j,k) = tx3 * ( u41i - u41im1 )
               flux(5,i,j,k) = 0.50d+00 * ( 1.0d+00 - c1*c5 )
     >              * tx3 * ( ( u21i  **2 + u31i  **2 + u41i  **2 )
     >                      - ( u21im1**2 + u31im1**2 + u41im1**2 ) )
     >              + (1.0d+00/6.0d+00)
     >              * tx3 * ( u21i**2 - u21im1**2 )
     >              + c1 * c5 * tx3 * ( u51i - u51im1 )
            end do

            do i = ist, iend
               frct(1,i,j,k) = frct(1,i,j,k)
     >              + dx1 * tx1 * (            rsd(1,i-1,j,k)
     >                             - 2.0d+00 * rsd(1,i,j,k)
     >                             +           rsd(1,i+1,j,k) )
               frct(2,i,j,k) = frct(2,i,j,k)
     >           + tx3 * c3 * c4 * ( flux(2,i+1,j,k) - flux(2,i,j,k) )
     >              + dx2 * tx1 * (            rsd(2,i-1,j,k)
     >                             - 2.0d+00 * rsd(2,i,j,k)
     >                             +           rsd(2,i+1,j,k) )
               frct(3,i,j,k) = frct(3,i,j,k)
     >           + tx3 * c3 * c4 * ( flux(3,i+1,j,k) - flux(3,i,j,k) )
     >              + dx3 * tx1 * (            rsd(3,i-1,j,k)
     >                             - 2.0d+00 * rsd(3,i,j,k)
     >                             +           rsd(3,i+1,j,k) )
               frct(4,i,j,k) = frct(4,i,j,k)
     >            + tx3 * c3 * c4 * ( flux(4,i+1,j,k) - flux(4,i,j,k) )
     >              + dx4 * tx1 * (            rsd(4,i-1,j,k)
     >                             - 2.0d+00 * rsd(4,i,j,k)
     >                             +           rsd(4,i+1,j,k) )
               frct(5,i,j,k) = frct(5,i,j,k)
     >           + tx3 * c3 * c4 * ( flux(5,i+1,j,k) - flux(5,i,j,k) )
     >              + dx5 * tx1 * (            rsd(5,i-1,j,k)
     >                             - 2.0d+00 * rsd(5,i,j,k)
     >                             +           rsd(5,i+1,j,k) )
            end do

c---------------------------------------------------------------------
c   Fourth-order dissipation
c---------------------------------------------------------------------
            IF (north.eq.-1) then
             do m = 1, 5
               frct(m,2,j,k) = frct(m,2,j,k)
     >           - dsspm * ( + 5.0d+00 * rsd(m,2,j,k)
     >                       - 4.0d+00 * rsd(m,3,j,k)
     >                       +           rsd(m,4,j,k) )
               frct(m,3,j,k) = frct(m,3,j,k)
     >           - dsspm * ( - 4.0d+00 * rsd(m,2,j,k)
     >                       + 6.0d+00 * rsd(m,3,j,k)
     >                       - 4.0d+00 * rsd(m,4,j,k)
     >                       +           rsd(m,5,j,k) )
             end do
            END IF

            ist1 = 1
            iend1 = nx
            if (north.eq.-1) ist1 = 4
            if (south.eq.-1) iend1 = nx - 3
            do i = ist1,iend1
               do m = 1, 5
                  frct(m,i,j,k) = frct(m,i,j,k)
     >              - dsspm * (            rsd(m,i-2,j,k)
     >                         - 4.0d+00 * rsd(m,i-1,j,k)
     >                         + 6.0d+00 * rsd(m,i,j,k)
     >                         - 4.0d+00 * rsd(m,i+1,j,k)
     >                         +           rsd(m,i+2,j,k) )
               end do
            end do

            IF (south.eq.-1) then
             do m = 1, 5
               frct(m,nx-2,j,k) = frct(m,nx-2,j,k)
     >           - dsspm * (             rsd(m,nx-4,j,k)
     >                       - 4.0d+00 * rsd(m,nx-3,j,k)
     >                       + 6.0d+00 * rsd(m,nx-2,j,k)
     >                       - 4.0d+00 * rsd(m,nx-1,j,k)  )
               frct(m,nx-1,j,k) = frct(m,nx-1,j,k)
     >           - dsspm * (             rsd(m,nx-3,j,k)
     >                       - 4.0d+00 * rsd(m,nx-2,j,k)
     >                       + 5.0d+00 * rsd(m,nx-1,j,k) )
             end do
            END IF

         end do
      end do

c---------------------------------------------------------------------
c   eta-direction flux differences
c---------------------------------------------------------------------
c
c   iex = flag : iex = 0  north/south communication
c              : iex = 1  east/west communication
c
c---------------------------------------------------------------------
      iex   = 1

c---------------------------------------------------------------------
c   communicate and receive/send two rows of data
c---------------------------------------------------------------------
      call exchange_3 (rsd,iex)

      L1 = 0
      if (west.eq.-1) L1 = 1
      L2 = ny + 1
      if (east.eq.-1) L2 = ny

      do k = 2, nz - 1
         do i = ist, iend
            do j = L1, L2
               flux(1,i,j,k) = rsd(3,i,j,k)
               u31 = rsd(3,i,j,k) / rsd(1,i,j,k)
               q = 0.50d+00 * (  rsd(2,i,j,k) * rsd(2,i,j,k)
     >                         + rsd(3,i,j,k) * rsd(3,i,j,k)
     >                         + rsd(4,i,j,k) * rsd(4,i,j,k) )
     >                      / rsd(1,i,j,k)
               flux(2,i,j,k) = rsd(2,i,j,k) * u31 
               flux(3,i,j,k) = rsd(3,i,j,k) * u31 + c2 * 
     >                       ( rsd(5,i,j,k) - q )
               flux(4,i,j,k) = rsd(4,i,j,k) * u31
               flux(5,i,j,k) = ( c1 * rsd(5,i,j,k) - c2 * q ) * u31
            end do
         end do
      end do

      do k = 2, nz - 1
         do i = ist, iend
            do j = jst, jend
               do m = 1, 5
                  frct(m,i,j,k) =  frct(m,i,j,k)
     >                 - ty2 * ( flux(m,i,j+1,k) - flux(m,i,j-1,k) )
               end do
            end do

            do j = jst, L2
               tmp = 1.0d+00 / rsd(1,i,j,k)

               u21j = tmp * rsd(2,i,j,k)
               u31j = tmp * rsd(3,i,j,k)
               u41j = tmp * rsd(4,i,j,k)
               u51j = tmp * rsd(5,i,j,k)

               tmp = 1.0d+00 / rsd(1,i,j-1,k)

               u21jm1 = tmp * rsd(2,i,j-1,k)
               u31jm1 = tmp * rsd(3,i,j-1,k)
               u41jm1 = tmp * rsd(4,i,j-1,k)
               u51jm1 = tmp * rsd(5,i,j-1,k)

               flux(2,i,j,k) = ty3 * ( u21j - u21jm1 )
               flux(3,i,j,k) = (4.0d+00/3.0d+00) * ty3 * 
     >                       ( u31j - u31jm1 )
               flux(4,i,j,k) = ty3 * ( u41j - u41jm1 )
               flux(5,i,j,k) = 0.50d+00 * ( 1.0d+00 - c1*c5 )
     >              * ty3 * ( ( u21j  **2 + u31j  **2 + u41j  **2 )
     >                      - ( u21jm1**2 + u31jm1**2 + u41jm1**2 ) )
     >              + (1.0d+00/6.0d+00)
     >              * ty3 * ( u31j**2 - u31jm1**2 )
     >              + c1 * c5 * ty3 * ( u51j - u51jm1 )
            end do

            do j = jst, jend
               frct(1,i,j,k) = frct(1,i,j,k)
     >              + dy1 * ty1 * (            rsd(1,i,j-1,k)
     >                             - 2.0d+00 * rsd(1,i,j,k)
     >                             +           rsd(1,i,j+1,k) )
               frct(2,i,j,k) = frct(2,i,j,k)
     >          + ty3 * c3 * c4 * ( flux(2,i,j+1,k) - flux(2,i,j,k) )
     >              + dy2 * ty1 * (            rsd(2,i,j-1,k)
     >                             - 2.0d+00 * rsd(2,i,j,k)
     >                             +           rsd(2,i,j+1,k) )
               frct(3,i,j,k) = frct(3,i,j,k)
     >          + ty3 * c3 * c4 * ( flux(3,i,j+1,k) - flux(3,i,j,k) )
     >              + dy3 * ty1 * (            rsd(3,i,j-1,k)
     >                             - 2.0d+00 * rsd(3,i,j,k)
     >                             +           rsd(3,i,j+1,k) )
               frct(4,i,j,k) = frct(4,i,j,k)
     >          + ty3 * c3 * c4 * ( flux(4,i,j+1,k) - flux(4,i,j,k) )
     >              + dy4 * ty1 * (            rsd(4,i,j-1,k)
     >                             - 2.0d+00 * rsd(4,i,j,k)
     >                             +           rsd(4,i,j+1,k) )
               frct(5,i,j,k) = frct(5,i,j,k)
     >          + ty3 * c3 * c4 * ( flux(5,i,j+1,k) - flux(5,i,j,k) )
     >              + dy5 * ty1 * (            rsd(5,i,j-1,k)
     >                             - 2.0d+00 * rsd(5,i,j,k)
     >                             +           rsd(5,i,j+1,k) )
            end do

c---------------------------------------------------------------------
c   fourth-order dissipation
c---------------------------------------------------------------------
            IF (west.eq.-1) then
             do m = 1, 5
               frct(m,i,2,k) = frct(m,i,2,k)
     >           - dsspm * ( + 5.0d+00 * rsd(m,i,2,k)
     >                       - 4.0d+00 * rsd(m,i,3,k)
     >                       +           rsd(m,i,4,k) )
               frct(m,i,3,k) = frct(m,i,3,k)
     >           - dsspm * ( - 4.0d+00 * rsd(m,i,2,k)
     >                       + 6.0d+00 * rsd(m,i,3,k)
     >                       - 4.0d+00 * rsd(m,i,4,k)
     >                       +           rsd(m,i,5,k) )
             end do
            END IF

            jst1 = 1
            jend1 = ny
            if (west.eq.-1) jst1 = 4
            if (east.eq.-1) jend1 = ny - 3

            do j = jst1, jend1
               do m = 1, 5
                  frct(m,i,j,k) = frct(m,i,j,k)
     >              - dsspm * (            rsd(m,i,j-2,k)
     >                        - 4.0d+00 * rsd(m,i,j-1,k)
     >                        + 6.0d+00 * rsd(m,i,j,k)
     >                        - 4.0d+00 * rsd(m,i,j+1,k)
     >                        +           rsd(m,i,j+2,k) )
               end do
            end do

            IF (east.eq.-1) then
             do m = 1, 5
               frct(m,i,ny-2,k) = frct(m,i,ny-2,k)
     >           - dsspm * (             rsd(m,i,ny-4,k)
     >                       - 4.0d+00 * rsd(m,i,ny-3,k)
     >                       + 6.0d+00 * rsd(m,i,ny-2,k)
     >                       - 4.0d+00 * rsd(m,i,ny-1,k)  )
               frct(m,i,ny-1,k) = frct(m,i,ny-1,k)
     >           - dsspm * (             rsd(m,i,ny-3,k)
     >                       - 4.0d+00 * rsd(m,i,ny-2,k)
     >                       + 5.0d+00 * rsd(m,i,ny-1,k)  )
             end do
            END IF

         end do
      end do

c---------------------------------------------------------------------
c   zeta-direction flux differences
c---------------------------------------------------------------------
      do j = jst, jend
         do i = ist, iend
            do k = 1, nz
               flux(1,i,j,k) = rsd(4,i,j,k)
               u41 = rsd(4,i,j,k) / rsd(1,i,j,k)
               q = 0.50d+00 * (  rsd(2,i,j,k) * rsd(2,i,j,k)
     >                         + rsd(3,i,j,k) * rsd(3,i,j,k)
     >                         + rsd(4,i,j,k) * rsd(4,i,j,k) )
     >                      / rsd(1,i,j,k)
               flux(2,i,j,k) = rsd(2,i,j,k) * u41 
               flux(3,i,j,k) = rsd(3,i,j,k) * u41 
               flux(4,i,j,k) = rsd(4,i,j,k) * u41 + c2 * 
     >                         ( rsd(5,i,j,k) - q )
               flux(5,i,j,k) = ( c1 * rsd(5,i,j,k) - c2 * q ) * u41
            end do

            do k = 2, nz - 1
               do m = 1, 5
                  frct(m,i,j,k) =  frct(m,i,j,k)
     >                  - tz2 * ( flux(m,i,j,k+1) - flux(m,i,j,k-1) )
               end do
            end do

            do k = 2, nz
               tmp = 1.0d+00 / rsd(1,i,j,k)

               u21k = tmp * rsd(2,i,j,k)
               u31k = tmp * rsd(3,i,j,k)
               u41k = tmp * rsd(4,i,j,k)
               u51k = tmp * rsd(5,i,j,k)

               tmp = 1.0d+00 / rsd(1,i,j,k-1)

               u21km1 = tmp * rsd(2,i,j,k-1)
               u31km1 = tmp * rsd(3,i,j,k-1)
               u41km1 = tmp * rsd(4,i,j,k-1)
               u51km1 = tmp * rsd(5,i,j,k-1)

               flux(2,i,j,k) = tz3 * ( u21k - u21km1 )
               flux(3,i,j,k) = tz3 * ( u31k - u31km1 )
               flux(4,i,j,k) = (4.0d+00/3.0d+00) * tz3 * ( u41k 
     >                       - u41km1 )
               flux(5,i,j,k) = 0.50d+00 * ( 1.0d+00 - c1*c5 )
     >              * tz3 * ( ( u21k  **2 + u31k  **2 + u41k  **2 )
     >                      - ( u21km1**2 + u31km1**2 + u41km1**2 ) )
     >              + (1.0d+00/6.0d+00)
     >              * tz3 * ( u41k**2 - u41km1**2 )
     >              + c1 * c5 * tz3 * ( u51k - u51km1 )
            end do

            do k = 2, nz - 1
               frct(1,i,j,k) = frct(1,i,j,k)
     >              + dz1 * tz1 * (            rsd(1,i,j,k+1)
     >                             - 2.0d+00 * rsd(1,i,j,k)
     >                             +           rsd(1,i,j,k-1) )
               frct(2,i,j,k) = frct(2,i,j,k)
     >          + tz3 * c3 * c4 * ( flux(2,i,j,k+1) - flux(2,i,j,k) )
     >              + dz2 * tz1 * (            rsd(2,i,j,k+1)
     >                             - 2.0d+00 * rsd(2,i,j,k)
     >                             +           rsd(2,i,j,k-1) )
               frct(3,i,j,k) = frct(3,i,j,k)
     >          + tz3 * c3 * c4 * ( flux(3,i,j,k+1) - flux(3,i,j,k) )
     >              + dz3 * tz1 * (            rsd(3,i,j,k+1)
     >                             - 2.0d+00 * rsd(3,i,j,k)
     >                             +           rsd(3,i,j,k-1) )
               frct(4,i,j,k) = frct(4,i,j,k)
     >          + tz3 * c3 * c4 * ( flux(4,i,j,k+1) - flux(4,i,j,k) )
     >              + dz4 * tz1 * (            rsd(4,i,j,k+1)
     >                             - 2.0d+00 * rsd(4,i,j,k)
     >                             +           rsd(4,i,j,k-1) )
               frct(5,i,j,k) = frct(5,i,j,k)
     >          + tz3 * c3 * c4 * ( flux(5,i,j,k+1) - flux(5,i,j,k) )
     >              + dz5 * tz1 * (            rsd(5,i,j,k+1)
     >                             - 2.0d+00 * rsd(5,i,j,k)
     >                             +           rsd(5,i,j,k-1) )
            end do

c---------------------------------------------------------------------
c   fourth-order dissipation
c---------------------------------------------------------------------
            do m = 1, 5
               frct(m,i,j,2) = frct(m,i,j,2)
     >           - dsspm * ( + 5.0d+00 * rsd(m,i,j,2)
     >                       - 4.0d+00 * rsd(m,i,j,3)
     >                       +           rsd(m,i,j,4) )
               frct(m,i,j,3) = frct(m,i,j,3)
     >           - dsspm * (- 4.0d+00 * rsd(m,i,j,2)
     >                      + 6.0d+00 * rsd(m,i,j,3)
     >                      - 4.0d+00 * rsd(m,i,j,4)
     >                      +           rsd(m,i,j,5) )
            end do

            do k = 4, nz - 3
               do m = 1, 5
                  frct(m,i,j,k) = frct(m,i,j,k)
     >              - dsspm * (           rsd(m,i,j,k-2)
     >                        - 4.0d+00 * rsd(m,i,j,k-1)
     >                        + 6.0d+00 * rsd(m,i,j,k)
     >                        - 4.0d+00 * rsd(m,i,j,k+1)
     >                        +           rsd(m,i,j,k+2) )
               end do
            end do

            do m = 1, 5
               frct(m,i,j,nz-2) = frct(m,i,j,nz-2)
     >           - dsspm * (            rsd(m,i,j,nz-4)
     >                      - 4.0d+00 * rsd(m,i,j,nz-3)
     >                      + 6.0d+00 * rsd(m,i,j,nz-2)
     >                      - 4.0d+00 * rsd(m,i,j,nz-1)  )
               frct(m,i,j,nz-1) = frct(m,i,j,nz-1)
     >           - dsspm * (             rsd(m,i,j,nz-3)
     >                       - 4.0d+00 * rsd(m,i,j,nz-2)
     >                       + 5.0d+00 * rsd(m,i,j,nz-1)  )
            end do
         end do
      end do

      return
      end
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine error

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   compute the solution error
c
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k, m
      integer iglob, jglob
      double precision  tmp
      double precision  u000ijk(5), dummy(5)

      integer IERROR


      do m = 1, 5
         errnm(m) = 0.0d+00
         dummy(m) = 0.0d+00
      end do

      do k = 2, nz-1
         do j = jst, jend
            jglob = jpt + j
            do i = ist, iend
               iglob = ipt + i
               call exact( iglob, jglob, k, u000ijk )
               do m = 1, 5
                  tmp = ( u000ijk(m) - u(m,i,j,k) )
                  dummy(m) = dummy(m) + tmp ** 2
               end do
            end do
         end do
      end do

c---------------------------------------------------------------------
c   compute the global sum of individual contributions to dot product.
c---------------------------------------------------------------------
      call MPI_ALLREDUCE( dummy,
     >                    errnm,
     >                    5,
     >                    dp_type,
     >                    MPI_SUM,
     >                    MPI_COMM_WORLD,
     >                    IERROR )

      do m = 1, 5
         errnm(m) = sqrt ( errnm(m) / ( (nx0-2)*(ny0-2)*(nz0-2) ) )
      end do

c      if (id.eq.0) then
c        write (*,1002) ( errnm(m), m = 1, 5 )
c      end if

 1002 format (1x/1x,'RMS-norm of error in soln. to ',
     > 'first pde  = ',1pe12.5/,
     > 1x,'RMS-norm of error in soln. to ',
     > 'second pde = ',1pe12.5/,
     > 1x,'RMS-norm of error in soln. to ',
     > 'third pde  = ',1pe12.5/,
     > 1x,'RMS-norm of error in soln. to ',
     > 'fourth pde = ',1pe12.5/,
     > 1x,'RMS-norm of error in soln. to ',
     > 'fifth pde  = ',1pe12.5)

      return
      end
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine exact( i, j, k, u000ijk )

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   compute the exact solution at (i,j,k)
c
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      integer i, j, k
      double precision u000ijk(*)

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer m
      double precision xi, eta, zeta

      xi  = ( dble ( i - 1 ) ) / ( nx0 - 1 )
      eta  = ( dble ( j - 1 ) ) / ( ny0 - 1 )
      zeta = ( dble ( k - 1 ) ) / ( nz - 1 )


      do m = 1, 5
         u000ijk(m) =  ce(m,1)
     >        + ce(m,2) * xi
     >        + ce(m,3) * eta
     >        + ce(m,4) * zeta
     >        + ce(m,5) * xi * xi
     >        + ce(m,6) * eta * eta
     >        + ce(m,7) * zeta * zeta
     >        + ce(m,8) * xi * xi * xi
     >        + ce(m,9) * eta * eta * eta
     >        + ce(m,10) * zeta * zeta * zeta
     >        + ce(m,11) * xi * xi * xi * xi
     >        + ce(m,12) * eta * eta * eta * eta
     >        + ce(m,13) * zeta * zeta * zeta * zeta
      end do

      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine exchange_1( g,k,iex )

c---------------------------------------------------------------------
c---------------------------------------------------------------------


      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

      double precision  g(5,-1:isiz1+2,-1:isiz2+2,isiz3)
      integer k
      integer iex
      integer i, j
      double precision dum(5,isiz1+isiz2), dum1(5,isiz1+isiz2)

      integer STATUS(MPI_STATUS_SIZE)
      integer IERROR



      if( iex .eq. 0 ) then

          if( north .ne. -1 ) then
              call MPI_RECV( dum1(1,jst),
     >                       5*(jend-jst+1),
     >                       dp_type,
     >                       north,
     >                       from_n,
     >                       MPI_COMM_WORLD,
     >                       status,
     >                       IERROR )
              do j=jst,jend
                  g(1,0,j,k) = dum1(1,j)
                  g(2,0,j,k) = dum1(2,j)
                  g(3,0,j,k) = dum1(3,j)
                  g(4,0,j,k) = dum1(4,j)
                  g(5,0,j,k) = dum1(5,j)
              enddo
          endif

          if( west .ne. -1 ) then
              call MPI_RECV( dum1(1,ist),
     >                       5*(iend-ist+1),
     >                       dp_type,
     >                       west,
     >                       from_w,
     >                       MPI_COMM_WORLD,
     >                       status,
     >                       IERROR )
              do i=ist,iend
                  g(1,i,0,k) = dum1(1,i)
                  g(2,i,0,k) = dum1(2,i)
                  g(3,i,0,k) = dum1(3,i)
                  g(4,i,0,k) = dum1(4,i)
                  g(5,i,0,k) = dum1(5,i)
              enddo
          endif

      else if( iex .eq. 1 ) then

          if( south .ne. -1 ) then
              call MPI_RECV( dum1(1,jst),
     >                       5*(jend-jst+1),
     >                       dp_type,
     >                       south,
     >                       from_s,
     >                       MPI_COMM_WORLD,
     >                       status,
     >                       IERROR )
              do j=jst,jend
                  g(1,nx+1,j,k) = dum1(1,j)
                  g(2,nx+1,j,k) = dum1(2,j)
                  g(3,nx+1,j,k) = dum1(3,j)
                  g(4,nx+1,j,k) = dum1(4,j)
                  g(5,nx+1,j,k) = dum1(5,j)
              enddo
          endif

          if( east .ne. -1 ) then
              call MPI_RECV( dum1(1,ist),
     >                       5*(iend-ist+1),
     >                       dp_type,
     >                       east,
     >                       from_e,
     >                       MPI_COMM_WORLD,
     >                       status,
     >                       IERROR )
              do i=ist,iend
                  g(1,i,ny+1,k) = dum1(1,i)
                  g(2,i,ny+1,k) = dum1(2,i)
                  g(3,i,ny+1,k) = dum1(3,i)
                  g(4,i,ny+1,k) = dum1(4,i)
                  g(5,i,ny+1,k) = dum1(5,i)
              enddo
          endif

      else if( iex .eq. 2 ) then

          if( south .ne. -1 ) then
              do j=jst,jend
                  dum(1,j) = g(1,nx,j,k) 
                  dum(2,j) = g(2,nx,j,k) 
                  dum(3,j) = g(3,nx,j,k) 
                  dum(4,j) = g(4,nx,j,k) 
                  dum(5,j) = g(5,nx,j,k) 
              enddo
              call MPI_SEND( dum(1,jst), 
     >                       5*(jend-jst+1), 
     >                       dp_type, 
     >                       south, 
     >                       from_n, 
     >                       MPI_COMM_WORLD, 
     >                       IERROR )
          endif

          if( east .ne. -1 ) then
              do i=ist,iend
                  dum(1,i) = g(1,i,ny,k)
                  dum(2,i) = g(2,i,ny,k)
                  dum(3,i) = g(3,i,ny,k)
                  dum(4,i) = g(4,i,ny,k)
                  dum(5,i) = g(5,i,ny,k)
              enddo
              call MPI_SEND( dum(1,ist), 
     >                       5*(iend-ist+1), 
     >                       dp_type, 
     >                       east, 
     >                       from_w, 
     >                       MPI_COMM_WORLD, 
     >                       IERROR )
          endif

      else

          if( north .ne. -1 ) then
              do j=jst,jend
                  dum(1,j) = g(1,1,j,k)
                  dum(2,j) = g(2,1,j,k)
                  dum(3,j) = g(3,1,j,k)
                  dum(4,j) = g(4,1,j,k)
                  dum(5,j) = g(5,1,j,k)
              enddo
              call MPI_SEND( dum(1,jst), 
     >                       5*(jend-jst+1), 
     >                       dp_type, 
     >                       north, 
     >                       from_s, 
     >                       MPI_COMM_WORLD, 
     >                       IERROR )
          endif

          if( west .ne. -1 ) then
              do i=ist,iend
                  dum(1,i) = g(1,i,1,k)
                  dum(2,i) = g(2,i,1,k)
                  dum(3,i) = g(3,i,1,k)
                  dum(4,i) = g(4,i,1,k)
                  dum(5,i) = g(5,i,1,k)
              enddo
              call MPI_SEND( dum(1,ist), 
     >                       5*(iend-ist+1), 
     >                       dp_type, 
     >                       west, 
     >                       from_e, 
     >                       MPI_COMM_WORLD, 
     >                       IERROR )
          endif

      endif

      end




c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine exchange_3(g,iex)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   compute the right hand side based on exact solution
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      double precision  g(5,-1:isiz1+2,-1:isiz2+2,isiz3)
      integer iex

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k
      integer ipos1, ipos2

      integer mid
      integer STATUS(MPI_STATUS_SIZE)
      integer IERROR



      if (iex.eq.0) then
c---------------------------------------------------------------------
c   communicate in the south and north directions
c---------------------------------------------------------------------
      if (north.ne.-1) then
          call MPI_IRECV( buf1,
     >                    10*ny*nz,
     >                    dp_type,
     >                    MPI_ANY_SOURCE,
     >                    from_n,
     >                    MPI_COMM_WORLD,
     >                    mid,
     >                    IERROR )
      end if

c---------------------------------------------------------------------
c   send south
c---------------------------------------------------------------------
      if (south.ne.-1) then
          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              buf(1,ipos1) = g(1,nx-1,j,k) 
              buf(2,ipos1) = g(2,nx-1,j,k) 
              buf(3,ipos1) = g(3,nx-1,j,k) 
              buf(4,ipos1) = g(4,nx-1,j,k) 
              buf(5,ipos1) = g(5,nx-1,j,k) 
              buf(1,ipos2) = g(1,nx,j,k)
              buf(2,ipos2) = g(2,nx,j,k)
              buf(3,ipos2) = g(3,nx,j,k)
              buf(4,ipos2) = g(4,nx,j,k)
              buf(5,ipos2) = g(5,nx,j,k)
            end do
          end do

          call MPI_SEND( buf,
     >                   10*ny*nz,
     >                   dp_type,
     >                   south,
     >                   from_n,
     >                   MPI_COMM_WORLD,
     >                   IERROR )
        end if

c---------------------------------------------------------------------
c   receive from north
c---------------------------------------------------------------------
        if (north.ne.-1) then
          call MPI_WAIT( mid, STATUS, IERROR )

          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              g(1,-1,j,k) = buf1(1,ipos1)
              g(2,-1,j,k) = buf1(2,ipos1)
              g(3,-1,j,k) = buf1(3,ipos1)
              g(4,-1,j,k) = buf1(4,ipos1)
              g(5,-1,j,k) = buf1(5,ipos1)
              g(1,0,j,k) = buf1(1,ipos2)
              g(2,0,j,k) = buf1(2,ipos2)
              g(3,0,j,k) = buf1(3,ipos2)
              g(4,0,j,k) = buf1(4,ipos2)
              g(5,0,j,k) = buf1(5,ipos2)
            end do
          end do

        end if

      if (south.ne.-1) then
          call MPI_IRECV( buf1,
     >                    10*ny*nz,
     >                    dp_type,
     >                    MPI_ANY_SOURCE,
     >                    from_s,
     >                    MPI_COMM_WORLD,
     >                    mid,
     >                    IERROR )
      end if

c---------------------------------------------------------------------
c   send north
c---------------------------------------------------------------------
        if (north.ne.-1) then
          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              buf(1,ipos1) = g(1,2,j,k)
              buf(2,ipos1) = g(2,2,j,k)
              buf(3,ipos1) = g(3,2,j,k)
              buf(4,ipos1) = g(4,2,j,k)
              buf(5,ipos1) = g(5,2,j,k)
              buf(1,ipos2) = g(1,1,j,k)
              buf(2,ipos2) = g(2,1,j,k)
              buf(3,ipos2) = g(3,1,j,k)
              buf(4,ipos2) = g(4,1,j,k)
              buf(5,ipos2) = g(5,1,j,k)
            end do
          end do

          call MPI_SEND( buf,
     >                   10*ny*nz,
     >                   dp_type,
     >                   north,
     >                   from_s,
     >                   MPI_COMM_WORLD,
     >                   IERROR )
        end if

c---------------------------------------------------------------------
c   receive from south
c---------------------------------------------------------------------
        if (south.ne.-1) then
          call MPI_WAIT( mid, STATUS, IERROR )

          do k = 1,nz
            do j = 1,ny
              ipos1 = (k-1)*ny + j
              ipos2 = ipos1 + ny*nz
              g(1,nx+2,j,k)  = buf1(1,ipos1)
              g(2,nx+2,j,k)  = buf1(2,ipos1)
              g(3,nx+2,j,k)  = buf1(3,ipos1)
              g(4,nx+2,j,k)  = buf1(4,ipos1)
              g(5,nx+2,j,k)  = buf1(5,ipos1)
              g(1,nx+1,j,k) = buf1(1,ipos2)
              g(2,nx+1,j,k) = buf1(2,ipos2)
              g(3,nx+1,j,k) = buf1(3,ipos2)
              g(4,nx+1,j,k) = buf1(4,ipos2)
              g(5,nx+1,j,k) = buf1(5,ipos2)
            end do
          end do
        end if

      else

c---------------------------------------------------------------------
c   communicate in the east and west directions
c---------------------------------------------------------------------
      if (west.ne.-1) then
          call MPI_IRECV( buf1,
     >                    10*nx*nz,
     >                    dp_type,
     >                    MPI_ANY_SOURCE,
     >                    from_w,
     >                    MPI_COMM_WORLD,
     >                    mid,
     >                    IERROR )
      end if

c---------------------------------------------------------------------
c   send east
c---------------------------------------------------------------------
        if (east.ne.-1) then
          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              buf(1,ipos1) = g(1,i,ny-1,k)
              buf(2,ipos1) = g(2,i,ny-1,k)
              buf(3,ipos1) = g(3,i,ny-1,k)
              buf(4,ipos1) = g(4,i,ny-1,k)
              buf(5,ipos1) = g(5,i,ny-1,k)
              buf(1,ipos2) = g(1,i,ny,k)
              buf(2,ipos2) = g(2,i,ny,k)
              buf(3,ipos2) = g(3,i,ny,k)
              buf(4,ipos2) = g(4,i,ny,k)
              buf(5,ipos2) = g(5,i,ny,k)
            end do
          end do

          call MPI_SEND( buf,
     >                   10*nx*nz,
     >                   dp_type,
     >                   east,
     >                   from_w,
     >                   MPI_COMM_WORLD,
     >                   IERROR )
        end if

c---------------------------------------------------------------------
c   receive from west
c---------------------------------------------------------------------
        if (west.ne.-1) then
          call MPI_WAIT( mid, STATUS, IERROR )

          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              g(1,i,-1,k) = buf1(1,ipos1)
              g(2,i,-1,k) = buf1(2,ipos1)
              g(3,i,-1,k) = buf1(3,ipos1)
              g(4,i,-1,k) = buf1(4,ipos1)
              g(5,i,-1,k) = buf1(5,ipos1)
              g(1,i,0,k) = buf1(1,ipos2)
              g(2,i,0,k) = buf1(2,ipos2)
              g(3,i,0,k) = buf1(3,ipos2)
              g(4,i,0,k) = buf1(4,ipos2)
              g(5,i,0,k) = buf1(5,ipos2)
            end do
          end do

        end if

      if (east.ne.-1) then
          call MPI_IRECV( buf1,
     >                    10*nx*nz,
     >                    dp_type,
     >                    MPI_ANY_SOURCE,
     >                    from_e,
     >                    MPI_COMM_WORLD,
     >                    mid,
     >                    IERROR )
      end if

c---------------------------------------------------------------------
c   send west
c---------------------------------------------------------------------
      if (west.ne.-1) then
          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              buf(1,ipos1) = g(1,i,2,k)
              buf(2,ipos1) = g(2,i,2,k)
              buf(3,ipos1) = g(3,i,2,k)
              buf(4,ipos1) = g(4,i,2,k)
              buf(5,ipos1) = g(5,i,2,k)
              buf(1,ipos2) = g(1,i,1,k)
              buf(2,ipos2) = g(2,i,1,k)
              buf(3,ipos2) = g(3,i,1,k)
              buf(4,ipos2) = g(4,i,1,k)
              buf(5,ipos2) = g(5,i,1,k)
            end do
          end do

          call MPI_SEND( buf,
     >                   10*nx*nz,
     >                   dp_type,
     >                   west,
     >                   from_e,
     >                   MPI_COMM_WORLD,
     >                   IERROR )
        end if

c---------------------------------------------------------------------
c   receive from east
c---------------------------------------------------------------------
        if (east.ne.-1) then
          call MPI_WAIT( mid, STATUS, IERROR )

          do k = 1,nz
            do i = 1,nx
              ipos1 = (k-1)*nx + i
              ipos2 = ipos1 + nx*nz
              g(1,i,ny+2,k)  = buf1(1,ipos1)
              g(2,i,ny+2,k)  = buf1(2,ipos1)
              g(3,i,ny+2,k)  = buf1(3,ipos1)
              g(4,i,ny+2,k)  = buf1(4,ipos1)
              g(5,i,ny+2,k)  = buf1(5,ipos1)
              g(1,i,ny+1,k) = buf1(1,ipos2)
              g(2,i,ny+1,k) = buf1(2,ipos2)
              g(3,i,ny+1,k) = buf1(3,ipos2)
              g(4,i,ny+1,k) = buf1(4,ipos2)
              g(5,i,ny+1,k) = buf1(5,ipos2)
            end do
          end do

        end if

      end if

      return
      end     

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine exchange_4(g,h,ibeg,ifin1,jbeg,jfin1)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   compute the right hand side based on exact solution
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      double precision  g(0:isiz2+1,0:isiz3+1), 
     >        h(0:isiz2+1,0:isiz3+1)
      integer ibeg, ifin1
      integer jbeg, jfin1

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j
      integer ny2
      double precision  dum(1024)

      integer msgid1, msgid3
      integer STATUS(MPI_STATUS_SIZE)
      integer IERROR



      ny2 = ny + 2

c---------------------------------------------------------------------
c   communicate in the east and west directions
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   receive from east
c---------------------------------------------------------------------
      if (jfin1.eq.ny) then
        call MPI_IRECV( dum,
     >                  2*nx,
     >                  dp_type,
     >                  MPI_ANY_SOURCE,
     >                  from_e,
     >                  MPI_COMM_WORLD,
     >                  msgid3,
     >                  IERROR )

        call MPI_WAIT( msgid3, STATUS, IERROR )

        do i = 1,nx
          g(i,ny+1) = dum(i)
          h(i,ny+1) = dum(i+nx)
        end do

      end if

c---------------------------------------------------------------------
c   send west
c---------------------------------------------------------------------
      if (jbeg.eq.1) then
        do i = 1,nx
          dum(i) = g(i,1)
          dum(i+nx) = h(i,1)
        end do

        call MPI_SEND( dum,
     >                 2*nx,
     >                 dp_type,
     >                 west,
     >                 from_e,
     >                 MPI_COMM_WORLD,
     >                 IERROR )

      end if

c---------------------------------------------------------------------
c   communicate in the south and north directions
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   receive from south
c---------------------------------------------------------------------
      if (ifin1.eq.nx) then
        call MPI_IRECV( dum,
     >                  2*ny2,
     >                  dp_type,
     >                  MPI_ANY_SOURCE,
     >                  from_s,
     >                  MPI_COMM_WORLD,
     >                  msgid1,
     >                  IERROR )

        call MPI_WAIT( msgid1, STATUS, IERROR )

        do j = 0,ny+1
          g(nx+1,j) = dum(j+1)
          h(nx+1,j) = dum(j+ny2+1)
        end do

      end if

c---------------------------------------------------------------------
c   send north
c---------------------------------------------------------------------
      if (ibeg.eq.1) then
        do j = 0,ny+1
          dum(j+1) = g(1,j)
          dum(j+ny2+1) = h(1,j)
        end do

        call MPI_SEND( dum,
     >                 2*ny2,
     >                 dp_type,
     >                 north,
     >                 from_s,
     >                 MPI_COMM_WORLD,
     >                 IERROR )

      end if

      return
      end     

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine exchange_5(g,ibeg,ifin1)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   compute the right hand side based on exact solution
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      double precision  g(0:isiz2+1,0:isiz3+1)
      integer ibeg, ifin1

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer k
      double precision  dum(1024)

      integer msgid1
      integer STATUS(MPI_STATUS_SIZE)
      integer IERROR



c---------------------------------------------------------------------
c   communicate in the south and north directions
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   receive from south
c---------------------------------------------------------------------
      if (ifin1.eq.nx) then
        call MPI_IRECV( dum,
     >                  nz,
     >                  dp_type,
     >                  MPI_ANY_SOURCE,
     >                  from_s,
     >                  MPI_COMM_WORLD,
     >                  msgid1,
     >                  IERROR )

        call MPI_WAIT( msgid1, STATUS, IERROR )

        do k = 1,nz
          g(nx+1,k) = dum(k)
        end do

      end if

c---------------------------------------------------------------------
c   send north
c---------------------------------------------------------------------
      if (ibeg.eq.1) then
        do k = 1,nz
          dum(k) = g(1,k)
        end do

        call MPI_SEND( dum,
     >                 nz,
     >                 dp_type,
     >                 north,
     >                 from_s,
     >                 MPI_COMM_WORLD,
     >                 IERROR )

      end if

      return
      end     

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine exchange_6(g,jbeg,jfin1)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   compute the right hand side based on exact solution
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      double precision  g(0:isiz2+1,0:isiz3+1)
      integer jbeg, jfin1

c---------------------------------------------------------------------
c  local parameters
c---------------------------------------------------------------------
      integer k
      double precision  dum(1024)

      integer msgid3
      integer STATUS(MPI_STATUS_SIZE)
      integer IERROR



c---------------------------------------------------------------------
c   communicate in the east and west directions
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   receive from east
c---------------------------------------------------------------------
      if (jfin1.eq.ny) then
        call MPI_IRECV( dum,
     >                  nz,
     >                  dp_type,
     >                  MPI_ANY_SOURCE,
     >                  from_e,
     >                  MPI_COMM_WORLD,
     >                  msgid3,
     >                  IERROR )

        call MPI_WAIT( msgid3, STATUS, IERROR )

        do k = 1,nz
          g(ny+1,k) = dum(k)
        end do

      end if

c---------------------------------------------------------------------
c   send west
c---------------------------------------------------------------------
      if (jbeg.eq.1) then
        do k = 1,nz
          dum(k) = g(1,k)
        end do

        call MPI_SEND( dum,
     >                 nz,
     >                 dp_type,
     >                 west,
     >                 from_e,
     >                 MPI_COMM_WORLD,
     >                 IERROR )

      end if

      return
      end     

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine init_comm 

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   initialize MPI and establish rank and size
c
c This is a module in the MPI implementation of LUSSOR
c pseudo application from the NAS Parallel Benchmarks. 
c
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

      integer nodedim
      integer IERROR


c---------------------------------------------------------------------
c    initialize MPI communication
c---------------------------------------------------------------------
      call MPI_INIT( IERROR )

c---------------------------------------------------------------------
c   establish the global rank of this process
c---------------------------------------------------------------------
      call MPI_COMM_RANK( MPI_COMM_WORLD,
     >                     id,
     >                     IERROR )

c---------------------------------------------------------------------
c   establish the size of the global group
c---------------------------------------------------------------------
      call MPI_COMM_SIZE( MPI_COMM_WORLD,
     >                     num,
     >                     IERROR )

      ndim   = nodedim(num)

      if (.not. convertdouble) then
         dp_type = MPI_DOUBLE_PRECISION
      else
         dp_type = MPI_REAL
      endif


      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine jacld(k)

c---------------------------------------------------------------------
c---------------------------------------------------------------------


c---------------------------------------------------------------------
c   compute the lower triangular part of the jacobian matrix
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      integer k

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j
      double precision  r43
      double precision  c1345
      double precision  c34
      double precision  tmp1, tmp2, tmp3



      r43 = ( 4.0d+00 / 3.0d+00 )
      c1345 = c1 * c3 * c4 * c5
      c34 = c3 * c4

         do j = jst, jend
            do i = ist, iend

c---------------------------------------------------------------------
c   form the block daigonal
c---------------------------------------------------------------------
               tmp1 = 1.0d+00 / u(1,i,j,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               d(1,1,i,j) =  1.0d+00
     >                       + dt * 2.0d+00 * (   tx1 * dx1
     >                                          + ty1 * dy1
     >                                          + tz1 * dz1 )
               d(1,2,i,j) =  0.0d+00
               d(1,3,i,j) =  0.0d+00
               d(1,4,i,j) =  0.0d+00
               d(1,5,i,j) =  0.0d+00

               d(2,1,i,j) =  dt * 2.0d+00
     >          * (  tx1 * ( - r43 * c34 * tmp2 * u(2,i,j,k) )
     >             + ty1 * ( -       c34 * tmp2 * u(2,i,j,k) )
     >             + tz1 * ( -       c34 * tmp2 * u(2,i,j,k) ) )
               d(2,2,i,j) =  1.0d+00
     >          + dt * 2.0d+00 
     >          * (  tx1 * r43 * c34 * tmp1
     >             + ty1 *       c34 * tmp1
     >             + tz1 *       c34 * tmp1 )
     >          + dt * 2.0d+00 * (   tx1 * dx2
     >                             + ty1 * dy2
     >                             + tz1 * dz2  )
               d(2,3,i,j) = 0.0d+00
               d(2,4,i,j) = 0.0d+00
               d(2,5,i,j) = 0.0d+00

               d(3,1,i,j) = dt * 2.0d+00
     >      * (  tx1 * ( -       c34 * tmp2 * u(3,i,j,k) )
     >         + ty1 * ( - r43 * c34 * tmp2 * u(3,i,j,k) )
     >         + tz1 * ( -       c34 * tmp2 * u(3,i,j,k) ) )
               d(3,2,i,j) = 0.0d+00
               d(3,3,i,j) = 1.0d+00
     >         + dt * 2.0d+00
     >              * (  tx1 *       c34 * tmp1
     >                 + ty1 * r43 * c34 * tmp1
     >                 + tz1 *       c34 * tmp1 )
     >         + dt * 2.0d+00 * (  tx1 * dx3
     >                           + ty1 * dy3
     >                           + tz1 * dz3 )
               d(3,4,i,j) = 0.0d+00
               d(3,5,i,j) = 0.0d+00

               d(4,1,i,j) = dt * 2.0d+00
     >      * (  tx1 * ( -       c34 * tmp2 * u(4,i,j,k) )
     >         + ty1 * ( -       c34 * tmp2 * u(4,i,j,k) )
     >         + tz1 * ( - r43 * c34 * tmp2 * u(4,i,j,k) ) )
               d(4,2,i,j) = 0.0d+00
               d(4,3,i,j) = 0.0d+00
               d(4,4,i,j) = 1.0d+00
     >         + dt * 2.0d+00
     >              * (  tx1 *       c34 * tmp1
     >                 + ty1 *       c34 * tmp1
     >                 + tz1 * r43 * c34 * tmp1 )
     >         + dt * 2.0d+00 * (  tx1 * dx4
     >                           + ty1 * dy4
     >                           + tz1 * dz4 )
               d(4,5,i,j) = 0.0d+00

               d(5,1,i,j) = dt * 2.0d+00
     > * ( tx1 * ( - ( r43*c34 - c1345 ) * tmp3 * ( u(2,i,j,k) ** 2 )
     >             - ( c34 - c1345 ) * tmp3 * ( u(3,i,j,k) ** 2 )
     >             - ( c34 - c1345 ) * tmp3 * ( u(4,i,j,k) ** 2 )
     >             - ( c1345 ) * tmp2 * u(5,i,j,k) )
     >   + ty1 * ( - ( c34 - c1345 ) * tmp3 * ( u(2,i,j,k) ** 2 )
     >             - ( r43*c34 - c1345 ) * tmp3 * ( u(3,i,j,k) ** 2 )
     >             - ( c34 - c1345 ) * tmp3 * ( u(4,i,j,k) ** 2 )
     >             - ( c1345 ) * tmp2 * u(5,i,j,k) )
     >   + tz1 * ( - ( c34 - c1345 ) * tmp3 * ( u(2,i,j,k) ** 2 )
     >             - ( c34 - c1345 ) * tmp3 * ( u(3,i,j,k) ** 2 )
     >             - ( r43*c34 - c1345 ) * tmp3 * ( u(4,i,j,k) ** 2 )
     >             - ( c1345 ) * tmp2 * u(5,i,j,k) ) )
               d(5,2,i,j) = dt * 2.0d+00
     > * ( tx1 * ( r43*c34 - c1345 ) * tmp2 * u(2,i,j,k)
     >   + ty1 * (     c34 - c1345 ) * tmp2 * u(2,i,j,k)
     >   + tz1 * (     c34 - c1345 ) * tmp2 * u(2,i,j,k) )
               d(5,3,i,j) = dt * 2.0d+00
     > * ( tx1 * ( c34 - c1345 ) * tmp2 * u(3,i,j,k)
     >   + ty1 * ( r43*c34 -c1345 ) * tmp2 * u(3,i,j,k)
     >   + tz1 * ( c34 - c1345 ) * tmp2 * u(3,i,j,k) )
               d(5,4,i,j) = dt * 2.0d+00
     > * ( tx1 * ( c34 - c1345 ) * tmp2 * u(4,i,j,k)
     >   + ty1 * ( c34 - c1345 ) * tmp2 * u(4,i,j,k)
     >   + tz1 * ( r43*c34 - c1345 ) * tmp2 * u(4,i,j,k) )
               d(5,5,i,j) = 1.0d+00
     >   + dt * 2.0d+00 * ( tx1 * c1345 * tmp1
     >                    + ty1 * c1345 * tmp1
     >                    + tz1 * c1345 * tmp1 )
     >   + dt * 2.0d+00 * (  tx1 * dx5
     >                    +  ty1 * dy5
     >                    +  tz1 * dz5 )

c---------------------------------------------------------------------
c   form the first block sub-diagonal
c---------------------------------------------------------------------
               tmp1 = 1.0d+00 / u(1,i,j,k-1)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               a(1,1,i,j) = - dt * tz1 * dz1
               a(1,2,i,j) =   0.0d+00
               a(1,3,i,j) =   0.0d+00
               a(1,4,i,j) = - dt * tz2
               a(1,5,i,j) =   0.0d+00

               a(2,1,i,j) = - dt * tz2
     >           * ( - ( u(2,i,j,k-1)*u(4,i,j,k-1) ) * tmp2 )
     >           - dt * tz1 * ( - c34 * tmp2 * u(2,i,j,k-1) )
               a(2,2,i,j) = - dt * tz2 * ( u(4,i,j,k-1) * tmp1 )
     >           - dt * tz1 * c34 * tmp1
     >           - dt * tz1 * dz2 
               a(2,3,i,j) = 0.0d+00
               a(2,4,i,j) = - dt * tz2 * ( u(2,i,j,k-1) * tmp1 )
               a(2,5,i,j) = 0.0d+00

               a(3,1,i,j) = - dt * tz2
     >           * ( - ( u(3,i,j,k-1)*u(4,i,j,k-1) ) * tmp2 )
     >           - dt * tz1 * ( - c34 * tmp2 * u(3,i,j,k-1) )
               a(3,2,i,j) = 0.0d+00
               a(3,3,i,j) = - dt * tz2 * ( u(4,i,j,k-1) * tmp1 )
     >           - dt * tz1 * ( c34 * tmp1 )
     >           - dt * tz1 * dz3
               a(3,4,i,j) = - dt * tz2 * ( u(3,i,j,k-1) * tmp1 )
               a(3,5,i,j) = 0.0d+00

               a(4,1,i,j) = - dt * tz2
     >        * ( - ( u(4,i,j,k-1) * tmp1 ) ** 2
     >            + 0.50d+00 * c2
     >            * ( ( u(2,i,j,k-1) * u(2,i,j,k-1)
     >                + u(3,i,j,k-1) * u(3,i,j,k-1)
     >                + u(4,i,j,k-1) * u(4,i,j,k-1) ) * tmp2 ) )
     >        - dt * tz1 * ( - r43 * c34 * tmp2 * u(4,i,j,k-1) )
               a(4,2,i,j) = - dt * tz2
     >             * ( - c2 * ( u(2,i,j,k-1) * tmp1 ) )
               a(4,3,i,j) = - dt * tz2
     >             * ( - c2 * ( u(3,i,j,k-1) * tmp1 ) )
               a(4,4,i,j) = - dt * tz2 * ( 2.0d+00 - c2 )
     >             * ( u(4,i,j,k-1) * tmp1 )
     >             - dt * tz1 * ( r43 * c34 * tmp1 )
     >             - dt * tz1 * dz4
               a(4,5,i,j) = - dt * tz2 * c2

               a(5,1,i,j) = - dt * tz2
     >     * ( ( c2 * (  u(2,i,j,k-1) * u(2,i,j,k-1)
     >                 + u(3,i,j,k-1) * u(3,i,j,k-1)
     >                 + u(4,i,j,k-1) * u(4,i,j,k-1) ) * tmp2
     >       - c1 * ( u(5,i,j,k-1) * tmp1 ) )
     >            * ( u(4,i,j,k-1) * tmp1 ) )
     >       - dt * tz1
     >       * ( - ( c34 - c1345 ) * tmp3 * (u(2,i,j,k-1)**2)
     >           - ( c34 - c1345 ) * tmp3 * (u(3,i,j,k-1)**2)
     >           - ( r43*c34 - c1345 )* tmp3 * (u(4,i,j,k-1)**2)
     >          - c1345 * tmp2 * u(5,i,j,k-1) )
               a(5,2,i,j) = - dt * tz2
     >       * ( - c2 * ( u(2,i,j,k-1)*u(4,i,j,k-1) ) * tmp2 )
     >       - dt * tz1 * ( c34 - c1345 ) * tmp2 * u(2,i,j,k-1)
               a(5,3,i,j) = - dt * tz2
     >       * ( - c2 * ( u(3,i,j,k-1)*u(4,i,j,k-1) ) * tmp2 )
     >       - dt * tz1 * ( c34 - c1345 ) * tmp2 * u(3,i,j,k-1)
               a(5,4,i,j) = - dt * tz2
     >       * ( c1 * ( u(5,i,j,k-1) * tmp1 )
     >       - 0.50d+00 * c2
     >       * ( (  u(2,i,j,k-1)*u(2,i,j,k-1)
     >            + u(3,i,j,k-1)*u(3,i,j,k-1)
     >            + 3.0d+00*u(4,i,j,k-1)*u(4,i,j,k-1) ) * tmp2 ) )
     >       - dt * tz1 * ( r43*c34 - c1345 ) * tmp2 * u(4,i,j,k-1)
               a(5,5,i,j) = - dt * tz2
     >       * ( c1 * ( u(4,i,j,k-1) * tmp1 ) )
     >       - dt * tz1 * c1345 * tmp1
     >       - dt * tz1 * dz5

c---------------------------------------------------------------------
c   form the second block sub-diagonal
c---------------------------------------------------------------------
               tmp1 = 1.0d+00 / u(1,i,j-1,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               b(1,1,i,j) = - dt * ty1 * dy1
               b(1,2,i,j) =   0.0d+00
               b(1,3,i,j) = - dt * ty2
               b(1,4,i,j) =   0.0d+00
               b(1,5,i,j) =   0.0d+00

               b(2,1,i,j) = - dt * ty2
     >           * ( - ( u(2,i,j-1,k)*u(3,i,j-1,k) ) * tmp2 )
     >           - dt * ty1 * ( - c34 * tmp2 * u(2,i,j-1,k) )
               b(2,2,i,j) = - dt * ty2 * ( u(3,i,j-1,k) * tmp1 )
     >          - dt * ty1 * ( c34 * tmp1 )
     >          - dt * ty1 * dy2
               b(2,3,i,j) = - dt * ty2 * ( u(2,i,j-1,k) * tmp1 )
               b(2,4,i,j) = 0.0d+00
               b(2,5,i,j) = 0.0d+00

               b(3,1,i,j) = - dt * ty2
     >           * ( - ( u(3,i,j-1,k) * tmp1 ) ** 2
     >      + 0.50d+00 * c2 * ( (  u(2,i,j-1,k) * u(2,i,j-1,k)
     >                           + u(3,i,j-1,k) * u(3,i,j-1,k)
     >                           + u(4,i,j-1,k) * u(4,i,j-1,k) )
     >                          * tmp2 ) )
     >       - dt * ty1 * ( - r43 * c34 * tmp2 * u(3,i,j-1,k) )
               b(3,2,i,j) = - dt * ty2
     >                   * ( - c2 * ( u(2,i,j-1,k) * tmp1 ) )
               b(3,3,i,j) = - dt * ty2 * ( ( 2.0d+00 - c2 )
     >                   * ( u(3,i,j-1,k) * tmp1 ) )
     >       - dt * ty1 * ( r43 * c34 * tmp1 )
     >       - dt * ty1 * dy3
               b(3,4,i,j) = - dt * ty2
     >                   * ( - c2 * ( u(4,i,j-1,k) * tmp1 ) )
               b(3,5,i,j) = - dt * ty2 * c2

               b(4,1,i,j) = - dt * ty2
     >              * ( - ( u(3,i,j-1,k)*u(4,i,j-1,k) ) * tmp2 )
     >       - dt * ty1 * ( - c34 * tmp2 * u(4,i,j-1,k) )
               b(4,2,i,j) = 0.0d+00
               b(4,3,i,j) = - dt * ty2 * ( u(4,i,j-1,k) * tmp1 )
               b(4,4,i,j) = - dt * ty2 * ( u(3,i,j-1,k) * tmp1 )
     >                        - dt * ty1 * ( c34 * tmp1 )
     >                        - dt * ty1 * dy4
               b(4,5,i,j) = 0.0d+00

               b(5,1,i,j) = - dt * ty2
     >          * ( ( c2 * (  u(2,i,j-1,k) * u(2,i,j-1,k)
     >                      + u(3,i,j-1,k) * u(3,i,j-1,k)
     >                      + u(4,i,j-1,k) * u(4,i,j-1,k) ) * tmp2
     >               - c1 * ( u(5,i,j-1,k) * tmp1 ) )
     >          * ( u(3,i,j-1,k) * tmp1 ) )
     >          - dt * ty1
     >          * ( - (     c34 - c1345 )*tmp3*(u(2,i,j-1,k)**2)
     >              - ( r43*c34 - c1345 )*tmp3*(u(3,i,j-1,k)**2)
     >              - (     c34 - c1345 )*tmp3*(u(4,i,j-1,k)**2)
     >              - c1345*tmp2*u(5,i,j-1,k) )
               b(5,2,i,j) = - dt * ty2
     >          * ( - c2 * ( u(2,i,j-1,k)*u(3,i,j-1,k) ) * tmp2 )
     >          - dt * ty1
     >          * ( c34 - c1345 ) * tmp2 * u(2,i,j-1,k)
               b(5,3,i,j) = - dt * ty2
     >          * ( c1 * ( u(5,i,j-1,k) * tmp1 )
     >          - 0.50d+00 * c2 
     >          * ( (  u(2,i,j-1,k)*u(2,i,j-1,k)
     >               + 3.0d+00 * u(3,i,j-1,k)*u(3,i,j-1,k)
     >               + u(4,i,j-1,k)*u(4,i,j-1,k) ) * tmp2 ) )
     >          - dt * ty1
     >          * ( r43*c34 - c1345 ) * tmp2 * u(3,i,j-1,k)
               b(5,4,i,j) = - dt * ty2
     >          * ( - c2 * ( u(3,i,j-1,k)*u(4,i,j-1,k) ) * tmp2 )
     >          - dt * ty1 * ( c34 - c1345 ) * tmp2 * u(4,i,j-1,k)
               b(5,5,i,j) = - dt * ty2
     >          * ( c1 * ( u(3,i,j-1,k) * tmp1 ) )
     >          - dt * ty1 * c1345 * tmp1
     >          - dt * ty1 * dy5

c---------------------------------------------------------------------
c   form the third block sub-diagonal
c---------------------------------------------------------------------
               tmp1 = 1.0d+00 / u(1,i-1,j,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               c(1,1,i,j) = - dt * tx1 * dx1
               c(1,2,i,j) = - dt * tx2
               c(1,3,i,j) =   0.0d+00
               c(1,4,i,j) =   0.0d+00
               c(1,5,i,j) =   0.0d+00

               c(2,1,i,j) = - dt * tx2
     >          * ( - ( u(2,i-1,j,k) * tmp1 ) ** 2
     >     + c2 * 0.50d+00 * (  u(2,i-1,j,k) * u(2,i-1,j,k)
     >                        + u(3,i-1,j,k) * u(3,i-1,j,k)
     >                        + u(4,i-1,j,k) * u(4,i-1,j,k) ) * tmp2 )
     >          - dt * tx1 * ( - r43 * c34 * tmp2 * u(2,i-1,j,k) )
               c(2,2,i,j) = - dt * tx2
     >          * ( ( 2.0d+00 - c2 ) * ( u(2,i-1,j,k) * tmp1 ) )
     >          - dt * tx1 * ( r43 * c34 * tmp1 )
     >          - dt * tx1 * dx2
               c(2,3,i,j) = - dt * tx2
     >              * ( - c2 * ( u(3,i-1,j,k) * tmp1 ) )
               c(2,4,i,j) = - dt * tx2
     >              * ( - c2 * ( u(4,i-1,j,k) * tmp1 ) )
               c(2,5,i,j) = - dt * tx2 * c2 

               c(3,1,i,j) = - dt * tx2
     >              * ( - ( u(2,i-1,j,k) * u(3,i-1,j,k) ) * tmp2 )
     >         - dt * tx1 * ( - c34 * tmp2 * u(3,i-1,j,k) )
               c(3,2,i,j) = - dt * tx2 * ( u(3,i-1,j,k) * tmp1 )
               c(3,3,i,j) = - dt * tx2 * ( u(2,i-1,j,k) * tmp1 )
     >          - dt * tx1 * ( c34 * tmp1 )
     >          - dt * tx1 * dx3
               c(3,4,i,j) = 0.0d+00
               c(3,5,i,j) = 0.0d+00

               c(4,1,i,j) = - dt * tx2
     >          * ( - ( u(2,i-1,j,k)*u(4,i-1,j,k) ) * tmp2 )
     >          - dt * tx1 * ( - c34 * tmp2 * u(4,i-1,j,k) )
               c(4,2,i,j) = - dt * tx2 * ( u(4,i-1,j,k) * tmp1 )
               c(4,3,i,j) = 0.0d+00
               c(4,4,i,j) = - dt * tx2 * ( u(2,i-1,j,k) * tmp1 )
     >          - dt * tx1 * ( c34 * tmp1 )
     >          - dt * tx1 * dx4
               c(4,5,i,j) = 0.0d+00

               c(5,1,i,j) = - dt * tx2
     >          * ( ( c2 * (  u(2,i-1,j,k) * u(2,i-1,j,k)
     >                      + u(3,i-1,j,k) * u(3,i-1,j,k)
     >                      + u(4,i-1,j,k) * u(4,i-1,j,k) ) * tmp2
     >              - c1 * ( u(5,i-1,j,k) * tmp1 ) )
     >          * ( u(2,i-1,j,k) * tmp1 ) )
     >          - dt * tx1
     >          * ( - ( r43*c34 - c1345 ) * tmp3 * ( u(2,i-1,j,k)**2 )
     >              - (     c34 - c1345 ) * tmp3 * ( u(3,i-1,j,k)**2 )
     >              - (     c34 - c1345 ) * tmp3 * ( u(4,i-1,j,k)**2 )
     >              - c1345 * tmp2 * u(5,i-1,j,k) )
               c(5,2,i,j) = - dt * tx2
     >          * ( c1 * ( u(5,i-1,j,k) * tmp1 )
     >             - 0.50d+00 * c2
     >             * ( (  3.0d+00*u(2,i-1,j,k)*u(2,i-1,j,k)
     >                  + u(3,i-1,j,k)*u(3,i-1,j,k)
     >                  + u(4,i-1,j,k)*u(4,i-1,j,k) ) * tmp2 ) )
     >           - dt * tx1
     >           * ( r43*c34 - c1345 ) * tmp2 * u(2,i-1,j,k)
               c(5,3,i,j) = - dt * tx2
     >           * ( - c2 * ( u(3,i-1,j,k)*u(2,i-1,j,k) ) * tmp2 )
     >           - dt * tx1
     >           * (  c34 - c1345 ) * tmp2 * u(3,i-1,j,k)
               c(5,4,i,j) = - dt * tx2
     >           * ( - c2 * ( u(4,i-1,j,k)*u(2,i-1,j,k) ) * tmp2 )
     >           - dt * tx1
     >           * (  c34 - c1345 ) * tmp2 * u(4,i-1,j,k)
               c(5,5,i,j) = - dt * tx2
     >           * ( c1 * ( u(2,i-1,j,k) * tmp1 ) )
     >           - dt * tx1 * c1345 * tmp1
     >           - dt * tx1 * dx5

            end do
         end do

      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine jacu(k)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   compute the upper triangular part of the jacobian matrix
c---------------------------------------------------------------------


      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      integer k

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j
      double precision  r43
      double precision  c1345
      double precision  c34
      double precision  tmp1, tmp2, tmp3



      r43 = ( 4.0d+00 / 3.0d+00 )
      c1345 = c1 * c3 * c4 * c5
      c34 = c3 * c4

         do j = jst, jend
            do i = ist, iend

c---------------------------------------------------------------------
c   form the block daigonal
c---------------------------------------------------------------------
               tmp1 = 1.0d+00 / u(1,i,j,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               d(1,1,i,j) =  1.0d+00
     >                       + dt * 2.0d+00 * (   tx1 * dx1
     >                                          + ty1 * dy1
     >                                          + tz1 * dz1 )
               d(1,2,i,j) =  0.0d+00
               d(1,3,i,j) =  0.0d+00
               d(1,4,i,j) =  0.0d+00
               d(1,5,i,j) =  0.0d+00

               d(2,1,i,j) =  dt * 2.0d+00
     >          * (  tx1 * ( - r43 * c34 * tmp2 * u(2,i,j,k) )
     >             + ty1 * ( -       c34 * tmp2 * u(2,i,j,k) )
     >             + tz1 * ( -       c34 * tmp2 * u(2,i,j,k) ) )
               d(2,2,i,j) =  1.0d+00
     >          + dt * 2.0d+00 
     >          * (  tx1 * r43 * c34 * tmp1
     >             + ty1 *       c34 * tmp1
     >             + tz1 *       c34 * tmp1 )
     >          + dt * 2.0d+00 * (   tx1 * dx2
     >                             + ty1 * dy2
     >                             + tz1 * dz2  )
               d(2,3,i,j) = 0.0d+00
               d(2,4,i,j) = 0.0d+00
               d(2,5,i,j) = 0.0d+00

               d(3,1,i,j) = dt * 2.0d+00
     >      * (  tx1 * ( -       c34 * tmp2 * u(3,i,j,k) )
     >         + ty1 * ( - r43 * c34 * tmp2 * u(3,i,j,k) )
     >         + tz1 * ( -       c34 * tmp2 * u(3,i,j,k) ) )
               d(3,2,i,j) = 0.0d+00
               d(3,3,i,j) = 1.0d+00
     >         + dt * 2.0d+00
     >              * (  tx1 *       c34 * tmp1
     >                 + ty1 * r43 * c34 * tmp1
     >                 + tz1 *       c34 * tmp1 )
     >         + dt * 2.0d+00 * (  tx1 * dx3
     >                           + ty1 * dy3
     >                           + tz1 * dz3 )
               d(3,4,i,j) = 0.0d+00
               d(3,5,i,j) = 0.0d+00

               d(4,1,i,j) = dt * 2.0d+00
     >      * (  tx1 * ( -       c34 * tmp2 * u(4,i,j,k) )
     >         + ty1 * ( -       c34 * tmp2 * u(4,i,j,k) )
     >         + tz1 * ( - r43 * c34 * tmp2 * u(4,i,j,k) ) )
               d(4,2,i,j) = 0.0d+00
               d(4,3,i,j) = 0.0d+00
               d(4,4,i,j) = 1.0d+00
     >         + dt * 2.0d+00
     >              * (  tx1 *       c34 * tmp1
     >                 + ty1 *       c34 * tmp1
     >                 + tz1 * r43 * c34 * tmp1 )
     >         + dt * 2.0d+00 * (  tx1 * dx4
     >                           + ty1 * dy4
     >                           + tz1 * dz4 )
               d(4,5,i,j) = 0.0d+00

               d(5,1,i,j) = dt * 2.0d+00
     > * ( tx1 * ( - ( r43*c34 - c1345 ) * tmp3 * ( u(2,i,j,k) ** 2 )
     >             - ( c34 - c1345 ) * tmp3 * ( u(3,i,j,k) ** 2 )
     >             - ( c34 - c1345 ) * tmp3 * ( u(4,i,j,k) ** 2 )
     >             - ( c1345 ) * tmp2 * u(5,i,j,k) )
     >   + ty1 * ( - ( c34 - c1345 ) * tmp3 * ( u(2,i,j,k) ** 2 )
     >             - ( r43*c34 - c1345 ) * tmp3 * ( u(3,i,j,k) ** 2 )
     >             - ( c34 - c1345 ) * tmp3 * ( u(4,i,j,k) ** 2 )
     >             - ( c1345 ) * tmp2 * u(5,i,j,k) )
     >   + tz1 * ( - ( c34 - c1345 ) * tmp3 * ( u(2,i,j,k) ** 2 )
     >             - ( c34 - c1345 ) * tmp3 * ( u(3,i,j,k) ** 2 )
     >             - ( r43*c34 - c1345 ) * tmp3 * ( u(4,i,j,k) ** 2 )
     >             - ( c1345 ) * tmp2 * u(5,i,j,k) ) )
               d(5,2,i,j) = dt * 2.0d+00
     > * ( tx1 * ( r43*c34 - c1345 ) * tmp2 * u(2,i,j,k)
     >   + ty1 * (     c34 - c1345 ) * tmp2 * u(2,i,j,k)
     >   + tz1 * (     c34 - c1345 ) * tmp2 * u(2,i,j,k) )
               d(5,3,i,j) = dt * 2.0d+00
     > * ( tx1 * ( c34 - c1345 ) * tmp2 * u(3,i,j,k)
     >   + ty1 * ( r43*c34 -c1345 ) * tmp2 * u(3,i,j,k)
     >   + tz1 * ( c34 - c1345 ) * tmp2 * u(3,i,j,k) )
               d(5,4,i,j) = dt * 2.0d+00
     > * ( tx1 * ( c34 - c1345 ) * tmp2 * u(4,i,j,k)
     >   + ty1 * ( c34 - c1345 ) * tmp2 * u(4,i,j,k)
     >   + tz1 * ( r43*c34 - c1345 ) * tmp2 * u(4,i,j,k) )
               d(5,5,i,j) = 1.0d+00
     >   + dt * 2.0d+00 * ( tx1 * c1345 * tmp1
     >                    + ty1 * c1345 * tmp1
     >                    + tz1 * c1345 * tmp1 )
     >   + dt * 2.0d+00 * (  tx1 * dx5
     >                    +  ty1 * dy5
     >                    +  tz1 * dz5 )

c---------------------------------------------------------------------
c   form the first block sub-diagonal
c---------------------------------------------------------------------
               tmp1 = 1.0d+00 / u(1,i+1,j,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               a(1,1,i,j) = - dt * tx1 * dx1
               a(1,2,i,j) =   dt * tx2
               a(1,3,i,j) =   0.0d+00
               a(1,4,i,j) =   0.0d+00
               a(1,5,i,j) =   0.0d+00

               a(2,1,i,j) =  dt * tx2
     >          * ( - ( u(2,i+1,j,k) * tmp1 ) ** 2
     >     + c2 * 0.50d+00 * (  u(2,i+1,j,k) * u(2,i+1,j,k)
     >                        + u(3,i+1,j,k) * u(3,i+1,j,k)
     >                        + u(4,i+1,j,k) * u(4,i+1,j,k) ) * tmp2 )
     >          - dt * tx1 * ( - r43 * c34 * tmp2 * u(2,i+1,j,k) )
               a(2,2,i,j) =  dt * tx2
     >          * ( ( 2.0d+00 - c2 ) * ( u(2,i+1,j,k) * tmp1 ) )
     >          - dt * tx1 * ( r43 * c34 * tmp1 )
     >          - dt * tx1 * dx2
               a(2,3,i,j) =  dt * tx2
     >              * ( - c2 * ( u(3,i+1,j,k) * tmp1 ) )
               a(2,4,i,j) =  dt * tx2
     >              * ( - c2 * ( u(4,i+1,j,k) * tmp1 ) )
               a(2,5,i,j) =  dt * tx2 * c2 

               a(3,1,i,j) =  dt * tx2
     >              * ( - ( u(2,i+1,j,k) * u(3,i+1,j,k) ) * tmp2 )
     >         - dt * tx1 * ( - c34 * tmp2 * u(3,i+1,j,k) )
               a(3,2,i,j) =  dt * tx2 * ( u(3,i+1,j,k) * tmp1 )
               a(3,3,i,j) =  dt * tx2 * ( u(2,i+1,j,k) * tmp1 )
     >          - dt * tx1 * ( c34 * tmp1 )
     >          - dt * tx1 * dx3
               a(3,4,i,j) = 0.0d+00
               a(3,5,i,j) = 0.0d+00

               a(4,1,i,j) = dt * tx2
     >          * ( - ( u(2,i+1,j,k)*u(4,i+1,j,k) ) * tmp2 )
     >          - dt * tx1 * ( - c34 * tmp2 * u(4,i+1,j,k) )
               a(4,2,i,j) = dt * tx2 * ( u(4,i+1,j,k) * tmp1 )
               a(4,3,i,j) = 0.0d+00
               a(4,4,i,j) = dt * tx2 * ( u(2,i+1,j,k) * tmp1 )
     >          - dt * tx1 * ( c34 * tmp1 )
     >          - dt * tx1 * dx4
               a(4,5,i,j) = 0.0d+00

               a(5,1,i,j) = dt * tx2
     >          * ( ( c2 * (  u(2,i+1,j,k) * u(2,i+1,j,k)
     >                      + u(3,i+1,j,k) * u(3,i+1,j,k)
     >                      + u(4,i+1,j,k) * u(4,i+1,j,k) ) * tmp2
     >              - c1 * ( u(5,i+1,j,k) * tmp1 ) )
     >          * ( u(2,i+1,j,k) * tmp1 ) )
     >          - dt * tx1
     >          * ( - ( r43*c34 - c1345 ) * tmp3 * ( u(2,i+1,j,k)**2 )
     >              - (     c34 - c1345 ) * tmp3 * ( u(3,i+1,j,k)**2 )
     >              - (     c34 - c1345 ) * tmp3 * ( u(4,i+1,j,k)**2 )
     >              - c1345 * tmp2 * u(5,i+1,j,k) )
               a(5,2,i,j) = dt * tx2
     >          * ( c1 * ( u(5,i+1,j,k) * tmp1 )
     >             - 0.50d+00 * c2
     >             * ( (  3.0d+00*u(2,i+1,j,k)*u(2,i+1,j,k)
     >                  + u(3,i+1,j,k)*u(3,i+1,j,k)
     >                  + u(4,i+1,j,k)*u(4,i+1,j,k) ) * tmp2 ) )
     >           - dt * tx1
     >           * ( r43*c34 - c1345 ) * tmp2 * u(2,i+1,j,k)
               a(5,3,i,j) = dt * tx2
     >           * ( - c2 * ( u(3,i+1,j,k)*u(2,i+1,j,k) ) * tmp2 )
     >           - dt * tx1
     >           * (  c34 - c1345 ) * tmp2 * u(3,i+1,j,k)
               a(5,4,i,j) = dt * tx2
     >           * ( - c2 * ( u(4,i+1,j,k)*u(2,i+1,j,k) ) * tmp2 )
     >           - dt * tx1
     >           * (  c34 - c1345 ) * tmp2 * u(4,i+1,j,k)
               a(5,5,i,j) = dt * tx2
     >           * ( c1 * ( u(2,i+1,j,k) * tmp1 ) )
     >           - dt * tx1 * c1345 * tmp1
     >           - dt * tx1 * dx5

c---------------------------------------------------------------------
c   form the second block sub-diagonal
c---------------------------------------------------------------------
               tmp1 = 1.0d+00 / u(1,i,j+1,k)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               b(1,1,i,j) = - dt * ty1 * dy1
               b(1,2,i,j) =   0.0d+00
               b(1,3,i,j) =  dt * ty2
               b(1,4,i,j) =   0.0d+00
               b(1,5,i,j) =   0.0d+00

               b(2,1,i,j) =  dt * ty2
     >           * ( - ( u(2,i,j+1,k)*u(3,i,j+1,k) ) * tmp2 )
     >           - dt * ty1 * ( - c34 * tmp2 * u(2,i,j+1,k) )
               b(2,2,i,j) =  dt * ty2 * ( u(3,i,j+1,k) * tmp1 )
     >          - dt * ty1 * ( c34 * tmp1 )
     >          - dt * ty1 * dy2
               b(2,3,i,j) =  dt * ty2 * ( u(2,i,j+1,k) * tmp1 )
               b(2,4,i,j) = 0.0d+00
               b(2,5,i,j) = 0.0d+00

               b(3,1,i,j) =  dt * ty2
     >           * ( - ( u(3,i,j+1,k) * tmp1 ) ** 2
     >      + 0.50d+00 * c2 * ( (  u(2,i,j+1,k) * u(2,i,j+1,k)
     >                           + u(3,i,j+1,k) * u(3,i,j+1,k)
     >                           + u(4,i,j+1,k) * u(4,i,j+1,k) )
     >                          * tmp2 ) )
     >       - dt * ty1 * ( - r43 * c34 * tmp2 * u(3,i,j+1,k) )
               b(3,2,i,j) =  dt * ty2
     >                   * ( - c2 * ( u(2,i,j+1,k) * tmp1 ) )
               b(3,3,i,j) =  dt * ty2 * ( ( 2.0d+00 - c2 )
     >                   * ( u(3,i,j+1,k) * tmp1 ) )
     >       - dt * ty1 * ( r43 * c34 * tmp1 )
     >       - dt * ty1 * dy3
               b(3,4,i,j) =  dt * ty2
     >                   * ( - c2 * ( u(4,i,j+1,k) * tmp1 ) )
               b(3,5,i,j) =  dt * ty2 * c2

               b(4,1,i,j) =  dt * ty2
     >              * ( - ( u(3,i,j+1,k)*u(4,i,j+1,k) ) * tmp2 )
     >       - dt * ty1 * ( - c34 * tmp2 * u(4,i,j+1,k) )
               b(4,2,i,j) = 0.0d+00
               b(4,3,i,j) =  dt * ty2 * ( u(4,i,j+1,k) * tmp1 )
               b(4,4,i,j) =  dt * ty2 * ( u(3,i,j+1,k) * tmp1 )
     >                        - dt * ty1 * ( c34 * tmp1 )
     >                        - dt * ty1 * dy4
               b(4,5,i,j) = 0.0d+00

               b(5,1,i,j) =  dt * ty2
     >          * ( ( c2 * (  u(2,i,j+1,k) * u(2,i,j+1,k)
     >                      + u(3,i,j+1,k) * u(3,i,j+1,k)
     >                      + u(4,i,j+1,k) * u(4,i,j+1,k) ) * tmp2
     >               - c1 * ( u(5,i,j+1,k) * tmp1 ) )
     >          * ( u(3,i,j+1,k) * tmp1 ) )
     >          - dt * ty1
     >          * ( - (     c34 - c1345 )*tmp3*(u(2,i,j+1,k)**2)
     >              - ( r43*c34 - c1345 )*tmp3*(u(3,i,j+1,k)**2)
     >              - (     c34 - c1345 )*tmp3*(u(4,i,j+1,k)**2)
     >              - c1345*tmp2*u(5,i,j+1,k) )
               b(5,2,i,j) =  dt * ty2
     >          * ( - c2 * ( u(2,i,j+1,k)*u(3,i,j+1,k) ) * tmp2 )
     >          - dt * ty1
     >          * ( c34 - c1345 ) * tmp2 * u(2,i,j+1,k)
               b(5,3,i,j) =  dt * ty2
     >          * ( c1 * ( u(5,i,j+1,k) * tmp1 )
     >          - 0.50d+00 * c2 
     >          * ( (  u(2,i,j+1,k)*u(2,i,j+1,k)
     >               + 3.0d+00 * u(3,i,j+1,k)*u(3,i,j+1,k)
     >               + u(4,i,j+1,k)*u(4,i,j+1,k) ) * tmp2 ) )
     >          - dt * ty1
     >          * ( r43*c34 - c1345 ) * tmp2 * u(3,i,j+1,k)
               b(5,4,i,j) =  dt * ty2
     >          * ( - c2 * ( u(3,i,j+1,k)*u(4,i,j+1,k) ) * tmp2 )
     >          - dt * ty1 * ( c34 - c1345 ) * tmp2 * u(4,i,j+1,k)
               b(5,5,i,j) =  dt * ty2
     >          * ( c1 * ( u(3,i,j+1,k) * tmp1 ) )
     >          - dt * ty1 * c1345 * tmp1
     >          - dt * ty1 * dy5

c---------------------------------------------------------------------
c   form the third block sub-diagonal
c---------------------------------------------------------------------
               tmp1 = 1.0d+00 / u(1,i,j,k+1)
               tmp2 = tmp1 * tmp1
               tmp3 = tmp1 * tmp2

               c(1,1,i,j) = - dt * tz1 * dz1
               c(1,2,i,j) =   0.0d+00
               c(1,3,i,j) =   0.0d+00
               c(1,4,i,j) = dt * tz2
               c(1,5,i,j) =   0.0d+00

               c(2,1,i,j) = dt * tz2
     >           * ( - ( u(2,i,j,k+1)*u(4,i,j,k+1) ) * tmp2 )
     >           - dt * tz1 * ( - c34 * tmp2 * u(2,i,j,k+1) )
               c(2,2,i,j) = dt * tz2 * ( u(4,i,j,k+1) * tmp1 )
     >           - dt * tz1 * c34 * tmp1
     >           - dt * tz1 * dz2 
               c(2,3,i,j) = 0.0d+00
               c(2,4,i,j) = dt * tz2 * ( u(2,i,j,k+1) * tmp1 )
               c(2,5,i,j) = 0.0d+00

               c(3,1,i,j) = dt * tz2
     >           * ( - ( u(3,i,j,k+1)*u(4,i,j,k+1) ) * tmp2 )
     >           - dt * tz1 * ( - c34 * tmp2 * u(3,i,j,k+1) )
               c(3,2,i,j) = 0.0d+00
               c(3,3,i,j) = dt * tz2 * ( u(4,i,j,k+1) * tmp1 )
     >           - dt * tz1 * ( c34 * tmp1 )
     >           - dt * tz1 * dz3
               c(3,4,i,j) = dt * tz2 * ( u(3,i,j,k+1) * tmp1 )
               c(3,5,i,j) = 0.0d+00

               c(4,1,i,j) = dt * tz2
     >        * ( - ( u(4,i,j,k+1) * tmp1 ) ** 2
     >            + 0.50d+00 * c2
     >            * ( ( u(2,i,j,k+1) * u(2,i,j,k+1)
     >                + u(3,i,j,k+1) * u(3,i,j,k+1)
     >                + u(4,i,j,k+1) * u(4,i,j,k+1) ) * tmp2 ) )
     >        - dt * tz1 * ( - r43 * c34 * tmp2 * u(4,i,j,k+1) )
               c(4,2,i,j) = dt * tz2
     >             * ( - c2 * ( u(2,i,j,k+1) * tmp1 ) )
               c(4,3,i,j) = dt * tz2
     >             * ( - c2 * ( u(3,i,j,k+1) * tmp1 ) )
               c(4,4,i,j) = dt * tz2 * ( 2.0d+00 - c2 )
     >             * ( u(4,i,j,k+1) * tmp1 )
     >             - dt * tz1 * ( r43 * c34 * tmp1 )
     >             - dt * tz1 * dz4
               c(4,5,i,j) = dt * tz2 * c2

               c(5,1,i,j) = dt * tz2
     >     * ( ( c2 * (  u(2,i,j,k+1) * u(2,i,j,k+1)
     >                 + u(3,i,j,k+1) * u(3,i,j,k+1)
     >                 + u(4,i,j,k+1) * u(4,i,j,k+1) ) * tmp2
     >       - c1 * ( u(5,i,j,k+1) * tmp1 ) )
     >            * ( u(4,i,j,k+1) * tmp1 ) )
     >       - dt * tz1
     >       * ( - ( c34 - c1345 ) * tmp3 * (u(2,i,j,k+1)**2)
     >           - ( c34 - c1345 ) * tmp3 * (u(3,i,j,k+1)**2)
     >           - ( r43*c34 - c1345 )* tmp3 * (u(4,i,j,k+1)**2)
     >          - c1345 * tmp2 * u(5,i,j,k+1) )
               c(5,2,i,j) = dt * tz2
     >       * ( - c2 * ( u(2,i,j,k+1)*u(4,i,j,k+1) ) * tmp2 )
     >       - dt * tz1 * ( c34 - c1345 ) * tmp2 * u(2,i,j,k+1)
               c(5,3,i,j) = dt * tz2
     >       * ( - c2 * ( u(3,i,j,k+1)*u(4,i,j,k+1) ) * tmp2 )
     >       - dt * tz1 * ( c34 - c1345 ) * tmp2 * u(3,i,j,k+1)
               c(5,4,i,j) = dt * tz2
     >       * ( c1 * ( u(5,i,j,k+1) * tmp1 )
     >       - 0.50d+00 * c2
     >       * ( (  u(2,i,j,k+1)*u(2,i,j,k+1)
     >            + u(3,i,j,k+1)*u(3,i,j,k+1)
     >            + 3.0d+00*u(4,i,j,k+1)*u(4,i,j,k+1) ) * tmp2 ) )
     >       - dt * tz1 * ( r43*c34 - c1345 ) * tmp2 * u(4,i,j,k+1)
               c(5,5,i,j) = dt * tz2
     >       * ( c1 * ( u(4,i,j,k+1) * tmp1 ) )
     >       - dt * tz1 * c1345 * tmp1
     >       - dt * tz1 * dz5

            end do
         end do

      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------
      subroutine l2norm ( ldx, ldy, ldz, 
     >                    nx0, ny0, nz0,
     >                    ist, iend, 
     >                    jst, jend,
     >                    v, sum )
c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   to compute the l2-norm of vector v.
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      integer ldx, ldy, ldz
      integer nx0, ny0, nz0
      integer ist, iend
      integer jst, jend
      double precision  v(5,-1:ldx+2,-1:ldy+2,*), sum(5)

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k, m
      double precision  dummy(5)

      integer IERROR


      do m = 1, 5
         dummy(m) = 0.0d+00
      end do

      do k = 2, nz0-1
         do j = jst, jend
            do i = ist, iend
               do m = 1, 5
                  dummy(m) = dummy(m) + v(m,i,j,k) * v(m,i,j,k)
               end do
            end do
         end do
      end do

c---------------------------------------------------------------------
c   compute the global sum of individual contributions to dot product.
c---------------------------------------------------------------------
      call MPI_ALLREDUCE( dummy,
     >                    sum,
     >                    5,
     >                    dp_type,
     >                    MPI_SUM,
     >                    MPI_COMM_WORLD,
     >                    IERROR )

      do m = 1, 5
         sum(m) = sqrt ( sum(m) / ( (nx0-2)*(ny0-2)*(nz0-2) ) )
      end do

      return
      end
!-------------------------------------------------------------------------!
!                                                                         !
!        N  A  S     P A R A L L E L     B E N C H M A R K S  3.1         !
!                                                                         !
!                                   L U                                   !
!                                                                         !
!-------------------------------------------------------------------------!
!                                                                         !
!    This benchmark is part of the NAS Parallel Benchmark 3.1 suite.      !
!    It is described in NAS Technical Reports 95-020 and 02-007           !
!                                                                         !
!    Permission to use, copy, distribute and modify this software         !
!    for any purpose with or without fee is hereby granted.  We           !
!    request, however, that all derived work reference the NAS            !
!    Parallel Benchmarks 3.1. This software is provided "as is"           !
!    without express or implied warranty.                                 !
!                                                                         !
!    Information on NPB 3.1, including the technical report, the          !
!    original specifications, source code, results and information        !
!    on how to submit new results, is available at:                       !
!                                                                         !
!           http://www.nas.nasa.gov/Software/NPB/                         !
!                                                                         !
!    Send comments or suggestions to  npb@nas.nasa.gov                    !
!                                                                         !
!          NAS Parallel Benchmarks Group                                  !
!          NASA Ames Research Center                                      !
!          Mail Stop: T27A-1                                              !
!          Moffett Field, CA   94035-1000                                 !
!                                                                         !
!          E-mail:  npb@nas.nasa.gov                                      !
!          Fax:     (650) 604-3957                                        !
!                                                                         !
!-------------------------------------------------------------------------!

c---------------------------------------------------------------------
c
c Authors: S. Weeratunga
c          V. Venkatakrishnan
c          E. Barszcz
c          M. Yarrow
c
c---------------------------------------------------------------------

c---------------------------------------------------------------------
      program applu
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   driver for the performance evaluation of the solver for
c   five coupled parabolic/elliptic partial differential equations.
c
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'
      character class
      logical verified
      double precision mflops
      integer ierr

c---------------------------------------------------------------------
c   initialize communications
c---------------------------------------------------------------------
      call init_comm()

c---------------------------------------------------------------------
c   read input data
c---------------------------------------------------------------------
      call read_input()

c---------------------------------------------------------------------
c   set up processor grid
c---------------------------------------------------------------------
      call proc_grid()

c---------------------------------------------------------------------
c   determine the neighbors
c---------------------------------------------------------------------
      call neighbors()

c---------------------------------------------------------------------
c   set up sub-domain sizes
c---------------------------------------------------------------------
      call subdomain()

c---------------------------------------------------------------------
c   set up coefficients
c---------------------------------------------------------------------
      call setcoeff()

c---------------------------------------------------------------------
c   set the masks required for comm
c---------------------------------------------------------------------
      call sethyper()

c---------------------------------------------------------------------
c   set the boundary values for dependent variables
c---------------------------------------------------------------------
      call setbv()

c---------------------------------------------------------------------
c   set the initial values for dependent variables
c---------------------------------------------------------------------
      call setiv()

c---------------------------------------------------------------------
c   compute the forcing term based on prescribed exact solution
c---------------------------------------------------------------------
      call erhs()

c---------------------------------------------------------------------
c   perform one SSOR iteration to touch all data and program pages 
c---------------------------------------------------------------------
      call ssor(1)

c---------------------------------------------------------------------
c   reset the boundary and initial values
c---------------------------------------------------------------------
      call setbv()
      call setiv()

c---------------------------------------------------------------------
c   perform the SSOR iterations
c---------------------------------------------------------------------
      call ssor(itmax)

c---------------------------------------------------------------------
c   compute the solution error
c---------------------------------------------------------------------
      call error()

c---------------------------------------------------------------------
c   compute the surface integral
c---------------------------------------------------------------------
      call pintgr()

c---------------------------------------------------------------------
c   verification test
c---------------------------------------------------------------------
      IF (id.eq.0) THEN
         call verify ( rsdnm, errnm, frc, class, verified )
         mflops = float(itmax)*(1984.77*float( nx0 )
     >        *float( ny0 )
     >        *float( nz0 )
     >        -10923.3*(float( nx0+ny0+nz0 )/3.)**2 
     >        +27770.9* float( nx0+ny0+nz0 )/3.
     >        -144010.)
     >        / (maxtime*1000000.)

         call print_results('LU', class, nx0,
     >     ny0, nz0, itmax, nnodes_compiled,
     >     num, maxtime, mflops, '          floating point', verified, 
     >     npbversion, compiletime, cs1, cs2, cs3, cs4, cs5, cs6, 
     >     '(none)')

      END IF

      call mpi_finalize(ierr)
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine neighbors ()

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c     figure out the neighbors and their wrap numbers for each processor
c---------------------------------------------------------------------

        south = -1
        east  = -1
        north = -1
        west  = -1

      if (row.gt.1) then
              north = id -1
      else
              north = -1
      end if

      if (row.lt.xdim) then
              south = id + 1
      else
              south = -1
      end if

      if (col.gt.1) then
              west = id- xdim
      else
              west = -1
      end if

      if (col.lt.ydim) then
              east = id + xdim
      else 
              east = -1
      end if

      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      integer function nodedim(num)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c  compute the exponent where num = 2**nodedim
c  NOTE: assumes a power-of-two number of nodes
c
c---------------------------------------------------------------------

      implicit none

c---------------------------------------------------------------------
c  input parameters
c---------------------------------------------------------------------
      integer num

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      double precision fnum


      fnum = dble(num)
      nodedim = log(fnum)/log(2.0d+0) + 0.00001

      return
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine pintgr

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k
      integer ibeg, ifin, ifin1
      integer jbeg, jfin, jfin1
      integer iglob, iglob1, iglob2
      integer jglob, jglob1, jglob2
      integer ind1, ind2
      double precision  phi1(0:isiz2+1,0:isiz3+1),
     >                  phi2(0:isiz2+1,0:isiz3+1)
      double precision  frc1, frc2, frc3
      double precision  dummy

      integer IERROR


c---------------------------------------------------------------------
c   set up the sub-domains for integeration in each processor
c---------------------------------------------------------------------
      ibeg = nx + 1
      ifin = 0
      iglob1 = ipt + 1
      iglob2 = ipt + nx
      if (iglob1.ge.ii1.and.iglob2.lt.ii2+nx) ibeg = 1
      if (iglob1.gt.ii1-nx.and.iglob2.le.ii2) ifin = nx
      if (ii1.ge.iglob1.and.ii1.le.iglob2) ibeg = ii1 - ipt
      if (ii2.ge.iglob1.and.ii2.le.iglob2) ifin = ii2 - ipt
      jbeg = ny + 1
      jfin = 0
      jglob1 = jpt + 1
      jglob2 = jpt + ny
      if (jglob1.ge.ji1.and.jglob2.lt.ji2+ny) jbeg = 1
      if (jglob1.gt.ji1-ny.and.jglob2.le.ji2) jfin = ny
      if (ji1.ge.jglob1.and.ji1.le.jglob2) jbeg = ji1 - jpt
      if (ji2.ge.jglob1.and.ji2.le.jglob2) jfin = ji2 - jpt
      ifin1 = ifin
      jfin1 = jfin
      if (ipt + ifin1.eq.ii2) ifin1 = ifin -1
      if (jpt + jfin1.eq.ji2) jfin1 = jfin -1

c---------------------------------------------------------------------
c   initialize
c---------------------------------------------------------------------
      do i = 0,isiz2+1
        do k = 0,isiz3+1
          phi1(i,k) = 0.
          phi2(i,k) = 0.
        end do
      end do

      do j = jbeg,jfin
         jglob = jpt + j
         do i = ibeg,ifin
            iglob = ipt + i

            k = ki1

            phi1(i,j) = c2*(  u(5,i,j,k)
     >           - 0.50d+00 * (  u(2,i,j,k) ** 2
     >                         + u(3,i,j,k) ** 2
     >                         + u(4,i,j,k) ** 2 )
     >                        / u(1,i,j,k) )

            k = ki2

            phi2(i,j) = c2*(  u(5,i,j,k)
     >           - 0.50d+00 * (  u(2,i,j,k) ** 2
     >                         + u(3,i,j,k) ** 2
     >                         + u(4,i,j,k) ** 2 )
     >                        / u(1,i,j,k) )
         end do
      end do

c---------------------------------------------------------------------
c  communicate in i and j directions
c---------------------------------------------------------------------
      call exchange_4(phi1,phi2,ibeg,ifin1,jbeg,jfin1)

      frc1 = 0.0d+00

      do j = jbeg,jfin1
         do i = ibeg, ifin1
            frc1 = frc1 + (  phi1(i,j)
     >                     + phi1(i+1,j)
     >                     + phi1(i,j+1)
     >                     + phi1(i+1,j+1)
     >                     + phi2(i,j)
     >                     + phi2(i+1,j)
     >                     + phi2(i,j+1)
     >                     + phi2(i+1,j+1) )
         end do
      end do

c---------------------------------------------------------------------
c  compute the global sum of individual contributions to frc1
c---------------------------------------------------------------------
      dummy = frc1
      call MPI_ALLREDUCE( dummy,
     >                    frc1,
     >                    1,
     >                    dp_type,
     >                    MPI_SUM,
     >                    MPI_COMM_WORLD,
     >                    IERROR )

      frc1 = dxi * deta * frc1

c---------------------------------------------------------------------
c   initialize
c---------------------------------------------------------------------
      do i = 0,isiz2+1
        do k = 0,isiz3+1
          phi1(i,k) = 0.
          phi2(i,k) = 0.
        end do
      end do
      jglob = jpt + jbeg
      ind1 = 0
      if (jglob.eq.ji1) then
        ind1 = 1
        do k = ki1, ki2
           do i = ibeg, ifin
              iglob = ipt + i
              phi1(i,k) = c2*(  u(5,i,jbeg,k)
     >             - 0.50d+00 * (  u(2,i,jbeg,k) ** 2
     >                           + u(3,i,jbeg,k) ** 2
     >                           + u(4,i,jbeg,k) ** 2 )
     >                          / u(1,i,jbeg,k) )
           end do
        end do
      end if

      jglob = jpt + jfin
      ind2 = 0
      if (jglob.eq.ji2) then
        ind2 = 1
        do k = ki1, ki2
           do i = ibeg, ifin
              iglob = ipt + i
              phi2(i,k) = c2*(  u(5,i,jfin,k)
     >             - 0.50d+00 * (  u(2,i,jfin,k) ** 2
     >                           + u(3,i,jfin,k) ** 2
     >                           + u(4,i,jfin,k) ** 2 )
     >                          / u(1,i,jfin,k) )
           end do
        end do
      end if

c---------------------------------------------------------------------
c  communicate in i direction
c---------------------------------------------------------------------
      if (ind1.eq.1) then
        call exchange_5(phi1,ibeg,ifin1)
      end if
      if (ind2.eq.1) then
        call exchange_5 (phi2,ibeg,ifin1)
      end if

      frc2 = 0.0d+00
      do k = ki1, ki2-1
         do i = ibeg, ifin1
            frc2 = frc2 + (  phi1(i,k)
     >                     + phi1(i+1,k)
     >                     + phi1(i,k+1)
     >                     + phi1(i+1,k+1)
     >                     + phi2(i,k)
     >                     + phi2(i+1,k)
     >                     + phi2(i,k+1)
     >                     + phi2(i+1,k+1) )
         end do
      end do

c---------------------------------------------------------------------
c  compute the global sum of individual contributions to frc2
c---------------------------------------------------------------------
      dummy = frc2
      call MPI_ALLREDUCE( dummy,
     >                    frc2,
     >                    1,
     >                    dp_type,
     >                    MPI_SUM,
     >                    MPI_COMM_WORLD,
     >                    IERROR )

      frc2 = dxi * dzeta * frc2

c---------------------------------------------------------------------
c   initialize
c---------------------------------------------------------------------
      do i = 0,isiz2+1
        do k = 0,isiz3+1
          phi1(i,k) = 0.
          phi2(i,k) = 0.
        end do
      end do
      iglob = ipt + ibeg
      ind1 = 0
      if (iglob.eq.ii1) then
        ind1 = 1
        do k = ki1, ki2
           do j = jbeg, jfin
              jglob = jpt + j
              phi1(j,k) = c2*(  u(5,ibeg,j,k)
     >             - 0.50d+00 * (  u(2,ibeg,j,k) ** 2
     >                           + u(3,ibeg,j,k) ** 2
     >                           + u(4,ibeg,j,k) ** 2 )
     >                          / u(1,ibeg,j,k) )
           end do
        end do
      end if

      iglob = ipt + ifin
      ind2 = 0
      if (iglob.eq.ii2) then
        ind2 = 1
        do k = ki1, ki2
           do j = jbeg, jfin
              jglob = jpt + j
              phi2(j,k) = c2*(  u(5,ifin,j,k)
     >             - 0.50d+00 * (  u(2,ifin,j,k) ** 2
     >                           + u(3,ifin,j,k) ** 2
     >                           + u(4,ifin,j,k) ** 2 )
     >                          / u(1,ifin,j,k) )
           end do
        end do
      end if

c---------------------------------------------------------------------
c  communicate in j direction
c---------------------------------------------------------------------
      if (ind1.eq.1) then
        call exchange_6(phi1,jbeg,jfin1)
      end if
      if (ind2.eq.1) then
        call exchange_6(phi2,jbeg,jfin1)
      end if

      frc3 = 0.0d+00

      do k = ki1, ki2-1
         do j = jbeg, jfin1
            frc3 = frc3 + (  phi1(j,k)
     >                     + phi1(j+1,k)
     >                     + phi1(j,k+1)
     >                     + phi1(j+1,k+1)
     >                     + phi2(j,k)
     >                     + phi2(j+1,k)
     >                     + phi2(j,k+1)
     >                     + phi2(j+1,k+1) )
         end do
      end do

c---------------------------------------------------------------------
c  compute the global sum of individual contributions to frc3
c---------------------------------------------------------------------
      dummy = frc3
      call MPI_ALLREDUCE( dummy,
     >                    frc3,
     >                    1,
     >                    dp_type,
     >                    MPI_SUM,
     >                    MPI_COMM_WORLD,
     >                    IERROR )

      frc3 = deta * dzeta * frc3
      frc = 0.25d+00 * ( frc1 + frc2 + frc3 )
c      if (id.eq.0) write (*,1001) frc

      return

 1001 format (//5x,'surface integral = ',1pe12.5//)

      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine proc_grid

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   set up a two-d grid for processors: column-major ordering of unknowns
c   NOTE: assumes a power-of-two number of processors
c
c---------------------------------------------------------------------

      xdim   = 2**(ndim/2)
      if (mod(ndim,2).eq.1) xdim = xdim + xdim
      ydim   = num/xdim

      row    = mod(id,xdim) + 1
      col    = id/xdim + 1


      return
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine read_input

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

      integer IERROR, fstatus, nnodes


c---------------------------------------------------------------------
c    only root reads the input file
c    if input file does not exist, it uses defaults
c       ipr = 1 for detailed progress output
c       inorm = how often the norm is printed (once every inorm iterations)
c       itmax = number of pseudo time steps
c       dt = time step
c       omega 1 over-relaxation factor for SSOR
c       tolrsd = steady state residual tolerance levels
c       nx, ny, nz = number of grid points in x, y, z directions
c---------------------------------------------------------------------
      ROOT = 0
      if (id .eq. ROOT) then

         write(*, 1000)

         open (unit=3,file='inputlu.data',status='old',
     >         access='sequential',form='formatted', iostat=fstatus)
         if (fstatus .eq. 0) then

            write(*, *) ' Reading from input file inputlu.data'

            read (3,*)
            read (3,*)
            read (3,*) ipr, inorm
            read (3,*)
            read (3,*)
            read (3,*) itmax
            read (3,*)
            read (3,*)
            read (3,*) dt
            read (3,*)
            read (3,*)
            read (3,*) omega
            read (3,*)
            read (3,*)
            read (3,*) tolrsd(1),tolrsd(2),tolrsd(3),tolrsd(4),tolrsd(5)
            read (3,*)
            read (3,*)
            read (3,*) nx0, ny0, nz0
            close(3)
         else
            ipr = ipr_default
            inorm = inorm_default
            itmax = itmax_default
            dt = dt_default
            omega = omega_default
            tolrsd(1) = tolrsd1_def
            tolrsd(2) = tolrsd2_def
            tolrsd(3) = tolrsd3_def
            tolrsd(4) = tolrsd4_def
            tolrsd(5) = tolrsd5_def
            nx0 = isiz01
            ny0 = isiz02
            nz0 = isiz03
         endif

c---------------------------------------------------------------------
c   check problem size
c---------------------------------------------------------------------
         call MPI_COMM_SIZE(MPI_COMM_WORLD, nnodes, ierror)
         if (nnodes .ne. nnodes_compiled) then
            write (*, 2000) nnodes, nnodes_compiled
 2000       format (5x,'Warning: program is running on',i3,' processors'
     >             /5x,'but was compiled for ', i3)
         endif

         if ( ( nx0 .lt. 4 ) .or.
     >        ( ny0 .lt. 4 ) .or.
     >        ( nz0 .lt. 4 ) ) then

            write (*,2001)
 2001       format (5x,'PROBLEM SIZE IS TOO SMALL - ',
     >           /5x,'SET EACH OF NX, NY AND NZ AT LEAST EQUAL TO 5')
            CALL MPI_ABORT( MPI_COMM_WORLD, MPI_ERR_OTHER, IERROR )

         end if

         if ( ( nx0 .gt. isiz01 ) .or.
     >        ( ny0 .gt. isiz02 ) .or.
     >        ( nz0 .gt. isiz03 ) ) then

            write (*,2002)
 2002       format (5x,'PROBLEM SIZE IS TOO LARGE - ',
     >           /5x,'NX, NY AND NZ SHOULD BE LESS THAN OR EQUAL TO ',
     >           /5x,'ISIZ01, ISIZ02 AND ISIZ03 RESPECTIVELY')
            CALL MPI_ABORT( MPI_COMM_WORLD, MPI_ERR_OTHER, IERROR )

         end if


         write(*, 1001) nx0, ny0, nz0
         write(*, 1002) itmax
         write(*, 1003) nnodes

 1000 format(//, ' NAS Parallel Benchmarks 3.1 -- LU Benchmark',/)
 1001    format(' Size: ', i3, 'x', i3, 'x', i3)
 1002    format(' Iterations: ', i3)
 1003    format(' Number of processes: ', i5, /)
         


      end if

      call bcast_inputs

      return
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine rhs

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   compute the right hand sides
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k, m
      integer iex
      integer L1, L2
      integer ist1, iend1
      integer jst1, jend1
      double precision  q
      double precision  u21, u31, u41
      double precision  tmp
      double precision  u21i, u31i, u41i, u51i
      double precision  u21j, u31j, u41j, u51j
      double precision  u21k, u31k, u41k, u51k
      double precision  u21im1, u31im1, u41im1, u51im1
      double precision  u21jm1, u31jm1, u41jm1, u51jm1
      double precision  u21km1, u31km1, u41km1, u51km1

      do k = 1, nz
         do j = 1, ny
            do i = 1, nx
               do m = 1, 5
                  rsd(m,i,j,k) = - frct(m,i,j,k)
               end do
            end do
         end do
      end do

c---------------------------------------------------------------------
c   xi-direction flux differences
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   iex = flag : iex = 0  north/south communication
c              : iex = 1  east/west communication
c---------------------------------------------------------------------
      iex   = 0

c---------------------------------------------------------------------
c   communicate and receive/send two rows of data
c---------------------------------------------------------------------
      call exchange_3(u,iex)

      L1 = 0
      if (north.eq.-1) L1 = 1
      L2 = nx + 1
      if (south.eq.-1) L2 = nx

      do k = 2, nz - 1
         do j = jst, jend
            do i = L1, L2
               flux(1,i,j,k) = u(2,i,j,k)
               u21 = u(2,i,j,k) / u(1,i,j,k)

               q = 0.50d+00 * (  u(2,i,j,k) * u(2,i,j,k)
     >                         + u(3,i,j,k) * u(3,i,j,k)
     >                         + u(4,i,j,k) * u(4,i,j,k) )
     >                      / u(1,i,j,k)

               flux(2,i,j,k) = u(2,i,j,k) * u21 + c2 * 
     >                        ( u(5,i,j,k) - q )
               flux(3,i,j,k) = u(3,i,j,k) * u21
               flux(4,i,j,k) = u(4,i,j,k) * u21
               flux(5,i,j,k) = ( c1 * u(5,i,j,k) - c2 * q ) * u21
            end do
         end do 
      end do 

      do k = 2, nz - 1
         do j = jst, jend
            do i = ist, iend
               do m = 1, 5
                  rsd(m,i,j,k) =  rsd(m,i,j,k)
     >                 - tx2 * ( flux(m,i+1,j,k) - flux(m,i-1,j,k) )
               end do
            end do

            L2 = nx + 1
            if (south.eq.-1) L2 = nx

            do i = ist, L2
               tmp = 1.0d+00 / u(1,i,j,k)

               u21i = tmp * u(2,i,j,k)
               u31i = tmp * u(3,i,j,k)
               u41i = tmp * u(4,i,j,k)
               u51i = tmp * u(5,i,j,k)

               tmp = 1.0d+00 / u(1,i-1,j,k)

               u21im1 = tmp * u(2,i-1,j,k)
               u31im1 = tmp * u(3,i-1,j,k)
               u41im1 = tmp * u(4,i-1,j,k)
               u51im1 = tmp * u(5,i-1,j,k)

               flux(2,i,j,k) = (4.0d+00/3.0d+00) * tx3 * (u21i-u21im1)
               flux(3,i,j,k) = tx3 * ( u31i - u31im1 )
               flux(4,i,j,k) = tx3 * ( u41i - u41im1 )
               flux(5,i,j,k) = 0.50d+00 * ( 1.0d+00 - c1*c5 )
     >              * tx3 * ( ( u21i  **2 + u31i  **2 + u41i  **2 )
     >                      - ( u21im1**2 + u31im1**2 + u41im1**2 ) )
     >              + (1.0d+00/6.0d+00)
     >              * tx3 * ( u21i**2 - u21im1**2 )
     >              + c1 * c5 * tx3 * ( u51i - u51im1 )
            end do

            do i = ist, iend
               rsd(1,i,j,k) = rsd(1,i,j,k)
     >              + dx1 * tx1 * (            u(1,i-1,j,k)
     >                             - 2.0d+00 * u(1,i,j,k)
     >                             +           u(1,i+1,j,k) )
               rsd(2,i,j,k) = rsd(2,i,j,k)
     >          + tx3 * c3 * c4 * ( flux(2,i+1,j,k) - flux(2,i,j,k) )
     >              + dx2 * tx1 * (            u(2,i-1,j,k)
     >                             - 2.0d+00 * u(2,i,j,k)
     >                             +           u(2,i+1,j,k) )
               rsd(3,i,j,k) = rsd(3,i,j,k)
     >          + tx3 * c3 * c4 * ( flux(3,i+1,j,k) - flux(3,i,j,k) )
     >              + dx3 * tx1 * (            u(3,i-1,j,k)
     >                             - 2.0d+00 * u(3,i,j,k)
     >                             +           u(3,i+1,j,k) )
               rsd(4,i,j,k) = rsd(4,i,j,k)
     >          + tx3 * c3 * c4 * ( flux(4,i+1,j,k) - flux(4,i,j,k) )
     >              + dx4 * tx1 * (            u(4,i-1,j,k)
     >                             - 2.0d+00 * u(4,i,j,k)
     >                             +           u(4,i+1,j,k) )
               rsd(5,i,j,k) = rsd(5,i,j,k)
     >          + tx3 * c3 * c4 * ( flux(5,i+1,j,k) - flux(5,i,j,k) )
     >              + dx5 * tx1 * (            u(5,i-1,j,k)
     >                             - 2.0d+00 * u(5,i,j,k)
     >                             +           u(5,i+1,j,k) )
            end do

c---------------------------------------------------------------------
c   Fourth-order dissipation
c---------------------------------------------------------------------
            IF (north.eq.-1) then
             do m = 1, 5
               rsd(m,2,j,k) = rsd(m,2,j,k)
     >           - dssp * ( + 5.0d+00 * u(m,2,j,k)
     >                      - 4.0d+00 * u(m,3,j,k)
     >                      +           u(m,4,j,k) )
               rsd(m,3,j,k) = rsd(m,3,j,k)
     >           - dssp * ( - 4.0d+00 * u(m,2,j,k)
     >                      + 6.0d+00 * u(m,3,j,k)
     >                      - 4.0d+00 * u(m,4,j,k)
     >                      +           u(m,5,j,k) )
             end do
            END IF

            ist1 = 1
            iend1 = nx
            if (north.eq.-1) ist1 = 4
            if (south.eq.-1) iend1 = nx - 3

            do i = ist1,iend1
               do m = 1, 5
                  rsd(m,i,j,k) = rsd(m,i,j,k)
     >              - dssp * (            u(m,i-2,j,k)
     >                        - 4.0d+00 * u(m,i-1,j,k)
     >                        + 6.0d+00 * u(m,i,j,k)
     >                        - 4.0d+00 * u(m,i+1,j,k)
     >                        +           u(m,i+2,j,k) )
               end do
            end do

            IF (south.eq.-1) then

             do m = 1, 5
               rsd(m,nx-2,j,k) = rsd(m,nx-2,j,k)
     >           - dssp * (             u(m,nx-4,j,k)
     >                      - 4.0d+00 * u(m,nx-3,j,k)
     >                      + 6.0d+00 * u(m,nx-2,j,k)
     >                      - 4.0d+00 * u(m,nx-1,j,k)  )
               rsd(m,nx-1,j,k) = rsd(m,nx-1,j,k)
     >           - dssp * (             u(m,nx-3,j,k)
     >                      - 4.0d+00 * u(m,nx-2,j,k)
     >                      + 5.0d+00 * u(m,nx-1,j,k) )
             end do
            END IF

         end do
      end do

c---------------------------------------------------------------------
c   eta-direction flux differences
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   iex = flag : iex = 0  north/south communication
c---------------------------------------------------------------------
      iex   = 1

c---------------------------------------------------------------------
c   communicate and receive/send two rows of data
c---------------------------------------------------------------------
      call exchange_3(u,iex)

      L1 = 0
      if (west.eq.-1) L1 = 1
      L2 = ny + 1
      if (east.eq.-1) L2 = ny

      do k = 2, nz - 1
         do i = ist, iend
            do j = L1, L2
               flux(1,i,j,k) = u(3,i,j,k)
               u31 = u(3,i,j,k) / u(1,i,j,k)

               q = 0.50d+00 * (  u(2,i,j,k) * u(2,i,j,k)
     >                         + u(3,i,j,k) * u(3,i,j,k)
     >                         + u(4,i,j,k) * u(4,i,j,k) )
     >                      / u(1,i,j,k)

               flux(2,i,j,k) = u(2,i,j,k) * u31 
               flux(3,i,j,k) = u(3,i,j,k) * u31 + c2 * (u(5,i,j,k)-q)
               flux(4,i,j,k) = u(4,i,j,k) * u31
               flux(5,i,j,k) = ( c1 * u(5,i,j,k) - c2 * q ) * u31
            end do
         end do
      end do

      do k = 2, nz - 1
         do i = ist, iend
            do j = jst, jend
               do m = 1, 5
                  rsd(m,i,j,k) =  rsd(m,i,j,k)
     >                   - ty2 * ( flux(m,i,j+1,k) - flux(m,i,j-1,k) )
               end do
            end do

            L2 = ny + 1
            if (east.eq.-1) L2 = ny
            do j = jst, L2
               tmp = 1.0d+00 / u(1,i,j,k)

               u21j = tmp * u(2,i,j,k)
               u31j = tmp * u(3,i,j,k)
               u41j = tmp * u(4,i,j,k)
               u51j = tmp * u(5,i,j,k)

               tmp = 1.0d+00 / u(1,i,j-1,k)
               u21jm1 = tmp * u(2,i,j-1,k)
               u31jm1 = tmp * u(3,i,j-1,k)
               u41jm1 = tmp * u(4,i,j-1,k)
               u51jm1 = tmp * u(5,i,j-1,k)

               flux(2,i,j,k) = ty3 * ( u21j - u21jm1 )
               flux(3,i,j,k) = (4.0d+00/3.0d+00) * ty3 * (u31j-u31jm1)
               flux(4,i,j,k) = ty3 * ( u41j - u41jm1 )
               flux(5,i,j,k) = 0.50d+00 * ( 1.0d+00 - c1*c5 )
     >              * ty3 * ( ( u21j  **2 + u31j  **2 + u41j  **2 )
     >                      - ( u21jm1**2 + u31jm1**2 + u41jm1**2 ) )
     >              + (1.0d+00/6.0d+00)
     >              * ty3 * ( u31j**2 - u31jm1**2 )
     >              + c1 * c5 * ty3 * ( u51j - u51jm1 )
            end do

            do j = jst, jend

               rsd(1,i,j,k) = rsd(1,i,j,k)
     >              + dy1 * ty1 * (            u(1,i,j-1,k)
     >                             - 2.0d+00 * u(1,i,j,k)
     >                             +           u(1,i,j+1,k) )

               rsd(2,i,j,k) = rsd(2,i,j,k)
     >          + ty3 * c3 * c4 * ( flux(2,i,j+1,k) - flux(2,i,j,k) )
     >              + dy2 * ty1 * (            u(2,i,j-1,k)
     >                             - 2.0d+00 * u(2,i,j,k)
     >                             +           u(2,i,j+1,k) )

               rsd(3,i,j,k) = rsd(3,i,j,k)
     >          + ty3 * c3 * c4 * ( flux(3,i,j+1,k) - flux(3,i,j,k) )
     >              + dy3 * ty1 * (            u(3,i,j-1,k)
     >                             - 2.0d+00 * u(3,i,j,k)
     >                             +           u(3,i,j+1,k) )

               rsd(4,i,j,k) = rsd(4,i,j,k)
     >          + ty3 * c3 * c4 * ( flux(4,i,j+1,k) - flux(4,i,j,k) )
     >              + dy4 * ty1 * (            u(4,i,j-1,k)
     >                             - 2.0d+00 * u(4,i,j,k)
     >                             +           u(4,i,j+1,k) )

               rsd(5,i,j,k) = rsd(5,i,j,k)
     >          + ty3 * c3 * c4 * ( flux(5,i,j+1,k) - flux(5,i,j,k) )
     >              + dy5 * ty1 * (            u(5,i,j-1,k)
     >                             - 2.0d+00 * u(5,i,j,k)
     >                             +           u(5,i,j+1,k) )

            end do

c---------------------------------------------------------------------
c   fourth-order dissipation
c---------------------------------------------------------------------
            IF (west.eq.-1) then
             do m = 1, 5
               rsd(m,i,2,k) = rsd(m,i,2,k)
     >           - dssp * ( + 5.0d+00 * u(m,i,2,k)
     >                      - 4.0d+00 * u(m,i,3,k)
     >                      +           u(m,i,4,k) )
               rsd(m,i,3,k) = rsd(m,i,3,k)
     >           - dssp * ( - 4.0d+00 * u(m,i,2,k)
     >                      + 6.0d+00 * u(m,i,3,k)
     >                      - 4.0d+00 * u(m,i,4,k)
     >                      +           u(m,i,5,k) )
             end do
            END IF

            jst1 = 1
            jend1 = ny
            if (west.eq.-1) jst1 = 4
            if (east.eq.-1) jend1 = ny - 3
            do j = jst1, jend1
               do m = 1, 5
                  rsd(m,i,j,k) = rsd(m,i,j,k)
     >              - dssp * (            u(m,i,j-2,k)
     >                        - 4.0d+00 * u(m,i,j-1,k)
     >                        + 6.0d+00 * u(m,i,j,k)
     >                        - 4.0d+00 * u(m,i,j+1,k)
     >                        +           u(m,i,j+2,k) )
               end do
            end do

            IF (east.eq.-1) then
             do m = 1, 5
               rsd(m,i,ny-2,k) = rsd(m,i,ny-2,k)
     >           - dssp * (             u(m,i,ny-4,k)
     >                      - 4.0d+00 * u(m,i,ny-3,k)
     >                      + 6.0d+00 * u(m,i,ny-2,k)
     >                      - 4.0d+00 * u(m,i,ny-1,k)  )
               rsd(m,i,ny-1,k) = rsd(m,i,ny-1,k)
     >           - dssp * (             u(m,i,ny-3,k)
     >                      - 4.0d+00 * u(m,i,ny-2,k)
     >                      + 5.0d+00 * u(m,i,ny-1,k) )
             end do
            END IF

         end do
      end do

c---------------------------------------------------------------------
c   zeta-direction flux differences
c---------------------------------------------------------------------
      do j = jst, jend
         do i = ist, iend
            do k = 1, nz
               flux(1,i,j,k) = u(4,i,j,k)
               u41 = u(4,i,j,k) / u(1,i,j,k)

               q = 0.50d+00 * (  u(2,i,j,k) * u(2,i,j,k)
     >                         + u(3,i,j,k) * u(3,i,j,k)
     >                         + u(4,i,j,k) * u(4,i,j,k) )
     >                      / u(1,i,j,k)

               flux(2,i,j,k) = u(2,i,j,k) * u41 
               flux(3,i,j,k) = u(3,i,j,k) * u41 
               flux(4,i,j,k) = u(4,i,j,k) * u41 + c2 * (u(5,i,j,k)-q)
               flux(5,i,j,k) = ( c1 * u(5,i,j,k) - c2 * q ) * u41
            end do

            do k = 2, nz - 1
               do m = 1, 5
                  rsd(m,i,j,k) =  rsd(m,i,j,k)
     >                - tz2 * ( flux(m,i,j,k+1) - flux(m,i,j,k-1) )
               end do
            end do

            do k = 2, nz
               tmp = 1.0d+00 / u(1,i,j,k)

               u21k = tmp * u(2,i,j,k)
               u31k = tmp * u(3,i,j,k)
               u41k = tmp * u(4,i,j,k)
               u51k = tmp * u(5,i,j,k)

               tmp = 1.0d+00 / u(1,i,j,k-1)

               u21km1 = tmp * u(2,i,j,k-1)
               u31km1 = tmp * u(3,i,j,k-1)
               u41km1 = tmp * u(4,i,j,k-1)
               u51km1 = tmp * u(5,i,j,k-1)

               flux(2,i,j,k) = tz3 * ( u21k - u21km1 )
               flux(3,i,j,k) = tz3 * ( u31k - u31km1 )
               flux(4,i,j,k) = (4.0d+00/3.0d+00) * tz3 * (u41k-u41km1)
               flux(5,i,j,k) = 0.50d+00 * ( 1.0d+00 - c1*c5 )
     >              * tz3 * ( ( u21k  **2 + u31k  **2 + u41k  **2 )
     >                      - ( u21km1**2 + u31km1**2 + u41km1**2 ) )
     >              + (1.0d+00/6.0d+00)
     >              * tz3 * ( u41k**2 - u41km1**2 )
     >              + c1 * c5 * tz3 * ( u51k - u51km1 )
            end do

            do k = 2, nz - 1
               rsd(1,i,j,k) = rsd(1,i,j,k)
     >              + dz1 * tz1 * (            u(1,i,j,k-1)
     >                             - 2.0d+00 * u(1,i,j,k)
     >                             +           u(1,i,j,k+1) )
               rsd(2,i,j,k) = rsd(2,i,j,k)
     >          + tz3 * c3 * c4 * ( flux(2,i,j,k+1) - flux(2,i,j,k) )
     >              + dz2 * tz1 * (            u(2,i,j,k-1)
     >                             - 2.0d+00 * u(2,i,j,k)
     >                             +           u(2,i,j,k+1) )
               rsd(3,i,j,k) = rsd(3,i,j,k)
     >          + tz3 * c3 * c4 * ( flux(3,i,j,k+1) - flux(3,i,j,k) )
     >              + dz3 * tz1 * (            u(3,i,j,k-1)
     >                             - 2.0d+00 * u(3,i,j,k)
     >                             +           u(3,i,j,k+1) )
               rsd(4,i,j,k) = rsd(4,i,j,k)
     >          + tz3 * c3 * c4 * ( flux(4,i,j,k+1) - flux(4,i,j,k) )
     >              + dz4 * tz1 * (            u(4,i,j,k-1)
     >                             - 2.0d+00 * u(4,i,j,k)
     >                             +           u(4,i,j,k+1) )
               rsd(5,i,j,k) = rsd(5,i,j,k)
     >          + tz3 * c3 * c4 * ( flux(5,i,j,k+1) - flux(5,i,j,k) )
     >              + dz5 * tz1 * (            u(5,i,j,k-1)
     >                             - 2.0d+00 * u(5,i,j,k)
     >                             +           u(5,i,j,k+1) )
            end do

c---------------------------------------------------------------------
c   fourth-order dissipation
c---------------------------------------------------------------------
            do m = 1, 5
               rsd(m,i,j,2) = rsd(m,i,j,2)
     >           - dssp * ( + 5.0d+00 * u(m,i,j,2)
     >                      - 4.0d+00 * u(m,i,j,3)
     >                      +           u(m,i,j,4) )
               rsd(m,i,j,3) = rsd(m,i,j,3)
     >           - dssp * ( - 4.0d+00 * u(m,i,j,2)
     >                      + 6.0d+00 * u(m,i,j,3)
     >                      - 4.0d+00 * u(m,i,j,4)
     >                      +           u(m,i,j,5) )
            end do

            do k = 4, nz - 3
               do m = 1, 5
                  rsd(m,i,j,k) = rsd(m,i,j,k)
     >              - dssp * (            u(m,i,j,k-2)
     >                        - 4.0d+00 * u(m,i,j,k-1)
     >                        + 6.0d+00 * u(m,i,j,k)
     >                        - 4.0d+00 * u(m,i,j,k+1)
     >                        +           u(m,i,j,k+2) )
               end do
            end do

            do m = 1, 5
               rsd(m,i,j,nz-2) = rsd(m,i,j,nz-2)
     >           - dssp * (             u(m,i,j,nz-4)
     >                      - 4.0d+00 * u(m,i,j,nz-3)
     >                      + 6.0d+00 * u(m,i,j,nz-2)
     >                      - 4.0d+00 * u(m,i,j,nz-1)  )
               rsd(m,i,j,nz-1) = rsd(m,i,j,nz-1)
     >           - dssp * (             u(m,i,j,nz-3)
     >                      - 4.0d+00 * u(m,i,j,nz-2)
     >                      + 5.0d+00 * u(m,i,j,nz-1) )
            end do
         end do
      end do

      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine setbv

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   set the boundary values of dependent variables
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c   local variables
c---------------------------------------------------------------------
      integer i, j, k
      integer iglob, jglob

c---------------------------------------------------------------------
c   set the dependent variable values along the top and bottom faces
c---------------------------------------------------------------------
      do j = 1, ny
         jglob = jpt + j
         do i = 1, nx
           iglob = ipt + i
            call exact( iglob, jglob, 1, u( 1, i, j, 1 ) )
            call exact( iglob, jglob, nz, u( 1, i, j, nz ) )
         end do
      end do

c---------------------------------------------------------------------
c   set the dependent variable values along north and south faces
c---------------------------------------------------------------------
      IF (west.eq.-1) then
         do k = 1, nz
            do i = 1, nx
               iglob = ipt + i
               call exact( iglob, 1, k, u( 1, i, 1, k ) )
            end do
         end do
      END IF

      IF (east.eq.-1) then
          do k = 1, nz
             do i = 1, nx
                iglob = ipt + i
                call exact( iglob, ny0, k, u( 1, i, ny, k ) )
             end do
          end do
      END IF

c---------------------------------------------------------------------
c   set the dependent variable values along east and west faces
c---------------------------------------------------------------------
      IF (north.eq.-1) then
         do k = 1, nz
            do j = 1, ny
               jglob = jpt + j
               call exact( 1, jglob, k, u( 1, 1, j, k ) )
            end do
         end do
      END IF

      IF (south.eq.-1) then
         do k = 1, nz
            do j = 1, ny
                  jglob = jpt + j
            call exact( nx0, jglob, k, u( 1, nx, j, k ) )
            end do
         end do
      END IF

      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine setcoeff

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------


c---------------------------------------------------------------------
c   set up coefficients
c---------------------------------------------------------------------
      dxi = 1.0d+00 / ( nx0 - 1 )
      deta = 1.0d+00 / ( ny0 - 1 )
      dzeta = 1.0d+00 / ( nz0 - 1 )

      tx1 = 1.0d+00 / ( dxi * dxi )
      tx2 = 1.0d+00 / ( 2.0d+00 * dxi )
      tx3 = 1.0d+00 / dxi

      ty1 = 1.0d+00 / ( deta * deta )
      ty2 = 1.0d+00 / ( 2.0d+00 * deta )
      ty3 = 1.0d+00 / deta

      tz1 = 1.0d+00 / ( dzeta * dzeta )
      tz2 = 1.0d+00 / ( 2.0d+00 * dzeta )
      tz3 = 1.0d+00 / dzeta

      ii1 = 2
      ii2 = nx0 - 1
      ji1 = 2
      ji2 = ny0 - 2
      ki1 = 3
      ki2 = nz0 - 1

c---------------------------------------------------------------------
c   diffusion coefficients
c---------------------------------------------------------------------
      dx1 = 0.75d+00
      dx2 = dx1
      dx3 = dx1
      dx4 = dx1
      dx5 = dx1

      dy1 = 0.75d+00
      dy2 = dy1
      dy3 = dy1
      dy4 = dy1
      dy5 = dy1

      dz1 = 1.00d+00
      dz2 = dz1
      dz3 = dz1
      dz4 = dz1
      dz5 = dz1

c---------------------------------------------------------------------
c   fourth difference dissipation
c---------------------------------------------------------------------
      dssp = ( max (dx1, dy1, dz1 ) ) / 4.0d+00

c---------------------------------------------------------------------
c   coefficients of the exact solution to the first pde
c---------------------------------------------------------------------
      ce(1,1) = 2.0d+00
      ce(1,2) = 0.0d+00
      ce(1,3) = 0.0d+00
      ce(1,4) = 4.0d+00
      ce(1,5) = 5.0d+00
      ce(1,6) = 3.0d+00
      ce(1,7) = 5.0d-01
      ce(1,8) = 2.0d-02
      ce(1,9) = 1.0d-02
      ce(1,10) = 3.0d-02
      ce(1,11) = 5.0d-01
      ce(1,12) = 4.0d-01
      ce(1,13) = 3.0d-01

c---------------------------------------------------------------------
c   coefficients of the exact solution to the second pde
c---------------------------------------------------------------------
      ce(2,1) = 1.0d+00
      ce(2,2) = 0.0d+00
      ce(2,3) = 0.0d+00
      ce(2,4) = 0.0d+00
      ce(2,5) = 1.0d+00
      ce(2,6) = 2.0d+00
      ce(2,7) = 3.0d+00
      ce(2,8) = 1.0d-02
      ce(2,9) = 3.0d-02
      ce(2,10) = 2.0d-02
      ce(2,11) = 4.0d-01
      ce(2,12) = 3.0d-01
      ce(2,13) = 5.0d-01

c---------------------------------------------------------------------
c   coefficients of the exact solution to the third pde
c---------------------------------------------------------------------
      ce(3,1) = 2.0d+00
      ce(3,2) = 2.0d+00
      ce(3,3) = 0.0d+00
      ce(3,4) = 0.0d+00
      ce(3,5) = 0.0d+00
      ce(3,6) = 2.0d+00
      ce(3,7) = 3.0d+00
      ce(3,8) = 4.0d-02
      ce(3,9) = 3.0d-02
      ce(3,10) = 5.0d-02
      ce(3,11) = 3.0d-01
      ce(3,12) = 5.0d-01
      ce(3,13) = 4.0d-01

c---------------------------------------------------------------------
c   coefficients of the exact solution to the fourth pde
c---------------------------------------------------------------------
      ce(4,1) = 2.0d+00
      ce(4,2) = 2.0d+00
      ce(4,3) = 0.0d+00
      ce(4,4) = 0.0d+00
      ce(4,5) = 0.0d+00
      ce(4,6) = 2.0d+00
      ce(4,7) = 3.0d+00
      ce(4,8) = 3.0d-02
      ce(4,9) = 5.0d-02
      ce(4,10) = 4.0d-02
      ce(4,11) = 2.0d-01
      ce(4,12) = 1.0d-01
      ce(4,13) = 3.0d-01

c---------------------------------------------------------------------
c   coefficients of the exact solution to the fifth pde
c---------------------------------------------------------------------
      ce(5,1) = 5.0d+00
      ce(5,2) = 4.0d+00
      ce(5,3) = 3.0d+00
      ce(5,4) = 2.0d+00
      ce(5,5) = 1.0d-01
      ce(5,6) = 4.0d-01
      ce(5,7) = 3.0d-01
      ce(5,8) = 5.0d-02
      ce(5,9) = 4.0d-02
      ce(5,10) = 3.0d-02
      ce(5,11) = 1.0d-01
      ce(5,12) = 3.0d-01
      ce(5,13) = 2.0d-01

      return
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine sethyper

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c    for each column in a hyperplane, istart = first row,
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j
      integer iglob, jglob
      integer kp

c---------------------------------------------------------------------
c compute the pointers for hyperplanes
c---------------------------------------------------------------------
        do kp = 2,nx0+ny0
          icomms(kp) = .false.
          icommn(kp) = .false.
          icomme(kp) = .false.
          icommw(kp) = .false.

c---------------------------------------------------------------------
c  check to see if comm. to south is required
c---------------------------------------------------------------------
          if (south.ne.-1) then
            i     = iend
            iglob = ipt + i
            jglob = kp - iglob
            j     = jglob - jpt
            if (jglob.ge.2.and.jglob.le.ny0-1.and.j.ge.jst.and.
     >         j.le.jend) icomms(kp) = .true.
          end if

c---------------------------------------------------------------------
c  check to see if comm. to north is required
c---------------------------------------------------------------------
          if (north.ne.-1) then
            i     = ist
            iglob = ipt + i
            jglob = kp - iglob
            j     = jglob - jpt
            if (jglob.ge.2.and.jglob.le.ny0-1.and.j.ge.jst.and.
     >         j.le.jend) icommn(kp) = .true.
          end if

c---------------------------------------------------------------------
c  check to see if comm. to east is required
c---------------------------------------------------------------------
          if (east.ne.-1) then
            j     = jend
            jglob = jpt + j
            iglob = kp - jglob
            i     = iglob - ipt
            if (iglob.ge.2.and.iglob.le.nx0-1.and.i.ge.ist.and.
     >         i.le.iend) icomme(kp) = .true.
          end if

c---------------------------------------------------------------------
c  check to see if comm. to west is required
c---------------------------------------------------------------------
          if (west.ne.-1) then
            j = jst
            jglob = jpt + j
            iglob = kp - jglob
            i     = iglob - ipt
            if (iglob.ge.2.and.iglob.le.nx0-1.and.i.ge.ist.and.
     >         i.le.iend) icommw(kp) = .true.
          end if

        end do

        icomms(1) = .false.
        icommn(1) = .false.
        icomme(1) = .false.
        icommw(1) = .false.
        icomms(nx0+ny0+1) = .false.
        icommn(nx0+ny0+1) = .false.
        icomme(nx0+ny0+1) = .false.
        icommw(nx0+ny0+1) = .false.

      return
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------
      subroutine setiv

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c
c   set the initial values of independent variables based on tri-linear
c   interpolation of boundary values in the computational space.
c
c---------------------------------------------------------------------

      implicit none

      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k, m
      integer iglob, jglob
      double precision  xi, eta, zeta
      double precision  pxi, peta, pzeta
      double precision  ue_1jk(5),ue_nx0jk(5),ue_i1k(5),
     >        ue_iny0k(5),ue_ij1(5),ue_ijnz(5)


      do k = 2, nz - 1
         zeta = ( dble (k-1) ) / (nz-1)
         do j = 1, ny
          jglob = jpt + j
          IF (jglob.ne.1.and.jglob.ne.ny0) then
            eta = ( dble (jglob-1) ) / (ny0-1)
            do i = 1, nx
              iglob = ipt + i
              IF (iglob.ne.1.and.iglob.ne.nx0) then
               xi = ( dble (iglob-1) ) / (nx0-1)
               call exact (1,jglob,k,ue_1jk)
               call exact (nx0,jglob,k,ue_nx0jk)
               call exact (iglob,1,k,ue_i1k)
               call exact (iglob,ny0,k,ue_iny0k)
               call exact (iglob,jglob,1,ue_ij1)
               call exact (iglob,jglob,nz,ue_ijnz)
               do m = 1, 5
                  pxi =   ( 1.0d+00 - xi ) * ue_1jk(m)
     >                              + xi   * ue_nx0jk(m)
                  peta =  ( 1.0d+00 - eta ) * ue_i1k(m)
     >                              + eta   * ue_iny0k(m)
                  pzeta = ( 1.0d+00 - zeta ) * ue_ij1(m)
     >                              + zeta   * ue_ijnz(m)

                  u( m, i, j, k ) = pxi + peta + pzeta
     >                 - pxi * peta - peta * pzeta - pzeta * pxi
     >                 + pxi * peta * pzeta

               end do
              END IF
            end do
          END IF
         end do
      end do

      return
      end
c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine ssor(niter)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   to perform pseudo-time stepping SSOR iterations
c   for five nonlinear pde's.
c---------------------------------------------------------------------

      implicit none
      integer  niter

      include 'mpinpb.h'
      include 'applu.incl'
      include "stub_global.h"


c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer i, j, k, m
      integer istep
      double precision  tmp
      double precision  delunm(5), tv(5,isiz1,isiz2)

      external timer_read
      double precision wtime, timer_read

      integer IERROR

c$openad INDEPENDENT(mpi_global_var)
c$openad INDEPENDENT(omega)

c$openad DEPENDENT(rsd)
c$openad DEPENDENT(mpi_global_var)
 
      ROOT = 0
 
c---------------------------------------------------------------------
c   begin pseudo-time stepping iterations
c---------------------------------------------------------------------
      tmp = 1.0d+00 / ( omega * ( 2.0d+00 - omega ) ) 

c---------------------------------------------------------------------
c   initialize a,b,c,d to zero (guarantees that page tables have been
c   formed, if applicable on given architecture, before timestepping).
c---------------------------------------------------------------------
      do m=1,isiz2
         do k=1,isiz1
            do j=1,5
               do i=1,5
                  a(i,j,k,m) = 0.d0
                  b(i,j,k,m) = 0.d0
                  c(i,j,k,m) = 0.d0
                  d(i,j,k,m) = 0.d0
               enddo
            enddo
         enddo
      enddo

c---------------------------------------------------------------------
c   compute the steady-state residuals
c---------------------------------------------------------------------
      call rhs
 
c---------------------------------------------------------------------
c   compute the L2 norms of newton iteration residuals
c---------------------------------------------------------------------
      call l2norm( isiz1, isiz2, isiz3, nx0, ny0, nz0,
     >             ist, iend, jst, jend,
     >             rsd, rsdnm )
  
      call MPI_BARRIER( MPI_COMM_WORLD, IERROR )
 
      call timer_clear(1)
      call timer_start(1)

c---------------------------------------------------------------------
c   the timestep loop
c---------------------------------------------------------------------
      do istep = 1, niter

         if (id .eq. 0) then
            if (mod ( istep, 20) .eq. 0 .or.
     >            istep .eq. itmax .or.
     >            istep .eq. 1) then
               if (niter .gt. 1) write( *, 200) istep
 200           format(' Time step ', i4)
            endif
         endif
 
c---------------------------------------------------------------------
c   perform SSOR iteration
c---------------------------------------------------------------------
         do k = 2, nz - 1
            do j = jst, jend
               do i = ist, iend
                  do m = 1, 5
                     rsd(m,i,j,k) = dt * rsd(m,i,j,k)
                  end do
               end do
            end do
         end do
 
	 DO k = 2, nz -1 
c---------------------------------------------------------------------
c   form the lower triangular part of the jacobian matrix
c---------------------------------------------------------------------
            call jacld(k)
 
c---------------------------------------------------------------------
c   perform the lower triangular solution
c---------------------------------------------------------------------
            call blts( isiz1, isiz2, isiz3,
     >                 nx, ny, nz, k,
     >                 omega,
     >                 rsd,
     >                 a, b, c, d,
     >                 ist, iend, jst, jend, 
     >                 nx0, ny0, ipt, jpt)
	  END DO
 
	  DO k = nz - 1, 2, -1
c---------------------------------------------------------------------
c   form the strictly upper triangular part of the jacobian matrix
c---------------------------------------------------------------------
            call jacu(k)

c---------------------------------------------------------------------
c   perform the upper triangular solution
c---------------------------------------------------------------------
            call buts( isiz1, isiz2, isiz3,
     >                 nx, ny, nz, k,
     >                 omega,
     >                 rsd, tv,
     >                 d, a, b, c,
     >                 ist, iend, jst, jend,
     >                 nx0, ny0, ipt, jpt)
	  END DO
 
c---------------------------------------------------------------------
c   update the variables
c---------------------------------------------------------------------
 
         do k = 2, nz-1
            do j = jst, jend
               do i = ist, iend
                  do m = 1, 5
                     u( m, i, j, k ) = u( m, i, j, k )
     >                    + tmp * rsd( m, i, j, k )
                  end do
               end do
            end do
         end do
 
c---------------------------------------------------------------------
c   compute the max-norms of newton iteration corrections
c---------------------------------------------------------------------
         if ( mod ( istep, inorm ) .eq. 0 ) then
            call l2norm( isiz1, isiz2, isiz3, nx0, ny0, nz0,
     >                   ist, iend, jst, jend,
     >                   rsd, delunm )
c            if ( ipr .eq. 1 .and. id .eq. 0 ) then
c                write (*,1006) ( delunm(m), m = 1, 5 )
c            else if ( ipr .eq. 2 .and. id .eq. 0 ) then
c                write (*,'(i5,f15.6)') istep,delunm(5)
c            end if
         end if
 
c---------------------------------------------------------------------
c   compute the steady-state residuals
c---------------------------------------------------------------------
         call rhs
 
c---------------------------------------------------------------------
c   compute the max-norms of newton iteration residuals
c---------------------------------------------------------------------
         if ( ( mod ( istep, inorm ) .eq. 0 ) .or.
     >        ( istep .eq. itmax ) ) then
            call l2norm( isiz1, isiz2, isiz3, nx0, ny0, nz0,
     >                   ist, iend, jst, jend,
     >                   rsd, rsdnm )
c            if ( ipr .eq. 1.and.id.eq.0 ) then
c                write (*,1007) ( rsdnm(m), m = 1, 5 )
c            end if
         end if

c---------------------------------------------------------------------
c   check the newton-iteration residuals against the tolerance levels
c---------------------------------------------------------------------
         if ( ( rsdnm(1) .lt. tolrsd(1) ) .and.
     >        ( rsdnm(2) .lt. tolrsd(2) ) .and.
     >        ( rsdnm(3) .lt. tolrsd(3) ) .and.
     >        ( rsdnm(4) .lt. tolrsd(4) ) .and.
     >        ( rsdnm(5) .lt. tolrsd(5) ) ) then
c            if (ipr .eq. 1 .and. id.eq.0) then
c               write (*,1004) istep
c            end if
            return
         end if
 
      end do
 
      call timer_stop(1)
C       wtime = timer_read(1)
 

      call MPI_ALLREDUCE( wtime, 
     >                    maxtime, 
     >                    1, 
     >                    MPI_DOUBLE_PRECISION, 
     >                    MPI_MAX, 
     >                    MPI_COMM_WORLD,
     >                    IERROR )
 


      return
      
 1001 format (1x/5x,'pseudo-time SSOR iteration no.=',i4/)
 1004 format (1x/1x,'convergence was achieved after ',i4,
     >   ' pseudo-time steps' )
 1006 format (1x/1x,'RMS-norm of SSOR-iteration correction ',
     > 'for first pde  = ',1pe12.5/,
     > 1x,'RMS-norm of SSOR-iteration correction ',
     > 'for second pde = ',1pe12.5/,
     > 1x,'RMS-norm of SSOR-iteration correction ',
     > 'for third pde  = ',1pe12.5/,
     > 1x,'RMS-norm of SSOR-iteration correction ',
     > 'for fourth pde = ',1pe12.5/,
     > 1x,'RMS-norm of SSOR-iteration correction ',
     > 'for fifth pde  = ',1pe12.5)
 1007 format (1x/1x,'RMS-norm of steady-state residual for ',
     > 'first pde  = ',1pe12.5/,
     > 1x,'RMS-norm of steady-state residual for ',
     > 'second pde = ',1pe12.5/,
     > 1x,'RMS-norm of steady-state residual for ',
     > 'third pde  = ',1pe12.5/,
     > 1x,'RMS-norm of steady-state residual for ',
     > 'fourth pde = ',1pe12.5/,
     > 1x,'RMS-norm of steady-state residual for ',
     > 'fifth pde  = ',1pe12.5)
 
      end

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      subroutine subdomain

c---------------------------------------------------------------------
c---------------------------------------------------------------------

      implicit none

      include 'mpinpb.h'
      include 'applu.incl'

c---------------------------------------------------------------------
c  local variables
c---------------------------------------------------------------------
      integer mm, ierror, errorcode


c---------------------------------------------------------------------
c
c   set up the sub-domain sizes
c
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c   x dimension
c---------------------------------------------------------------------
      mm   = mod(nx0,xdim)
      if (row.le.mm) then
        nx = nx0/xdim + 1
        ipt = (row-1)*nx
      else
        nx = nx0/xdim
        ipt = (row-1)*nx + mm
      end if

c---------------------------------------------------------------------
c   y dimension
c---------------------------------------------------------------------
      mm   = mod(ny0,ydim)
      if (col.le.mm) then
        ny = ny0/ydim + 1
        jpt = (col-1)*ny
      else
        ny = ny0/ydim
        jpt = (col-1)*ny + mm
      end if

c---------------------------------------------------------------------
c   z dimension
c---------------------------------------------------------------------
      nz = nz0

c---------------------------------------------------------------------
c   check the sub-domain size
c---------------------------------------------------------------------
      if ( ( nx .lt. 4 ) .or.
     >     ( ny .lt. 4 ) .or.
     >     ( nz .lt. 4 ) ) then
         write (*,2001) nx, ny, nz
 2001    format (5x,'SUBDOMAIN SIZE IS TOO SMALL - ',
     >        /5x,'ADJUST PROBLEM SIZE OR NUMBER OF PROCESSORS',
     >        /5x,'SO THAT NX, NY AND NZ ARE GREATER THAN OR EQUAL',
     >        /5x,'TO 4 THEY ARE CURRENTLY', 3I3)
          CALL MPI_ABORT( MPI_COMM_WORLD,
     >                    ERRORCODE,
     >                    IERROR )
      end if

      if ( ( nx .gt. isiz1 ) .or.
     >     ( ny .gt. isiz2 ) .or.
     >     ( nz .gt. isiz3 ) ) then
         write (*,2002) nx, ny, nz
 2002    format (5x,'SUBDOMAIN SIZE IS TOO LARGE - ',
     >        /5x,'ADJUST PROBLEM SIZE OR NUMBER OF PROCESSORS',
     >        /5x,'SO THAT NX, NY AND NZ ARE LESS THAN OR EQUAL TO ',
     >        /5x,'ISIZ1, ISIZ2 AND ISIZ3 RESPECTIVELY.  THEY ARE',
     >        /5x,'CURRENTLY', 3I4)
          CALL MPI_ABORT( MPI_COMM_WORLD,
     >                    ERRORCODE,
     >                    IERROR )
      end if


c---------------------------------------------------------------------
c   set up the start and end in i and j extents for all processors
c---------------------------------------------------------------------
      ist = 1
      iend = nx
      if (north.eq.-1) ist = 2
      if (south.eq.-1) iend = nx - 1

      jst = 1
      jend = ny
      if (west.eq.-1) jst = 2
      if (east.eq.-1) jend = ny - 1

      return
      end



c---------------------------------------------------------------------
c---------------------------------------------------------------------

        subroutine verify(xcr, xce, xci, class, verified)

c---------------------------------------------------------------------
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c  verification routine                         
c---------------------------------------------------------------------

        implicit none
        include 'mpinpb.h'
        include 'applu.incl'

        double precision xcr(5), xce(5), xci
        double precision xcrref(5),xceref(5),xciref, 
     >                   xcrdif(5),xcedif(5),xcidif,
     >                   epsilon, dtref
        integer m
        character class
        logical verified

c---------------------------------------------------------------------
c   tolerance level
c---------------------------------------------------------------------
        epsilon = 1.0d-08

        class = 'U'
        verified = .true.

        do m = 1,5
           xcrref(m) = 1.0
           xceref(m) = 1.0
        end do
        xciref = 1.0

        if ( (nx0  .eq. 12     ) .and. 
     >       (ny0  .eq. 12     ) .and.
     >       (nz0  .eq. 12     ) .and.
     >       (itmax   .eq. 50    ))  then

           class = 'S'
           dtref = 5.0d-1
c---------------------------------------------------------------------
c   Reference values of RMS-norms of residual, for the (12X12X12) grid,
c   after 50 time steps, with  DT = 5.0d-01
c---------------------------------------------------------------------
         xcrref(1) = 1.6196343210976702d-02
         xcrref(2) = 2.1976745164821318d-03
         xcrref(3) = 1.5179927653399185d-03
         xcrref(4) = 1.5029584435994323d-03
         xcrref(5) = 3.4264073155896461d-02

c---------------------------------------------------------------------
c   Reference values of RMS-norms of solution error, for the (12X12X12) grid,
c   after 50 time steps, with  DT = 5.0d-01
c---------------------------------------------------------------------
         xceref(1) = 6.4223319957960924d-04
         xceref(2) = 8.4144342047347926d-05
         xceref(3) = 5.8588269616485186d-05
         xceref(4) = 5.8474222595157350d-05
         xceref(5) = 1.3103347914111294d-03

c---------------------------------------------------------------------
c   Reference value of surface integral, for the (12X12X12) grid,
c   after 50 time steps, with DT = 5.0d-01
c---------------------------------------------------------------------
         xciref = 7.8418928865937083d+00


        elseif ( (nx0 .eq. 33) .and. 
     >           (ny0 .eq. 33) .and.
     >           (nz0 .eq. 33) .and.
     >           (itmax . eq. 300) ) then

           class = 'W'   !SPEC95fp size
           dtref = 1.5d-3
c---------------------------------------------------------------------
c   Reference values of RMS-norms of residual, for the (33x33x33) grid,
c   after 300 time steps, with  DT = 1.5d-3
c---------------------------------------------------------------------
           xcrref(1) =   0.1236511638192d+02
           xcrref(2) =   0.1317228477799d+01
           xcrref(3) =   0.2550120713095d+01
           xcrref(4) =   0.2326187750252d+01
           xcrref(5) =   0.2826799444189d+02


c---------------------------------------------------------------------
c   Reference values of RMS-norms of solution error, for the (33X33X33) grid,
c---------------------------------------------------------------------
           xceref(1) =   0.4867877144216d+00
           xceref(2) =   0.5064652880982d-01
           xceref(3) =   0.9281818101960d-01
           xceref(4) =   0.8570126542733d-01
           xceref(5) =   0.1084277417792d+01


c---------------------------------------------------------------------
c   Reference value of surface integral, for the (33X33X33) grid,
c   after 300 time steps, with  DT = 1.5d-3
c---------------------------------------------------------------------
           xciref    =   0.1161399311023d+02

        elseif ( (nx0 .eq. 64) .and. 
     >           (ny0 .eq. 64) .and.
     >           (nz0 .eq. 64) .and.
     >           (itmax . eq. 250) ) then

           class = 'A'
           dtref = 2.0d+0
c---------------------------------------------------------------------
c   Reference values of RMS-norms of residual, for the (64X64X64) grid,
c   after 250 time steps, with  DT = 2.0d+00
c---------------------------------------------------------------------
         xcrref(1) = 7.7902107606689367d+02
         xcrref(2) = 6.3402765259692870d+01
         xcrref(3) = 1.9499249727292479d+02
         xcrref(4) = 1.7845301160418537d+02
         xcrref(5) = 1.8384760349464247d+03

c---------------------------------------------------------------------
c   Reference values of RMS-norms of solution error, for the (64X64X64) grid,
c   after 250 time steps, with  DT = 2.0d+00
c---------------------------------------------------------------------
         xceref(1) = 2.9964085685471943d+01
         xceref(2) = 2.8194576365003349d+00
         xceref(3) = 7.3473412698774742d+00
         xceref(4) = 6.7139225687777051d+00
         xceref(5) = 7.0715315688392578d+01

c---------------------------------------------------------------------
c   Reference value of surface integral, for the (64X64X64) grid,
c   after 250 time steps, with DT = 2.0d+00
c---------------------------------------------------------------------
         xciref = 2.6030925604886277d+01


        elseif ( (nx0 .eq. 102) .and. 
     >           (ny0 .eq. 102) .and.
     >           (nz0 .eq. 102) .and.
     >           (itmax . eq. 250) ) then

           class = 'B'
           dtref = 2.0d+0

c---------------------------------------------------------------------
c   Reference values of RMS-norms of residual, for the (102X102X102) grid,
c   after 250 time steps, with  DT = 2.0d+00
c---------------------------------------------------------------------
         xcrref(1) = 3.5532672969982736d+03
         xcrref(2) = 2.6214750795310692d+02
         xcrref(3) = 8.8333721850952190d+02
         xcrref(4) = 7.7812774739425265d+02
         xcrref(5) = 7.3087969592545314d+03

c---------------------------------------------------------------------
c   Reference values of RMS-norms of solution error, for the (102X102X102) 
c   grid, after 250 time steps, with  DT = 2.0d+00
c---------------------------------------------------------------------
         xceref(1) = 1.1401176380212709d+02
         xceref(2) = 8.1098963655421574d+00
         xceref(3) = 2.8480597317698308d+01
         xceref(4) = 2.5905394567832939d+01
         xceref(5) = 2.6054907504857413d+02

c---------------------------------------------------------------------
c   Reference value of surface integral, for the (102X102X102) grid,
c   after 250 time steps, with DT = 2.0d+00
c---------------------------------------------------------------------
         xciref = 4.7887162703308227d+01

        elseif ( (nx0 .eq. 162) .and. 
     >           (ny0 .eq. 162) .and.
     >           (nz0 .eq. 162) .and.
     >           (itmax . eq. 250) ) then

           class = 'C'
           dtref = 2.0d+0

c---------------------------------------------------------------------
c   Reference values of RMS-norms of residual, for the (162X162X162) grid,
c   after 250 time steps, with  DT = 2.0d+00
c---------------------------------------------------------------------
         xcrref(1) = 1.03766980323537846d+04
         xcrref(2) = 8.92212458801008552d+02
         xcrref(3) = 2.56238814582660871d+03
         xcrref(4) = 2.19194343857831427d+03
         xcrref(5) = 1.78078057261061185d+04

c---------------------------------------------------------------------
c   Reference values of RMS-norms of solution error, for the (162X162X162) 
c   grid, after 250 time steps, with  DT = 2.0d+00
c---------------------------------------------------------------------
         xceref(1) = 2.15986399716949279d+02
         xceref(2) = 1.55789559239863600d+01
         xceref(3) = 5.41318863077207766d+01
         xceref(4) = 4.82262643154045421d+01
         xceref(5) = 4.55902910043250358d+02

c---------------------------------------------------------------------
c   Reference value of surface integral, for the (162X162X162) grid,
c   after 250 time steps, with DT = 2.0d+00
c---------------------------------------------------------------------
         xciref = 6.66404553572181300d+01

        elseif ( (nx0 .eq. 408) .and. 
     >           (ny0 .eq. 408) .and.
     >           (nz0 .eq. 408) .and.
     >           (itmax . eq. 300) ) then

           class = 'D'
           dtref = 1.0d+0

c---------------------------------------------------------------------
c   Reference values of RMS-norms of residual, for the (408X408X408) grid,
c   after 300 time steps, with  DT = 1.0d+00
c---------------------------------------------------------------------
         xcrref(1) = 0.4868417937025d+05
         xcrref(2) = 0.4696371050071d+04
         xcrref(3) = 0.1218114549776d+05 
         xcrref(4) = 0.1033801493461d+05
         xcrref(5) = 0.7142398413817d+05

c---------------------------------------------------------------------
c   Reference values of RMS-norms of solution error, for the (408X408X408) 
c   grid, after 300 time steps, with  DT = 1.0d+00
c---------------------------------------------------------------------
         xceref(1) = 0.3752393004482d+03
         xceref(2) = 0.3084128893659d+02
         xceref(3) = 0.9434276905469d+02
         xceref(4) = 0.8230686681928d+02
         xceref(5) = 0.7002620636210d+03

c---------------------------------------------------------------------
c   Reference value of surface integral, for the (408X408X408) grid,
c   after 300 time steps, with DT = 1.0d+00
c---------------------------------------------------------------------
         xciref =    0.8334101392503d+02

        else
           verified = .FALSE.
        endif

c---------------------------------------------------------------------
c    verification test for residuals if gridsize is either 12X12X12 or 
c    64X64X64 or 102X102X102 or 162X162X162 or 408X408X408
c---------------------------------------------------------------------

c---------------------------------------------------------------------
c    Compute the difference of solution values and the known reference values.
c---------------------------------------------------------------------
        do m = 1, 5
           
           xcrdif(m) = dabs((xcr(m)-xcrref(m))/xcrref(m)) 
           xcedif(m) = dabs((xce(m)-xceref(m))/xceref(m))
           
        enddo
        xcidif = dabs((xci - xciref)/xciref)


c---------------------------------------------------------------------
c    Output the comparison of computed results to known cases.
c---------------------------------------------------------------------

        if (class .ne. 'U') then
           write(*, 1990) class
 1990      format(/, ' Verification being performed for class ', a)
           write (*,2000) epsilon
 2000      format(' Accuracy setting for epsilon = ', E20.13)
           if (dabs(dt-dtref) .gt. epsilon) then  
              verified = .false.
              class = 'U'
              write (*,1000) dtref
 1000         format(' DT does not match the reference value of ', 
     >                 E15.8)
           endif
        else 
           write(*, 1995)
 1995      format(' Unknown class')
        endif


        if (class .ne. 'U') then
           write (*,2001) 
        else
           write (*, 2005)
        endif

 2001   format(' Comparison of RMS-norms of residual')
 2005   format(' RMS-norms of residual')
        do m = 1, 5
           if (class .eq. 'U') then
              write(*, 2015) m, xcr(m)
           else if (xcrdif(m) .gt. epsilon) then
              verified = .false.
              write (*,2010) m,xcr(m),xcrref(m),xcrdif(m)
           else 
              write (*,2011) m,xcr(m),xcrref(m),xcrdif(m)
           endif
        enddo

        if (class .ne. 'U') then
           write (*,2002)
        else
           write (*,2006)
        endif
 2002   format(' Comparison of RMS-norms of solution error')
 2006   format(' RMS-norms of solution error')
        
        do m = 1, 5
           if (class .eq. 'U') then
              write(*, 2015) m, xce(m)
           else if (xcedif(m) .le. epsilon) then
              write (*,2011) m,xce(m),xceref(m),xcedif(m)
           else
              verified = .false.
              write (*,2010) m,xce(m),xceref(m),xcedif(m)
           endif
        enddo
        
 2010   format(' FAILURE: ', i2, 2x, E20.13, E20.13, E20.13)
 2011   format('          ', i2, 2x, E20.13, E20.13, E20.13)
 2015   format('          ', i2, 2x, E20.13)
        
        if (class .ne. 'U') then
           write (*,2025)
        else
           write (*,2026)
        endif
 2025   format(' Comparison of surface integral')
 2026   format(' Surface integral')


        if (class .eq. 'U') then
           write(*, 2030) xci
        else if (xcidif .le. epsilon) then
           write(*, 2032) xci, xciref, xcidif
        else
           verified = .false.
           write(*, 2031) xci, xciref, xcidif
        endif

 2030   format('          ', 4x, E20.13)
 2031   format(' FAILURE: ', 4x, E20.13, E20.13, E20.13)
 2032   format('          ', 4x, E20.13, E20.13, E20.13)



        if (class .eq. 'U') then
           write(*, 2022)
           write(*, 2023)
 2022      format(' No reference values provided')
 2023      format(' No verification performed')
        else if (verified) then
           write(*, 2020)
 2020      format(' Verification Successful')
        else
           write(*, 2021)
 2021      format(' Verification failed')
        endif

        return


        end
!------------------------
        ! Bring those stubs in
        include "all_stubs_mpig.f"
